/*
 * Copyright (c) 2015, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-praise nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.core.representation.classbased.expressionbased.core;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeExpressionOfExpression;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeOfExpression;
import static com.sri.ai.grinder.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.library.FunctorConstants.EQUIVALENCE;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedQuery;

/**
 * Given a {@link ExpressionBasedModel} and a (possibly compound) query expression for it,
 * prepares information for solving the query, including the factors that need to be used, context, and which query symbol to use.
 * <p>
 * This is done by, in the case of compound queries, introducing a new variable, "query", and a factor
 * making it equal to the original compound query.
 * <p>
 * Also provides a method for replacing the compound query back into a given expression containing
 * the variable query.
 */
public class DefaultExpressionBasedQuery implements ExpressionBasedQuery {

	private static final Expression QUERY_SYMBOL = parse("query");
	
	/** The original {@link ExpressionBasedModel}. */
	private ExpressionBasedModel originalExpressionBasedModel;
	
	/** The variable query to be used ("query" if original query was compound). */
	private Expression querySymbol;
	
	/** The list of factors to be used (including the variable query definition if original query was compound). */
	private List<Expression> factorExpressionsIncludingQueryDefinitionIfAny;
	
	private List<Expression> originalRandomVariables;

	private Predicate<Expression> isParameterPredicate;
	
	/** The original query. */
	private Expression queryExpression;
	
	/** Whether the list of factors represents a Bayesian network. */
	private boolean isKnownToBeBayesianNetwork;
	
	/** Whether the original query was compound. */
	private boolean queryIsCompound;
	
	/** The context to be used, possibly including the symbol "query" and its type. */
	private Context context;
	
	public DefaultExpressionBasedQuery(ExpressionBasedModel model, Expression queryExpression) {
		this.originalExpressionBasedModel = model;
		this.queryExpression = queryExpression;
		this.originalRandomVariables = originalExpressionBasedModel.getRandomVariables();
		this.isParameterPredicate = e -> model.getMapFromNonUniquelyNamedConstantNameToTypeName().containsKey(e.toString());
		this.isKnownToBeBayesianNetwork = originalExpressionBasedModel.isKnownToBeBayesianNetwork();
		
		if (decideIfQueryIsCompound()) {
			processCompoundQuery();
		}
		else {
			processVariableQuery();
		}
	}

	/** 
	 * Replaces variable query ("query") by original query in given expression, 
	 * and simplifies it with original model's context.
	 */
	@Override
	public Expression replaceQuerySymbolByQueryExpressionIfNeeded(Expression expression) {
		Expression result;
		if (queryIsCompound) {
			Expression replacement = 
					expression.replaceAllOccurrences(QUERY_SYMBOL, queryExpression, originalExpressionBasedModel.getContext());
			result = originalExpressionBasedModel.getContext().evaluate(replacement);
		}
		else {
			result = expression;
		}
		return result;
	}
	
	private boolean decideIfQueryIsCompound() {
		Map<String, String> mapFromRandomVariableNameToTypeName = originalExpressionBasedModel.getMapFromRandomVariableNameToTypeName();
		boolean queryIsOneOfTheRandomVariables = mapFromRandomVariableNameToTypeName.containsKey(queryExpression.toString());
		queryIsCompound = ! queryIsOneOfTheRandomVariables;
		return queryIsCompound;
	}

	private void processVariableQuery() {
		querySymbol = queryExpression;
		factorExpressionsIncludingQueryDefinitionIfAny = originalExpressionBasedModel.getFactors();
		context = originalExpressionBasedModel.getContext();
	}

	private void processCompoundQuery() {
		querySymbol = QUERY_SYMBOL;
		factorExpressionsIncludingQueryDefinitionIfAny = getListOfFactorsAndQueryFactor();
		context = extendContextWithQuerySymbol(queryExpression);
	}

	private List<Expression> getListOfFactorsAndQueryFactor() {
		Expression queryFactor = makeQueryDefinitionFactor(queryExpression);  
		List<Expression> originalFactorsAndQueryFactor = makeListOfOriginalFactorsAndQueryDefinitionFactor(queryFactor);
		return originalFactorsAndQueryFactor;
	}

	private Expression makeQueryDefinitionFactor(Expression queryExpression) {
		boolean queryIsBoolean = queryIsBoolean(queryExpression);
		String typeAppropriateEquality = queryIsBoolean? EQUIVALENCE : EQUAL;
		Expression queryFactorCondition = apply(typeAppropriateEquality, QUERY_SYMBOL, queryExpression);
		Expression queryFactor = IfThenElse.make(queryFactorCondition, ONE, ZERO);
		return queryFactor;
	}

	private boolean queryIsBoolean(Expression queryExpression) {
		Type typeOfQueryExpression = getTypeOfExpression(queryExpression, originalExpressionBasedModel.getContext());
		boolean queryIsBoolean = typeOfQueryExpression.toString().equals("Boolean");
		return queryIsBoolean;
	}

	private List<Expression> makeListOfOriginalFactorsAndQueryDefinitionFactor(Expression queryFactor) {
		List<Expression> originalFactorsAndQueryFactor = getCopyOfOriginalFactors();
		originalFactorsAndQueryFactor.add(queryFactor);
		return originalFactorsAndQueryFactor;
	}

	private List<Expression> getCopyOfOriginalFactors() {
		LinkedList<Expression> result = new LinkedList<>(originalExpressionBasedModel.getFactors());
		return result;
	}

	private Context extendContextWithQuerySymbol(Expression queryExpression) {
		Context originalContext = originalExpressionBasedModel.getContext();
		Expression queryType = getTypeExpressionOfExpression(queryExpression, originalContext);
		String queryTypeName = queryType.toString();
		Context result = originalContext.extendWithSymbolsAndTypes("query", queryTypeName);
		return result;
	}

	public ExpressionBasedModel getOriginalExpressionBasedModel() {
		return originalExpressionBasedModel;
	}

	@Override
	public Expression getQuerySymbol() {
		return querySymbol;
	}

	@Override
	public List<Expression> getFactorExpressionsIncludingQueryDefinitionIfAny() {
		return Collections.unmodifiableList(factorExpressionsIncludingQueryDefinitionIfAny);
	}

	@Override
	public List<Expression> getRandomVariablesExcludingQuerySymbol() {
		return Collections.unmodifiableList(originalRandomVariables);
	}

	@Override
	public Predicate<Expression> getIsParameterPredicate() {
		return isParameterPredicate;
	}

	@Override
	public Expression getQueryExpression() {
		return queryExpression;
	}

	@Override
	public boolean isKnownToBeBayesianNetwork() {
		return isKnownToBeBayesianNetwork;
	}

	@Override
	public boolean getQueryIsCompound() {
		return queryIsCompound;
	}

	@Override
	public Context getContext() {
		return context;
	}
}
