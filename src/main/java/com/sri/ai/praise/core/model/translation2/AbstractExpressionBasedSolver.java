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
package com.sri.ai.praise.core.model.translation2;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeOfExpression;
import static com.sri.ai.grinder.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.library.FunctorConstants.EQUIVALENCE;

import java.util.LinkedList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.inference.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.model.classbased.expressionbased.ExpressionBasedModel;

/**
 * 
 */
public abstract class AbstractExpressionBasedSolver<V, M> implements ExpressionBasedSolver {

	private static final Expression QUERY = parse("query");
	
	private ExpressionBasedModel originalExpressionBasedModel;
	
	abstract Expression solve(V variable, M model, boolean isKnownToBeBayesianNetwork);
//	protected Expression solveConcrete(V variable, N model, boolean isKnownToBeBayesianNetwork) {
//		ExactBPFromVariable exactBP = new ExactBPFromVariable(parameters.queryVariable, parameters.model);
//		Expression result = (ExpressionFactor) exactBP.apply();
//		return result;
//	}
	
	abstract protected V makeVariableFromExpression(Expression query);

	abstract protected M makeModelFromListOfExpressions(List<Expression> listOfFactors);

	public AbstractExpressionBasedSolver(ExpressionBasedModel model) {
		this.originalExpressionBasedModel = model;
	}
	
	private class Parameters {
		V queryVariable;
		M model;
		boolean isKnownToBeBayesianNetwork;
		public Parameters(V query, M model, boolean isKnownToBeBayesianNetwork) {
			super();
			this.queryVariable = query;
			this.model = model;
			this.isKnownToBeBayesianNetwork = isKnownToBeBayesianNetwork;
		}
	}
	
	@Override
	public Expression solve(Expression queryExpression) {
		Parameters parameters = getParameters(queryExpression);
		Expression result = solve(parameters.queryVariable, parameters.model, parameters.isKnownToBeBayesianNetwork);
		return result;
	}


	private Parameters getParameters(Expression queryExpression) {
		Parameters parameters;
		if (isCompound(queryExpression)) {
			parameters = makeExactBPParametersForCompoundQuery(queryExpression);
		}
		else {
			parameters = makeExactBPParametersForVariableQuery(queryExpression);
		}
		return parameters;
	}

	private boolean isCompound(Expression queryExpression) {
		boolean queryIsOneOfTheRandomVariables = originalExpressionBasedModel.getMapFromRandomVariableNameToTypeName().containsKey(queryExpression.toString());
		boolean queryIsCompound = ! queryIsOneOfTheRandomVariables;
		return queryIsCompound;
	}

	private Parameters makeExactBPParametersForCompoundQuery(Expression queryExpression) {
		V queryVariable = makeVariableFromExpression(QUERY);
		M model = makeModelIncludingCompoundQueryDefinition(queryExpression);
		Parameters parameters = new Parameters(queryVariable, model, originalExpressionBasedModel.isKnownToBeBayesianNetwork());
		return parameters;
	}

	private Parameters makeExactBPParametersForVariableQuery(Expression queryExpression) {
		V queryVariable = makeVariableFromExpression(queryExpression);
		M model = getModelOfOriginalFactors();
		Parameters parameters = new Parameters(queryVariable, model, originalExpressionBasedModel.isKnownToBeBayesianNetwork());
		return parameters;
	}

	private M makeModelIncludingCompoundQueryDefinition(Expression queryExpression) {
		List<Expression> originalFactorsAndQueryFactor = getListOfFactorsAndQueryFactor(queryExpression);
		M result = makeModelFromListOfExpressions(originalFactorsAndQueryFactor);
		return result;
	}

	private List<Expression> getListOfFactorsAndQueryFactor(Expression queryExpression) {
		Expression queryFactor = makeQueryDefinitionFactor(queryExpression);  
		List<Expression> originalFactorsAndQueryFactor = makeListOfOriginalFactorsAndQueryDefinitionFactor(queryFactor);
		return originalFactorsAndQueryFactor;
	}

	private Expression makeQueryDefinitionFactor(Expression queryExpression) {
		boolean queryIsBoolean = queryIsBoolean(queryExpression);
		String typeAppropriateEquality = queryIsBoolean? EQUAL : EQUIVALENCE;
		Expression queryFactor = apply(typeAppropriateEquality, QUERY, queryExpression);
		return queryFactor;
	}

	private boolean queryIsBoolean(Expression queryExpression) {
		Type typeOfQueryExpression = getTypeOfExpression(queryExpression, getContext());
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

	private M modelOfOriginalFactors;
	
	private M getModelOfOriginalFactors() {
		if (modelOfOriginalFactors == null) {
			List<Expression> factorExpressions = originalExpressionBasedModel.getFactors();
			modelOfOriginalFactors = makeModelFromListOfExpressions(factorExpressions);
		}
		return modelOfOriginalFactors;
	}

	private boolean interrupted = false;
	
	@Override
	public void interrupt() {
		interrupted = true;
	}
	
	public boolean isInterrupted() {
		return interrupted;
	}

	@Override
	public Context getContext() {
		return originalExpressionBasedModel.getContext();
	}
}
