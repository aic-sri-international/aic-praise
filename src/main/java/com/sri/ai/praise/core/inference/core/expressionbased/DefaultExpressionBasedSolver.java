/*
 * Copyright (c) 2013, SRI International
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
 * Neither the name of the aic-expresso nor the names of its
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
package com.sri.ai.praise.core.inference.core.expressionbased;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.inference.core.helper.AddBooleanQueryToContext.addBooleanQuery;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.setDifference;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminator;
import com.sri.ai.grinder.core.solver.DefaultMultiQuantifierEliminator;
import com.sri.ai.grinder.core.solver.SGVET;
import com.sri.ai.grinder.group.AssociativeCommutativeSemiRing;
import com.sri.ai.grinder.group.SumProduct;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.praise.core.inference.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.core.helper.AddBooleanQueryToContext;
import com.sri.ai.praise.core.model.classbased.expressionbased.ExpressionBasedModel;

/**
 * A probabilistic solver for an {@link AddBooleanQueryToContext}
 * that applies multi-quantifier elimination to marginalizing summations.
 * 
 * @author braz
 *
 */
public class DefaultExpressionBasedSolver implements ExpressionBasedSolver {

	private ExpressionBasedModel model;
	private Expression partitionFunction;
	private AssociativeCommutativeSemiRing semiRing;
	private MultiQuantifierEliminator multiQuantifierEliminator;

	/**
	 * Constructs a quantifier elimination-based variable elimination solver for a factor graph.
	 * @param model a {@link AddBooleanQueryToContext} to be solved.
	 */
	public DefaultExpressionBasedSolver(ExpressionBasedModel model) {
		this(model, true);
	}

	/**
	 * Constructs a quantifier elimination-based solver for a factor graph.
	 * @param model a {@link AddBooleanQueryToContext} to be solved.
	 * @param useFactorization indicates whether to use factorization (that is, factor factors out as in variable elimination)
	 */
	public DefaultExpressionBasedSolver(ExpressionBasedModel model, boolean useFactorization) {
	
		this.model = model;
		
		if (useFactorization) {
			multiQuantifierEliminator = new SGVET();
		}
		else {
			multiQuantifierEliminator = new DefaultMultiQuantifierEliminator();
		}
	
		partitionFunction = null;
		semiRing = new SumProduct(); // for marginalization
	}

	@Override
	public void interrupt() {
		multiQuantifierEliminator.interrupt();
	}
	
	@Override
	public Context getContext() {
		return model.getContext();
	}
	
	private Context contextWithBooleanQuery = null;
	private Context contextUsedToComputeContextWithQuery = null;
	
	private Context getContextWithQuery() {
		Context currentContext = getContext();
		if (contextWithBooleanQuery == null || currentContext != contextUsedToComputeContextWithQuery) {
			contextUsedToComputeContextWithQuery = currentContext;
			contextWithBooleanQuery = addBooleanQuery(contextUsedToComputeContextWithQuery);
		}
		return contextWithBooleanQuery;
	}

	private static class InferenceData {
		Expression originalQuery;
		Expression productOfPotentials;
		Expression queryVariable;
		List<Expression> queryVariables;
		List<Expression> variablesToBeEliminated; 
		boolean queryIsCompoundExpression;
		Context context;
	}
	
	private InferenceData setUpInferenceData(Expression queryExpression) {
		InferenceData data;
		if (model.getRandomVariables().contains(queryExpression)) {
			data = setUpInferenceDataForSimpleQuery(queryExpression);
		}
		else {
			data = setUpInferenceDataForCompoundQuery(queryExpression);
		}
		return data;
	}

	private InferenceData setUpInferenceDataForSimpleQuery(Expression queryExpression) {
		InferenceData data = new InferenceData();
		data.originalQuery = queryExpression;
		data.productOfPotentials = Times.make(model.getFactors());
		data.queryIsCompoundExpression = false;
		data.queryVariable = queryExpression;
		data.queryVariables = list(data.queryVariable);
		data.variablesToBeEliminated = setDifference(model.getRandomVariables(), data.queryVariables);
		data.context = getContext();
		return data;
	}

	private InferenceData setUpInferenceDataForCompoundQuery(Expression queryExpression) {
		// Add a query variable equivalent to query expression; this introduces no cycles and the model remains a Bayesian network
		InferenceData data = new InferenceData();
		data.originalQuery = queryExpression;
		data.queryIsCompoundExpression = true;
		data.queryVariable = makeSymbol("query");
		data.queryVariables = list(data.queryVariable);
		data.productOfPotentials = Times.make(list(Times.make(model.getFactors()), parse("if query <=> " + queryExpression + " then 1 else 0")));
		data.variablesToBeEliminated = model.getRandomVariables(); 
		data.context = getContextWithQuery();
		return data;
	}

	@Override
	public Expression solve(Expression queryExpression) {
		InferenceData data = setUpInferenceData(queryExpression);
		Expression unnormalizedMarginal = marginalize(data.variablesToBeEliminated, data.productOfPotentials, data);
		Expression normalizedMarginal = getNormalizedMarginal(unnormalizedMarginal, data);
		Expression result = makeResultInTermsOfOriginalQuery(normalizedMarginal, data);
		return result;
	}

	private Expression getNormalizedMarginal(Expression unnormalizedMarginal, InferenceData data) {
		Expression normalizedMarginal;
		if (model.isKnownToBeBayesianNetwork()) {
			normalizedMarginal = unnormalizedMarginal;
		}
		else {
			normalizedMarginal = normalize(unnormalizedMarginal, data);
		}
		return normalizedMarginal;
	}

	private Expression normalize(Expression unnormalizedMarginal, InferenceData data) {
		computePartitionFunction(unnormalizedMarginal, data);
		Expression normalizedMarginal = divideByPartitionFunction(unnormalizedMarginal, data);
		return normalizedMarginal;
	}

	private void computePartitionFunction(Expression unnormalizedMarginal, InferenceData data) {
		if (partitionFunction == null) {
			partitionFunction = marginalize(data.queryVariables, unnormalizedMarginal, data);
		}
	}

	private Expression divideByPartitionFunction(Expression unnormalizedMarginal, InferenceData data) {
		Expression normalizedMarginalDefinition = Division.make(unnormalizedMarginal, partitionFunction);
		Expression normalizedMarginal = data.context.evaluate(normalizedMarginalDefinition);
		return normalizedMarginal;
	}

	private Expression makeResultInTermsOfOriginalQuery(Expression marginal, InferenceData data) {
		if (data.queryIsCompoundExpression) {
			marginal = marginal.replaceAllOccurrences(data.queryVariable, data.originalQuery, data.context);
		}
		return marginal;
	}

	private Expression marginalize(List<Expression> variablesToBeEliminated, Expression expression, InferenceData data) {
		return multiQuantifierEliminator.extendContextAndSolve(semiRing, variablesToBeEliminated, expression, data.context);
	}
}
