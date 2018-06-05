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
package com.sri.ai.praise.core.inference.core.expressionbased.query;

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
import com.sri.ai.praise.core.model.classbased.expressionbased.api.ExpressionBasedQuery;
import com.sri.ai.praise.core.model.classbased.expressionbased.core.ExpressionBasedQueryFromModel;

/**
 * A probabilistic solver for an {@link ExpressionBasedQueryFromModel} using multi-quantifier eliminators.
 * 
 * @author braz
 */
public class DefaultExpressionBasedQuerySolver extends AbstractExpressionBasedQuerySolver {

	private Expression partitionFunction;
	private AssociativeCommutativeSemiRing semiRing;
	private MultiQuantifierEliminator multiQuantifierEliminator;

	/**
	 * Constructs a quantifier elimination-based variable elimination solver for a factor graph.
	 * @param model a {@link AddBooleanQueryToContext} to be solved.
	 */
	public DefaultExpressionBasedQuerySolver() {
		this(true);
	}

	/**
	 * Constructs a quantifier elimination-based solver for a factor graph.
	 * @param model a {@link AddBooleanQueryToContext} to be solved.
	 * @param useFactorization indicates whether to use factorization (that is, factor factors out as in variable elimination)
	 */
	public DefaultExpressionBasedQuerySolver(boolean useFactorization) {
		
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
	public Expression computeNormalizedMarginal(ExpressionBasedQuery query) {
		Context context = query.getContext();
		Expression productOfPotentials = Times.make(query.getFactorExpressionsIncludingQueryDefinitionIfAny());
		Expression queryVariable = query.getQuerySymbol();
		List<Expression> queryVariables = list(queryVariable);
		boolean queryIsCompound = query.getQueryIsCompound();
		List<Expression> randomVariables = query.getRandomVariablesExcludingQuerySymbol();
		List<Expression> variablesToBeEliminated = queryIsCompound? randomVariables : setDifference(randomVariables, queryVariables);
		
		Expression unnormalizedMarginal = marginalize(variablesToBeEliminated, productOfPotentials, context);
		Expression normalizedMarginal = getNormalizedMarginal(unnormalizedMarginal, query);
		return normalizedMarginal;
	}

	private Expression getNormalizedMarginal(Expression unnormalizedMarginal, ExpressionBasedQuery query) {
		Expression normalizedMarginal;
		if (query.isKnownToBeBayesianNetwork()) {
			normalizedMarginal = unnormalizedMarginal;
		}
		else {
			normalizedMarginal = normalize(unnormalizedMarginal, query);
		}
		return normalizedMarginal;
	}

	private Expression normalize(Expression unnormalizedMarginal, ExpressionBasedQuery query) {
		computePartitionFunction(unnormalizedMarginal, query);
		Expression normalizedMarginal = divideByPartitionFunction(unnormalizedMarginal, query.getContext());
		return normalizedMarginal;
	}

	private void computePartitionFunction(Expression unnormalizedMarginal, ExpressionBasedQuery query) {
		if (partitionFunction == null) {
			partitionFunction = marginalize(list(query.getQuerySymbol()), unnormalizedMarginal, query.getContext());
		}
	}

	private Expression divideByPartitionFunction(Expression unnormalizedMarginal, Context context) {
		Expression normalizedMarginalDefinition = Division.make(unnormalizedMarginal, partitionFunction);
		Expression normalizedMarginal = context.evaluate(normalizedMarginalDefinition);
		return normalizedMarginal;
	}

	private Expression marginalize(List<Expression> variablesToBeEliminated, Expression expression, Context context) {
		Expression result = multiQuantifierEliminator.extendContextAndSolve(semiRing, variablesToBeEliminated, expression, context);
		return result;
	}
}
