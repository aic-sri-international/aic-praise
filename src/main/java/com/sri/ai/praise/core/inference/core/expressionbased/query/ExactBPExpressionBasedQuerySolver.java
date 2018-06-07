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

import static com.sri.ai.praise.core.PRAiSEUtil.normalize;

import java.util.function.Predicate;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.core.treebased.exactbp.core.ExactBP;
import com.sri.ai.praise.core.model.api.Variable;
import com.sri.ai.praise.core.model.classbased.expressionbased.api.ExpressionBasedQuery;
import com.sri.ai.praise.core.model.classbased.expressionbased.core.ExpressionBasedQueryFromModel;
import com.sri.ai.praise.core.model.encapsulatedoperations.expression.ExpressionFactor;
import com.sri.ai.praise.core.model.encapsulatedoperations.expression.ExpressionFactorNetwork;
import com.sri.ai.praise.core.model.encapsulatedoperations.expression.ExpressionVariable;

/**
 * A probabilistic solver for an {@link ExpressionBasedQueryFromModel}
 * that applies multi-quantifier elimination to marginalizing summations.
 * 
 * @author braz
 *
 */
public class ExactBPExpressionBasedQuerySolver extends AbstractExpressionBasedQuerySolver {

	@Override
	protected Expression computeNormalizedMarginal(ExpressionBasedQuery query) {
		ExpressionFactorNetwork factorNetwork = new ExpressionFactorNetwork(query.getFactorExpressionsIncludingQueryDefinitionIfAny(), query.getContext());
		ExpressionVariable queryVariable = new ExpressionVariable(query.getQuerySymbol());
		Predicate<Expression> isDefinedAsFreeByTheClientCodePredicate = query.getIsDefinedAsFreeByTheClientCodePredicate();
		ExactBP exactBP = new ExactBP(queryVariable, factorNetwork, makeIsDefinedAsFreeByTheClientCodePredicate(isDefinedAsFreeByTheClientCodePredicate));
		Expression unnormalized = (ExpressionFactor) exactBP.apply();
		Expression normalizedMarginal = normalize(queryVariable, unnormalized, factorNetwork.getContext());
		return normalizedMarginal;
	}

	private Predicate<Variable> makeIsDefinedAsFreeByTheClientCodePredicate(
			Predicate<Expression> isDefinedAsFreeByTheClientCodePredicate) {
		
		return ev -> isDefinedAsFreeByTheClientCodePredicate.test((ExpressionVariable) ev);
	}
}