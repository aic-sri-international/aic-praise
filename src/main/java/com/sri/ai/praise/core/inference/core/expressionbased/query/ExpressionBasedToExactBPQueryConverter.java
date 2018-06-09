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

import java.util.function.Predicate;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.core.treebased.exactbp.api.ExactBPQuery;
import com.sri.ai.praise.core.inference.core.treebased.exactbp.core.DefaultExactBPQuery;
import com.sri.ai.praise.core.model.api.Variable;
import com.sri.ai.praise.core.model.classbased.expressionbased.api.ExpressionBasedQuery;
import com.sri.ai.praise.core.model.encapsulatedoperations.expression.ExpressionFactorNetwork;
import com.sri.ai.praise.core.model.encapsulatedoperations.expression.ExpressionVariable;

public class ExpressionBasedToExactBPQueryConverter {

	public static ExactBPQuery convert(ExpressionBasedQuery query) {
		ExpressionVariable queryVariable = new ExpressionVariable(query.getQuerySymbol());
		ExpressionFactorNetwork factorNetwork = new ExpressionFactorNetwork(query.getFactorExpressionsIncludingQueryDefinitionIfAny(), query.getContext());
		Predicate<Expression> isExpressionParameterPredicate = query.getIsParameterPredicate();
		Predicate<Variable> isParameterPredicate = makeIsParameterPredicate(isExpressionParameterPredicate);
		ExactBPQuery exactBPQuery = new DefaultExactBPQuery(queryVariable, factorNetwork, isParameterPredicate);
		return exactBPQuery;
	}

	private static Predicate<Variable> makeIsParameterPredicate(Predicate<Expression> isExpressionParameterPredicate) {
		return ev -> isExpressionParameterPredicate.test((ExpressionVariable) ev);
	}
}