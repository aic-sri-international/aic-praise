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
package com.sri.ai.praise.core.representation.translation.rodrigoframework.fromexpressionstosamplingfactors;

import static com.sri.ai.util.Util.unionArrayList;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.List;
import java.util.Random;
import java.util.function.Predicate;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;

public class ExpressionBasedProblemToSamplingFactorInterfaceBasedProblemConversion {

	public static Problem translate(ExpressionBasedProblem expressionBasedProblem) {
		ExpressionVariable queryVariable = new DefaultExpressionVariable(expressionBasedProblem.getQuerySymbol());
		FactorNetwork factorNetwork = makeFactorNetwork(expressionBasedProblem);
		Predicate<Variable> isParameterPredicate = makeIsParameterPredicate(expressionBasedProblem);
		Problem problem = new DefaultProblem(queryVariable, factorNetwork, isParameterPredicate);
		return problem;
	}

	private static FactorNetwork makeFactorNetwork(ExpressionBasedProblem expressionBasedProblem) {
		Predicate<Expression> isVariablePredicate = makeIsVariablePredicate(expressionBasedProblem);
		List<Factor> factors = makeFactors(expressionBasedProblem, isVariablePredicate);
		return new DefaultFactorNetwork(factors);
	}

	private static Predicate<Expression> makeIsVariablePredicate(ExpressionBasedProblem expressionBasedProblem) {
		IsVariable isVariable = new IsVariable(expressionBasedProblem.getContext());
		Predicate<Expression> isVariablePredicate = e -> isVariable.apply(e);
		return isVariablePredicate;
	}

	private static List<Factor> makeFactors(ExpressionBasedProblem expressionBasedProblem, Predicate<Expression> isVariablePredicate) {
		FromExpressionToSamplingFactors factorTranslator = new FromExpressionToSamplingFactors(isVariablePredicate, new Random());
		List<Factor> factors = 
				unionArrayList(
						functionIterator(
								expressionBasedProblem.getFactorExpressionsIncludingQueryDefinitionIfAny(), 
								e -> factorTranslator.factorCompilation(e)));
		return factors;
	}

	private static Predicate<Variable> makeIsParameterPredicate(ExpressionBasedProblem expressionBasedProblem) {
		Predicate<Expression> isExpressionParameterPredicate = expressionBasedProblem.getIsParameterPredicate();
		Predicate<Variable> isParameterPredicate = makeIsParameterPredicate(isExpressionParameterPredicate);
		return isParameterPredicate;
	}
	
	private static Predicate<Variable> makeIsParameterPredicate(Predicate<Expression> isExpressionParameterPredicate) {
		return ev -> isExpressionParameterPredicate.test((ExpressionVariable) ev);
	}
}