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
package com.sri.ai.praise.core.representation.translation.fromexpressionstosamplingfactors;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable.expressionVariable;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.unionArrayList;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.praise.PRAiSEConfiguration;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SupplierBasedUniformSamplingFactor;
import com.sri.ai.util.math.Rational;

public class ExpressionBasedProblemToSamplingFactorInterfaceBasedProblemConversion {
	
	private Random random;

	public ExpressionBasedProblemToSamplingFactorInterfaceBasedProblemConversion(Random random) {
		this.random = random;
	}

	public Problem translate(ExpressionBasedProblem expressionBasedProblem) {
		Expression querySymbol = expressionBasedProblem.getQuerySymbol();
		ExpressionVariable queryVariable = DefaultExpressionVariable.expressionVariable(querySymbol);
		FactorNetwork factorNetwork = makeFactorNetwork(expressionBasedProblem);
		Predicate<Variable> isParameterPredicate = makeIsParameterPredicate(expressionBasedProblem);
		Problem problem = new DefaultProblem(queryVariable, factorNetwork, isParameterPredicate);
		return problem;
	}

	private FactorNetwork makeFactorNetwork(ExpressionBasedProblem expressionBasedProblem) {
		Predicate<Expression> isVariablePredicate = makeIsVariablePredicate(expressionBasedProblem);
		List<Factor> factors = makeFactors(expressionBasedProblem, isVariablePredicate, expressionBasedProblem.getContext());
		FactorNetwork factorNetwork = new DefaultFactorNetwork(factors);
		
		explanationBlock("Sampling factors generated from expressions: ", code( () -> {
			for (Factor factor : factors) {
				explanationBlock("Sampling factor: ", factor, code( () -> {
					for (Variable variable : factor.getVariables()) {
						explain(variable);
					}
				}));
			}
		}));
		
		return factorNetwork;
	}

	private Predicate<Expression> makeIsVariablePredicate(ExpressionBasedProblem expressionBasedProblem) {
		IsVariable isVariable = new IsVariable(expressionBasedProblem.getContext());
		Predicate<Expression> isVariablePredicate = e -> isVariable.apply(e);
		return isVariablePredicate;
	}

	private List<Factor> makeFactors(ExpressionBasedProblem expressionBasedProblem, Predicate<Expression> isVariablePredicate, Context context) {
		List<Factor> factors = list();
		factors.addAll(factorsFromFactorExpressions(expressionBasedProblem));
		factors.addAll(uniformFactorsForVariablesIfRequested(expressionBasedProblem));
		return factors;
	}

	private ArrayList<Factor> factorsFromFactorExpressions(ExpressionBasedProblem expressionBasedProblem) {

		boolean combineSamplingFactorsFromEachFactorExpressionIntoASingleNormalizedFactor = false;
		// This is false for now until we fix marginalized sampling factors to generate contingent rather than "lucky" goals.

		ArrayList<Factor> result;

		if (combineSamplingFactorsFromEachFactorExpressionIntoASingleNormalizedFactor) {
			FromExpressionToSamplingFactors factorTranslator = 
					new FromExpressionToSamplingFactors(random, expressionBasedProblem.getContext());

			List<Expression> factorExpressions = 
					expressionBasedProblem.getFactorExpressionsIncludingQueryDefinitionIfAny();

			result = mapIntoArrayList(factorExpressions, factorTranslator::getSingleSamplingFactor);
		}
		else {
			result = unionArrayList(arrayListsOfSamplingFactorsForEachFactorExpression(expressionBasedProblem));
		}

		return result;
	}

	private
	Iterator<ArrayList<? extends SamplingFactor>>
	arrayListsOfSamplingFactorsForEachFactorExpression(ExpressionBasedProblem expressionBasedProblem) {
		
		FromExpressionToSamplingFactors factorTranslator = 
				new FromExpressionToSamplingFactors(random, expressionBasedProblem.getContext());
		
		return functionIterator(
				expressionBasedProblem.getFactorExpressionsIncludingQueryDefinitionIfAny(), 
				factorTranslator::factorCompilation);
	}

	private Collection<? extends Factor> uniformFactorsForVariablesIfRequested(ExpressionBasedProblem expressionBasedProblem) {

		Collection<? extends Factor> result;
		
		if (PRAiSEConfiguration.useUniformSamplingBackup()) {
			result = uniformFactorsForVariables(expressionBasedProblem);
		}
		else {
			result = list();
		}
		
		return result;
	}

	private List<SamplingFactor> uniformFactorsForVariables(ExpressionBasedProblem expressionBasedProblem) {
		List<SamplingFactor> result = 
				getSymbolsExcludingQueryOne(expressionBasedProblem).stream()
				.filter(v -> hasUniformFactor(v, expressionBasedProblem.getContext()))
				.map(v -> makeUniformFactor(v, expressionBasedProblem.getContext()))
				.collect(Collectors.toList());
		return result;
	}

	private List<Expression> getSymbolsExcludingQueryOne(ExpressionBasedProblem expressionBasedProblem) {
		List<Expression> result = list(); 
		result.addAll(expressionBasedProblem.getRandomVariablesExcludingQuerySymbol());
		result.addAll(expressionBasedProblem.getParameters());
		return result;
	}

	private boolean hasUniformFactor(Expression variable, Context context) {
		Type type = context.getTypeOfRegisteredSymbol(variable);
		boolean result = type.isSampleUniquelyNamedConstantSupported();
		println(variable + " has uniform factor: " + result);
		return result;
	}
	
	private SamplingFactor makeUniformFactor(Expression variable, Context context) {
		ExpressionVariable expressionVariable = expressionVariable(variable);
		Type type = context.getTypeOfRegisteredSymbol(variable);
		myAssert(type.isSampleUniquelyNamedConstantSupported(), this, () -> "requires variable " + variable + "'s type to support uniform sampling of its values, but its type is " + type + " which does not");
		double estimatedSuccessProbability = getEstimatedSuccessWeight(type);
		Supplier<Object> uniformSampler = () -> fromExpressionValueToSampleValue(type.sampleUniquelyNamedConstant(random).getValue());
		SamplingFactor result = new SupplierBasedUniformSamplingFactor(expressionVariable, uniformSampler, estimatedSuccessProbability, random);
		return result;
	}
	
	private static Object fromExpressionValueToSampleValue(Object expressionValue) {
		if (expressionValue instanceof Rational) {
			return ((Rational) expressionValue).doubleValue();
		}
		else {
			return expressionValue;
		}
	}

	private double getEstimatedSuccessWeight(Type type) {
		Expression cardinality = type.cardinality();
		if (cardinality.equals(Expressions.UNKNOWN) || cardinality.equals(Expressions.INFINITY)) {
			return 0.00001; // this is a last resort sampling rule
		}
		else {
			return 1.0 / cardinality.doubleValue();
		}
	}

	private Predicate<Variable> makeIsParameterPredicate(ExpressionBasedProblem expressionBasedProblem) {
		Predicate<Expression> isExpressionParameterPredicate = expressionBasedProblem.getIsParameterPredicate();
		Predicate<Variable> isParameterPredicate = makeIsParameterPredicate(isExpressionParameterPredicate);
		return isParameterPredicate;
	}
	
	private Predicate<Variable> makeIsParameterPredicate(Predicate<Expression> isExpressionParameterPredicate) {
		return ev -> isExpressionParameterPredicate.test((ExpressionVariable) ev);
	}
}