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
package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.praise.core.PRAiSEUtil.normalize;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionSamplingFactor.expressionSamplingFactor;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.fold;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.Solver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ConstantFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionDiscretization;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.DynamicSamplingProductFactor;

/**
 * A {@link Solver} adapter based on {@link DynamicSamplingProductFactor} for {@link Problem}s using {@link SamplingFactor}s defined on {@link ExpressionVariable}s.
 * 
 * @author braz
 *
 */
public class SolverAdapterDynamicSampling implements Solver {

	private Function<Expression, Integer> fromExpressionVariableToNumberOfDiscreteValues;
	private int initialNumberOfSamples;
	private Context context;
	
	public SolverAdapterDynamicSampling(
			Function<Expression, Integer> fromExpressionVariableToNumberOfDiscreteValues,
			int initialNumberOfSamples,
			Context context) {
		
		this.fromExpressionVariableToNumberOfDiscreteValues = fromExpressionVariableToNumberOfDiscreteValues;
		this.initialNumberOfSamples = initialNumberOfSamples;
		this.context = context;
	}
	
	@Override
	public Expression solve(Problem problem) {

		Random random = new Random();
		
		Factor unnormalizedMarginal = useDynamicGlobalProduct(problem, random);
//		Factor unnormalizedMarginal = useGlobalProduct(problem);
//		Factor unnormalizedMarginal = useExactBP(problem);
		
		SamplingFactor unnormalizedMarginalAsSamplingFactor = ensureItsSamplingFactor(unnormalizedMarginal, (ExpressionVariable) problem.getQueryVariable(), random);
		
		Expression normalizedMarginalExpression = getNormalizedMarginalExpression(unnormalizedMarginalAsSamplingFactor, problem);
		return normalizedMarginalExpression;
	}

	@SuppressWarnings("unused")
	private Factor useExactBP(Problem problem) {
		Factor unnormalizedMarginal = new ExactBP(problem).apply();
		println("ExactBP result:");
		if (unnormalizedMarginal instanceof SamplingFactor) {
			println(((SamplingFactor)unnormalizedMarginal).nestedString(true));
		}
		return unnormalizedMarginal;
	}
	
	@SuppressWarnings("unused")
	private Factor useGlobalProduct(Problem problem) {
		Factor globalProduct = fold(problem.getModel().getFactors(), Factor::multiply, IDENTITY_FACTOR);
		List<Variable> eliminatedVariables = 
				collectToList(
						problem.getModel().getVariables(), 
						v -> ! isRemainingVariable(v, problem));
		Factor result = globalProduct.sumOut(eliminatedVariables);
		return result;
	}
	
	private SamplingFactor ensureItsSamplingFactor(Factor factor, ExpressionVariable queryVariable, Random random) {
		if (factor instanceof ConstantFactor) {
			return ExpressionDiscretization.makeUniformSamplingFactor(queryVariable, random, context);
		}
		else if (factor instanceof SamplingFactor) {
			return (SamplingFactor) factor;
		}
		else {
			throw new Error(getClass() + "' inner solver should have received either a ConstantFactor or a SamplingFactor, but got " + factor.getClass().getSimpleName() + " " + factor);
		}
	}
	
	private Factor useDynamicGlobalProduct(Problem problem, Random random) {
		ArrayList<SamplingFactor> samplingFactors = getFactors(problem);
		Factor globalProduct;
		if (samplingFactors.isEmpty()) {
			globalProduct = IDENTITY_FACTOR;
		}
		else {
			globalProduct = new DynamicSamplingProductFactor(samplingFactors, random);
		}
		List<Variable> eliminatedVariables = 
				collectToList(
						problem.getModel().getVariables(), 
						v -> ! isRemainingVariable(v, problem));
		Factor result = globalProduct.sumOut(eliminatedVariables);
		return result;
	}

	private ArrayList<SamplingFactor> getFactors(Problem problem) {
		return mapIntoArrayList(problem.getModel().getFactors(), f -> ((SamplingFactor) f));
	}

	private boolean isRemainingVariable(Variable variable, Problem problem) {
		boolean result = 
				variable.equals(problem.getQueryVariable()) 
				|| 
				problem.getIsParameterPredicate().test(variable);
		return result;
	}

	private Expression getNormalizedMarginalExpression(Factor unnormalizedMarginal, Problem problem) {
		Expression normalizedMarginalExpression;
		if (unnormalizedMarginal instanceof ConstantFactor) {
			normalizedMarginalExpression = getNormalizedMarginalExpressionFromConstantFactor(problem);
		}
		else {
			normalizedMarginalExpression = getNormalizedMarginalExpressionFromSamplingFactor(unnormalizedMarginal, problem);
		}
		return normalizedMarginalExpression;
	}

	private Expression getNormalizedMarginalExpressionFromConstantFactor(Problem problem) {
		ExpressionVariable queryVariable = (ExpressionVariable) problem.getQueryVariable();
		Expression normalizedMarginalExpression = normalize(queryVariable, ONE, context);
		return normalizedMarginalExpression;
	}

	private Expression getNormalizedMarginalExpressionFromSamplingFactor(Factor unnormalizedMarginal, Problem problem) {
		SamplingFactor unnormalizedSamplingFactor = getUnnormalizedSamplingFactor(unnormalizedMarginal);
		Expression normalizedMarginalExpression = makeNormalizedMarginalExpression(unnormalizedSamplingFactor, problem);
		return normalizedMarginalExpression;
	}

	private SamplingFactor getUnnormalizedSamplingFactor(Factor unnormalizedMarginal) {
		SamplingFactor unnormalizedSamplingFactor = (SamplingFactor) unnormalizedMarginal; 
		myAssert(isDefinedOnExpressionVariables(unnormalizedSamplingFactor), () -> getClass() + " requires " + ExactBP.class.getSimpleName() + " to return " + SamplingFactor.class.getSimpleName() + " defined on " + ExpressionVariable.class.getSimpleName() + "s only.");
		return unnormalizedSamplingFactor;
	}

	private Expression makeNormalizedMarginalExpression(SamplingFactor unnormalizedSamplingFactor, Problem problem) {
		int queryIndex = unnormalizedSamplingFactor.getVariables().indexOf(problem.getQueryVariable());
		Expression normalizedMarginalExpression = 
				expressionSamplingFactor(
						unnormalizedSamplingFactor, 
						queryIndex, 
						fromExpressionVariableToNumberOfDiscreteValues, 
						initialNumberOfSamples,
						context);
		return normalizedMarginalExpression;
	}
	
	@Override
	public void interrupt() {
		throw new Error(this.getClass() + ".interrupt not implemented yet");
	}

	private boolean isDefinedOnExpressionVariables(SamplingFactor unnormalizedSamplingFactor) {
		return forAll(unnormalizedSamplingFactor.getVariables(), v -> v instanceof ExpressionVariable);
	}

}