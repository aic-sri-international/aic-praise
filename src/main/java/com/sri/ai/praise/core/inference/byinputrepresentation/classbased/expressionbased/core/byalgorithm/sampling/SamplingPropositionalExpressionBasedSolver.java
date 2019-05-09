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
package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.sampling;

import java.util.Random;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.adaptinginterfacebasedsolver.SolverToExpressionBasedSolverAdapter;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.Solver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.sampling.SolverAdapterForDynamicSampling;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.sampling.SolverAdapterForExactBPOnSamplingFactors;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.sampling.SolverAdapterForStaticSampling;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.fromexpressionstosamplingfactors.ExpressionBasedProblemToSamplingFactorInterfaceBasedProblemConversion;

public class SamplingPropositionalExpressionBasedSolver extends SolverToExpressionBasedSolverAdapter {

	public SamplingPropositionalExpressionBasedSolver(
			SolverType solverType, 
			Function<Expression, Integer> fromVariableToNumberOfDiscreteValues, 
			int initialNumberOfSamples, 
			Random random) {
		
		super(
				new ExpressionBasedProblemToSamplingFactorInterfaceBasedProblemConversion(random)::translate, 

				ebp -> makeSolver(solverType, fromVariableToNumberOfDiscreteValues, initialNumberOfSamples, ebp.getContext()));
	}

	private static Solver makeSolver(
			SolverType solverType,
			Function<Expression, Integer> fromVariableToNumberOfDiscreteValues,
			int initialNumberOfSamples, 
			Context context) {

		switch (solverType) {

		case dynamic:
			return new SolverAdapterForDynamicSampling(
					fromVariableToNumberOfDiscreteValues, 
					initialNumberOfSamples, 
					context);

		case planned:
			return new SolverAdapterForStaticSampling(
					fromVariableToNumberOfDiscreteValues, 
					initialNumberOfSamples, 
					context);

		case exactbp:
			return new SolverAdapterForExactBPOnSamplingFactors(
					fromVariableToNumberOfDiscreteValues, 
					initialNumberOfSamples, 
					context);

		default: throw new Error(SamplingPropositionalExpressionBasedSolver.class.getSimpleName() + " received illegal solver type " + solverType);

		}
	}

}