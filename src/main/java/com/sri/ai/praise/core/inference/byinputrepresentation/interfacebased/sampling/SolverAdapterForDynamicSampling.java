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
package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.sampling;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.mapIntoArrayList;

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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.DynamicSamplingProductFactor;

/**
 * A {@link Solver} adapter based on {@link DynamicSamplingProductFactor} for {@link Problem}s using {@link SamplingFactor}s defined on {@link ExpressionVariable}s.
 * 
 * @author braz
 *
 */
public class SolverAdapterForDynamicSampling extends AbstractSolverAdapterForSampling {

	public SolverAdapterForDynamicSampling(
			Function<Expression, Integer> fromExpressionVariableToNumberOfDiscreteValues,
			int initialNumberOfSamples,
			Context context) {
		
		super(
				fromExpressionVariableToNumberOfDiscreteValues,
				initialNumberOfSamples,
				context);
	}
	
	@Override
	protected Factor computeUnnormalizedMarginal(Problem problem, Random random) {
		return useDynamicGlobalProduct(problem, random);
	}

	protected Factor useDynamicGlobalProduct(Problem problem, Random random) {
		
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

}