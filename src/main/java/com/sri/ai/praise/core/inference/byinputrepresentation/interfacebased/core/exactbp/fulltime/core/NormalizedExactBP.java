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

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.PRAiSEUtil;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.Solver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.ConstantFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork;
import com.sri.ai.util.DefaultExplanationTree;

public class NormalizedExactBP implements Solver {

	@Override
	public Expression solve(Problem problem) {
		ExactBP exactBP = new ExactBP(problem);
		ExpressionVariable queryVariable = (ExpressionVariable) problem.getQueryVariable();
		Factor factor = exactBP.apply();
		Context context = getContext(problem);
		ExpressionFactor unnormalized = getUnnormalizedExpressionFactor(factor, context);
		ExpressionFactor normalizedMarginal = new DefaultExpressionFactor(normalize(queryVariable, unnormalized, context), context);
		normalizedMarginal.setExplanation(makeExplanation(normalizedMarginal, factor));
		return normalizedMarginal;
	}

	private DefaultExplanationTree makeExplanation(ExpressionFactor normalizedMarginal, Factor factor) {
		return new DefaultExplanationTree(PRAiSEUtil.conditionOnlyIfDeterministic(normalizedMarginal) + ", after normalizing:", factor.getExplanation());
	}

	private Context getContext(Problem problem) {
		return ((ExpressionFactorNetwork) problem.getModel()).getContext();
	}

	private ExpressionFactor getUnnormalizedExpressionFactor(Factor factor, Context context) {
		return factor instanceof ConstantFactor? new DefaultExpressionFactor(ONE, context) : (ExpressionFactor) factor;
	}

	@Override
	public void interrupt() {
		throw new Error(this.getClass() + ".interrupt not implemented yet");
	}
}