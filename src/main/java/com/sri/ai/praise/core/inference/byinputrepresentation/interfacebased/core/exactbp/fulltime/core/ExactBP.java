/*
 * Copyright (c) 2015, SRI International
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
 * Neither the name of the aic-praise nor the names of its
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

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;
import static com.sri.ai.util.livesets.core.lazy.memoryless.ExtensionalLiveSet.liveSet;
import static com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet.redirectingTo;

import java.util.function.Predicate;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;
import com.sri.ai.util.livesets.core.lazy.memoryless.ExtensionalLiveSet;
import com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet;

/**
 * A solver that returns the normalized marginal of a query given a factor network, using the Exact BP algorithm.
 * 
 * @author braz
 *
 */
public class ExactBP extends ExactBPNodeFromVariableToFactor {

	@Override
	public
	Factor apply() {
		return
				explanationBlock("Computing unnormalized marginal of ", getRoot(), code(() -> {
					return super.apply();
				}), "Unnormalized marginal is ", getRoot(), " is ", RESULT);
	}
	
	public ExactBP(Variable query, FactorNetwork factorNetwork) {
		this(query, factorNetwork, v -> false /* default is "no uninterpreted constants" */);
	}

	public ExactBP(Problem problem) {
		this(problem.getQueryVariable(), problem.getModel(), problem.getIsParameterPredicate());
	}
	
	private ExactBP(Variable query, FactorNetwork factorNetwork, Predicate<Variable> isParameterPredicate) {
		super(
				query,
				makeParent(),
				makeExcludedFactors(),
				makeIncludedFactors(),
				factorNetwork,
				isParameterPredicate);
	}

	private static Factor makeParent() {
		return null;  // there is none, as the message on the query is the final computation
	}
	
	private static ExtensionalLiveSet<Factor> makeExcludedFactors() {
		return liveSet(list()); // there is no "exterior" to this ExactBPNode, so there are no excluded factors
	}

	private static RedirectingLiveSet<Factor> makeIncludedFactors() {
		return redirectingTo(makeExcludedFactors()); // the search initially starts with no included factors having been included yet
	}
}