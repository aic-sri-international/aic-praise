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
package com.sri.ai.praise.lbp.core;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.grinder.rewriterrefiner.RewriterRefiner;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.RandomVariableFromMessageRewriterCall;

/**
 * A Refiner for use by the Anytime algorithm.
 * 
 * @author oreilly
 *
 */
@Beta
public class AnytimeRefiner extends RewriterRefiner {

	protected RandomVariableFromMessageRewriterCall randomVariableFromMessageRewriterCall;

	@Override
	protected boolean callMustBeWrappedInRewriterRefiner(String rewriterName, Expression expression) {
		return isChildABoundsRewriter(rewriterName, expression);
	}
	
	public AnytimeRefiner(String rewriterName, Expression expression, RewritingProcess process,
				RandomVariableFromMessageRewriterCall randomVariableFromMessageRewriterCall) {
		this(rewriterName, Tuple.make(expression, Expressions.ZERO), process, randomVariableFromMessageRewriterCall,
			 new ConcurrentHashMap<ChildRewriterInvocation, RewriterRefiner>());
		// This is the root rewriter, which means this instance
		// will increment the time step parameter for each of its
		// calls to its bounds rewriter.
		isRootRefiner = true;
	}
	
	public AnytimeRefiner(String rewriterName, Expression expression, RewritingProcess process,
			RandomVariableFromMessageRewriterCall randomVariableFromMessageRewriterCall,
			Map<ChildRewriterInvocation, RewriterRefiner> sharedRewriterRefiners) {
		super(computeInitialBound(rewriterName, expression, process, randomVariableFromMessageRewriterCall));
		this.rewriterName                          = rewriterName;
		this.expression                            = expression;
		this.process                               = process;
		this.sharedRewriterRefiners                = sharedRewriterRefiners;
		this.randomVariableFromMessageRewriterCall = randomVariableFromMessageRewriterCall;
	}

	//
	// PROTECTED
	//
	@Override
	protected boolean resultIndicatesThatRefinerIsDone(Expression result) {
		return LPIUtil.isMessageValue(result, process);
	}
	
	static protected Expression computeInitialBound(
			String rewriterName, Expression expression, RewritingProcess process,
			RandomVariableFromMessageRewriterCall randomVariableFromMessageRewriterCall) {
		Expression randomVariable = randomVariableFromMessageRewriterCall.getRandomVariableFor(rewriterName, expression);
		
		if (randomVariable == null) {
			throw new Error("AnytimeRefiner must wrap bound rewriters only, not " + rewriterName + " (it needs a random variable to create a trivial bound on as an initial value)");
		}

		Expression randomVariableValueExpression = LPIUtil.getRandomVariableValueExpression(randomVariable, process);
		Expression result = LPIUtil.makeTrivialBound(randomVariableValueExpression);
		return result;
	}

	@Override
	protected RewriterRefiner makeChildRewriterRefiner(String childRewriterName, Expression childExpression, RewritingProcess childProcess) {
		return new AnytimeRefiner(childRewriterName, childExpression, childProcess, randomVariableFromMessageRewriterCall, sharedRewriterRefiners);
	}
	
	protected boolean isChildABoundsRewriter(String rewriterName, Expression expression) {
		boolean result = false;
		// If can find a random variable this indicates is a bounds rewriter.
		if (randomVariableFromMessageRewriterCall.getRandomVariableFor(rewriterName, expression) != null) {
			result = true;
		}
		return result;
	}
}
