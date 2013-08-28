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
package com.sri.ai.praise;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.helper.RewriterLoggingNamedRewriterFilter;
import com.sri.ai.util.Util;

/**
 * A rewriter that normalizes basic expressions to having logic variable conditions on top of random variable ones.
 * 
 * @author braz
 *
 */
@Beta
public class MoveAllConditionsOnRandomVariablesDown extends AbstractRewriter {
	private Rewriter rRootRewriter = null;
	
	public MoveAllConditionsOnRandomVariablesDown() {
	}
	
	@Override
	public String getName() {
		return "Moving All conditions on RVs down";
	}
	
	public Rewriter getRootRewriter() {
		// Lazy initialize so that required supporting classes
		// can be setup and configured as necessary.
		if (rRootRewriter == null) {
			TotalRewriter externalizeAllConditionsOnLogicalVariables =
					new TotalRewriter(
							"Total rewriter externalizing conditions on logical variables",
							Util.list((Rewriter) new ExternalizeConditionalOnLogicalVariables()));

			TotalRewriter moveAllRandomVariableConditionsDown =
					new TotalRewriter(
							"Total rewriter moving all conditions on random variables down",
							Util.list((Rewriter) new MoveRandomVariableValueExpressionConditionDown()));
			
			TotalRewriter rootRewriter =
					new TotalRewriter(
							getName()+" Total Rewriter",
							Util.list(
									(Rewriter) externalizeAllConditionsOnLogicalVariables,
									(Rewriter) moveAllRandomVariableConditionsDown));
			
//			TotalRewriter rootRewriter = new TotalRewriter(getName()+" Total Rewriter", getAtomicRewriters());
			
			RewriterLoggingNamedRewriterFilter rewriterFilter = new RewriterLoggingNamedRewriterFilter();
			if (rewriterFilter.isRewriterFiltered(getName())) {
				rootRewriter.setOuterTraceEnabled(false);
			} 
			else {
				rootRewriter.setOuterTraceEnabled(true);
			}
			
			rRootRewriter = rootRewriter;
		}
		return rRootRewriter;
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = getRootRewriter().rewrite(expression, process);
		return result;
	}
	
	//
	// PROTECTED METHODS
	//
	protected List<Rewriter> getAtomicRewriters() {
		return new ArrayList<Rewriter>(
				Arrays.asList(new Rewriter[] {
						new MoveRandomVariableValueExpressionConditionDown(),
						new ExternalizeConditionalOnLogicalVariables()
				}));
	}
}