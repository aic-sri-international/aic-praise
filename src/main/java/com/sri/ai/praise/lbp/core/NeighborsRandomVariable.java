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

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.BranchRewriteTask;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;

/**
 * Default implementation {@link LBPRewriter#R_neigh_v}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class NeighborsRandomVariable extends AbstractLBPHierarchicalRewriter implements LBPRewriter {
	
	public NeighborsRandomVariable() {
	}
	
	@Override
	public String getName() {
		return R_neigh_v;
	}
	
	/**
	 * @see LBPRewriter#R_neigh_v
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		// an expression in the form: Neigh(V), where V is a random variable.
		if (!expression.hasFunctor(LPIUtil.FUNCTOR_NEIGHBOR) || expression.numberOfArguments() != 1) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		
		Expression randomVariable = expression.get(0);
		
		LPIUtil.assertRandomVariableOk(randomVariable, process);

		Expression result = null;

		if (Justification.isEnabled()) {
			Justification.log(expression);
		}
		
		Trace.log("return R_union(R_neigh_v_parf(V,PF_1)");
		Trace.log("               union ... union");
		Trace.log("               R_neigh_v_parf(V,PF_n))");
	
		List<Expression>    parfactors    = Model.getParfactors(process);
		BranchRewriteTask[] taskRewriters = new BranchRewriteTask[parfactors.size()];
		for (int i = 0; i < parfactors.size(); i++) {
			taskRewriters[i] = new BranchRewriteTask(
					new RewriteOnBranch() {
						
						@Override
						public Expression rewrite(Expression[] expressions, RewritingProcess process) {
							Expression result = process.rewrite(R_neigh_v_parf,
													LPIUtil.argForNeighborsOfRandomVariableInParfactorRewriteCall(expressions[0], expressions[1]));
							return result;
						}
					},
					new Expression[] {randomVariable, parfactors.get(i)});
		}
		
		List<Expression> unionArgs = GrinderUtil.branchAndMergeTasks(taskRewriters, process);
		Expression       union     = Expressions.apply(FunctorConstants.UNION, unionArgs);

		Justification.beginEqualityStep("union");
		result = process.rewrite(R_union, union);
		Justification.endEqualityStep(result);

		return result;
	}
}
