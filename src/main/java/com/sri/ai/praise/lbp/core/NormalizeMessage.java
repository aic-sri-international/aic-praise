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
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_normalize_message}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class NormalizeMessage extends AbstractLBPHierarchicalRewriter implements LBPRewriter {
	
	public NormalizeMessage() {
	}
	
	@Override
	public String getName() {
		return R_normalize_message;
	}
	
	/**
	 * @see LBPRewriter#R_normalize_message
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		// a tuple of the form: ([ v ], E).
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}

		Expression randomVariable = Tuple.get(expression, 0);
		Expression expressionE    = Tuple.get(expression, 1);

		Expression result = null;

		if (Justification.isEnabled()) {
			Justification.log(Expressions.apply(LPIUtil.FUNCTOR_NORMALIZE, expressionE));
		}

		if (IfThenElse.isIfThenElse(expressionE)) {
			Expression condition  = IfThenElse.getCondition(expressionE);
			Expression thenBranch = IfThenElse.getThenBranch(expressionE);
			Expression elseBranch = IfThenElse.getElseBranch(expressionE);

			if (!LPIUtil.containsRandomVariableValueExpression(condition, process)) {
				Trace.log("Externalizing conditional");

				if (Justification.isEnabled()) {
					Justification.beginEqualityStep("externalization of conditional");
					Expression externalized = IfThenElse.make(condition,
							Expressions.apply(LPIUtil.FUNCTOR_NORMALIZE, thenBranch),
							Expressions.apply(LPIUtil.FUNCTOR_NORMALIZE, elseBranch));
					Justification.endEqualityStep(externalized);
				}

				Justification.beginEqualityStep("solving internal normalizations");
				result = GrinderUtil.branchAndMergeOnACondition(
						condition,
						newNormalize(), new Expression[] { randomVariable, thenBranch },
						newNormalize(), new Expression[] { randomVariable, elseBranch },
						R_check_branch_reachable, 
						R_basic, process);
			}
			else {
				Trace.log("if E is 'if v then Alpha else Beta'");

				// must be a conditional on the random variable value
				if (thenBranch.equals(Expressions.ZERO) && elseBranch.equals(Expressions.ZERO)) {
					Trace.log("    if Alpha = Beta = 0");
					Trace.log("        error: 'Cannot normalize message with partition 0'");
					throw new IllegalArgumentException(
							"Asked to normalize message with partition 0: "
									+ expressionE);
				} 
				else if (thenBranch.equals(Expressions.ZERO)) {
					Trace.log("    if Alpha = 0");
					Trace.log("        return if v then 0 else 1");
					Justification.beginEqualityStep("deterministic message because of 0 in then branch");
					result = IfThenElse.make(condition, Expressions.ZERO, Expressions.ONE);
				} 
				else if (elseBranch.equals(Expressions.ZERO)) {
					Trace.log("    if Beta = 0");
					Trace.log("        return if v then 1 else 0");
					Justification.beginEqualityStep("deterministic message because of 0 in else branch");
					result = IfThenElse.make(condition, Expressions.ONE, Expressions.ZERO);
				} 
				else {
					Trace.log("    Z = R_basic(Alpha + Beta)");
					Expression partitionZ = process.rewrite(R_basic, Expressions.apply(FunctorConstants.PLUS, thenBranch, elseBranch));

					Trace.log("    return if v then R_basic(Alpha/Z) else R_basic(Beta/Z)");
					Justification.beginEqualityStep("apply partition function to each branch");

					BranchRewriteTask[] taskRewriters = new BranchRewriteTask[] {
							new BranchRewriteTask(newPartition(), new Expression[] {thenBranch, partitionZ}),
							new BranchRewriteTask(newPartition(), new Expression[] {elseBranch, partitionZ})
					};

					List<Expression> taskResults    = GrinderUtil.branchAndMergeTasks(taskRewriters, process);
					Expression normalizedThenBranch = taskResults.get(0);
					Expression normalizedElseBranch = taskResults.get(1);
					result = IfThenElse.make(condition, normalizedThenBranch, normalizedElseBranch);
				}
			}
		} 
		else { 
			// at this point, it is an unconditional arithmetic expression,
			// that is, a constant.
			Trace.log("otherwise // not a conditional on v");
			Trace.log("    return 0.5");
			Justification
					.beginEqualityStep("normalization of a constant");
			result = Expressions.makeSymbol(0.5);
		}

		Justification.endEqualityStep(result);

		return result;
	}
	
	//
	// PRIVATE
	//
	private RewriteOnBranch newNormalize() {
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				Expression result = process.rewrite(R_normalize_message,
										LPIUtil.argForNormalizeRewriteCall(expressions[0], expressions[1]));
				return result;
			}
		};
	}
	
	private RewriteOnBranch newPartition() { 
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				Expression result = process.rewrite(R_basic, 
										Expressions.apply(FunctorConstants.DIVISION, expressions[0], expressions[1]));
				return result;
			}
		};
	}
}
