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

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;

/**
 * Default implementation of {@link LBPRewriter#R_prod_m_and_prod_factor}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class ProductMessageAndProductFactor extends AbstractLBPHierarchicalRewriter implements LBPRewriter {
	
	public ProductMessageAndProductFactor() {
	}
	
	@Override
	public String getName() {
		return R_prod_m_and_prod_factor;
	}

	/**
	 * @see LBPRewriter#R_prod_m_and_prod_factor
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		// a tuple of the form: (m * prod_F in S m_V<-F, beingComputed).
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		Expression times = Tuple.get(expression, 0);
		if (!times.hasFunctor(FunctorConstants.TIMES) || times.numberOfArguments() != 2) {
			throw new IllegalArgumentException("Times argument is invalid:"+times);
		}
		Expression conditionalMessage         = times.get(0);
		Expression productOfFactorsToVariable = times.get(1); 
		Expression beingComputed              = Tuple.get(expression, 1);
		
		Expression result = null;
		
		if (Justification.isEnabled()) {
			Justification.log(times);
		}
		
		// Cases:
		if (IfThenElse.isIfThenElse(productOfFactorsToVariable)) {
			// Externalizes conditionals on Pi
			// if Pi is 'if condition then p1 else p2
			// return R_basic(if condition
			// then R_prod_m_and_prod_factor(m * ExprPi1, beingComputed, C)
			// else R_prod_m_and_prod_factor(m * ExprPi2, beingComputed, C))
			result = rewriteExternalizeProduct(conditionalMessage, productOfFactorsToVariable, beingComputed, process);
		} 
		else {

			// prod_F in S m_V<-F
			LPIUtil.assertProductOk(productOfFactorsToVariable);
			Expression prodIntensionalSet = productOfFactorsToVariable.get(0);
			Expression msgToV_F           = ((IntensionalSet) prodIntensionalSet).getHead();
			LPIUtil.assertMessageToFromOk(msgToV_F, process);

			Expression randomVariable                = msgToV_F.get(0);
			Expression randomVariableValueExpression = LPIUtil.getRandomVariableValueExpression(randomVariable, process);

			if (LPIUtil.isConditionalNotContainingRandomVariableValueExpressionsInCondition(conditionalMessage, process)) {
				// Externalizes conditionals on m
				// if m is 'if condition then m1 else m2'");
				// return R_basic(if condition
				// then R_prod_m_and_prod_factor(m1 * prod_F in S m_V<-F, beingComputed, C)");
				// else R_prod_m_and_prod_factor(m2 * prod_F in S m_V<-F, beingComputed, C))");
				result = rewriteExternalizeMessage(conditionalMessage, productOfFactorsToVariable, beingComputed, process);
			} 
			else if (isSingularlyDeterministic(conditionalMessage, randomVariableValueExpression, process)) {
				Trace.log("m is deterministic");
				Trace.log("    return m");
				Justification.beginEqualityStep("first message is deterministic, so we ignore the remaining ones");
				result = conditionalMessage;
				Justification.endEqualityStep(result);
			} 
			else {
				Trace.log("else m is not deterministic");
				Trace.log("    return R_normalize(m * R_prod_factor(prod_F in S m_V<-F, beingComputed, C))");

				Justification.beginEqualityStep("computing product of remaining messages");
				Expression rewriteOfProductOfFactorsToVariable = process.rewrite(R_prod_factor,
																	LPIUtil.argForProductFactorRewriteCall(productOfFactorsToVariable, beingComputed));
				Expression productOfMessagesToSimplify = Expressions.apply(FunctorConstants.TIMES, conditionalMessage, rewriteOfProductOfFactorsToVariable);
				if (Justification.isEnabled()) {
					Expression currentExpression = productOfMessagesToSimplify;
					Justification.endEqualityStep(currentExpression);
				}

				Justification.beginEqualityStep("multiplying the two messages");
				result = process.rewrite(R_normalize, productOfMessagesToSimplify);
				Justification.endEqualityStep(result);
			}
		}

		return result;
	}

	//
	// PRIVATE METHODS
	//
	private Expression rewriteExternalizeMessage(Expression conditionalMessage,
			Expression productOfFactorsToVariable, 
			Expression beingComputed, 
			RewritingProcess process) {
		Trace.log("if m is 'if condition then ExprM1 else ExprM2'");
		Trace.log("    return R_basic(if condition");
		Trace.log("                   then R_prod_m_and_prod_factor(ExprM1 * prod_F in S m_V<-F, beingComputed)");
		Trace.log("                   else R_prod_m_and_prod_factor(ExprM2 * prod_F in S m_V<-F, beingComputed))");

		Justification.beginEqualityStep("externalizing conditional on initial message");
		Expression condition = IfThenElse.getCondition(conditionalMessage);
		Expression m1        = IfThenElse.getThenBranch(conditionalMessage);
		Expression m2        = IfThenElse.getElseBranch(conditionalMessage);
		if (Justification.isEnabled()) {
			Expression currentExpression = 
				IfThenElse.make(
						condition,
						Expressions.apply("*", m1, productOfFactorsToVariable),
						Expressions.apply("*", m2, productOfFactorsToVariable));
			Justification.endEqualityStep(currentExpression);
		}

		Justification.beginEqualityStep("computing products in then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newCallProductMessageAndProductFactorRewrite(), new Expression[] { m1, productOfFactorsToVariable, beingComputed },
				newCallProductMessageAndProductFactorRewrite(), new Expression[] { m2, productOfFactorsToVariable, beingComputed },
				R_check_branch_reachable, 
				R_basic, process);
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteExternalizeProduct(
			Expression conditionalMessage,
			Expression productOfFactorsToVariable, 
			Expression beingComputed, 
			RewritingProcess process) {
		Trace.log("if Pi is 'if condition then ExprPi1 else ExprPi2");
		Trace.log("    return R_basic(if condition");
		Trace.log("                   then R_prod_m_and_prod_factor(m * ExprPi1, beingComputed)");
		Trace.log("                   else R_prod_m_and_prod_factor(m * ExprPi2, beingComputed))");

		Justification.beginEqualityStep("externalizing conditional on remaining product of messages");
		Expression condition   = IfThenElse.getCondition(productOfFactorsToVariable);
		Expression productThen = IfThenElse.getThenBranch(productOfFactorsToVariable);
		Expression productElse = IfThenElse.getElseBranch(productOfFactorsToVariable);
		if (Justification.isEnabled()) {
			Expression currentExpression = 
				IfThenElse.make(
						condition,
						Expressions.apply("*", conditionalMessage, productThen),
						Expressions.apply("*", conditionalMessage, productElse));
			Justification.endEqualityStep(currentExpression);
		}

		Justification.beginEqualityStep("computing products in then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newCallProductMessageAndProductFactorRewrite(), new Expression[] { conditionalMessage, productThen, beingComputed },
				newCallProductMessageAndProductFactorRewrite(), new Expression[] { conditionalMessage, productElse, beingComputed },
				R_check_branch_reachable, 
				R_basic, process);
		Justification.endEqualityStep(result);

		return result;
	}

	/**
	 * Note from Rodrigo:<br>
	 * <br>
	 * Deterministic here means that the message assigns non-zero potential to a
	 * single value of its random variable. For example
	 * 
	 * if p(X) then 1 else 0
	 * 
	 * assigns non-zero potential to p(X)=true only.
	 * 
	 * Intuitively, this means the random variable is no longer random; it has a
	 * determined value - all others will have probability zero.
	 * 
	 * This is why this rewriter works; once one message determines a value, we
	 * don't need to consider the other messages (they are all on the same value
	 * V).
	 * 
	 * This works only under two assumptions, though. One is that messages are
	 * normalized by the sum of potentials at the end, so
	 * "if p(X) then 1 else 0" and "if p(X) then 20 else 0" will end up meaning
	 * the same thing. This is why I don't need to consider the other messages;
	 * they could in principle change the absolute non-zero value, but that's
	 * not important.
	 * 
	 * The other assumption is that the other messages are not deterministic on
	 * *another* value for the same variable; that would be contradictory and
	 * make the whole model inconsistent and we would not be able to draw any
	 * conclusions from it. So we are making the assumption that the model is
	 * not "crazy". <br>
	 * <br>
	 * Additional Note:<br>
	 * Assumptions is that m is always of the form:<br>
	 * [if v then E1 else E2] <br>
	 * 
	 * This may change in the future for Value Elimination or even stronger
	 * versions of Belief Propagation. To handle this: evaluate m with R_basic
	 * for different values of the variable and see if you get a single
	 * non-zero. That would be completely independent of m's shape.
	 * 
	 * @param conditionalMessage
	 *            a conditional message of the form: [if v then E1 else E2].
	 * @param randomVariableValueExpression
	 *            the value expression of a Random Variable, i.e v in [v].
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return true if m is deterministic, false otherwise.
	 */
	private boolean isSingularlyDeterministic(Expression conditionalMessage, Expression randomVariableValueExpression, RewritingProcess process) {
		int nonZeroCount = 0;

		for (Expression vVal : Model.range(randomVariableValueExpression, process)) {
			Expression subM   = SemanticSubstitute.replace(conditionalMessage, randomVariableValueExpression, vVal, process);
			Expression basicE = process.rewrite(R_basic, subM);

			if (!Expressions.ZERO.equals(basicE)) {
				nonZeroCount++;
			}
			if (nonZeroCount > 1) {
				// Non-deterministic if this occurs
				break;
			}
		}

		// Is deterministic if we get a single non-zero
		return nonZeroCount == 1;
	}
	
	//
	private RewriteOnBranch newCallProductMessageAndProductFactorRewrite() {
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
	
				Expression result = process.rewrite(R_prod_m_and_prod_factor, LPIUtil.argForProductMessageAndProductFactorRewriteCall(expressions[0], expressions[1], expressions[2]));
				
				return result;
			}
		};
	}
}
