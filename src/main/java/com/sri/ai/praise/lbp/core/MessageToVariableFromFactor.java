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

import static com.sri.ai.util.Util.list;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_m_to_v_from_f}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class MessageToVariableFromFactor extends AbstractLBPHierarchicalRewriter implements LBPRewriter {
	
	private LBPConfiguration configuration = null;
	
	public MessageToVariableFromFactor(LBPConfiguration configuration) {
		this.configuration  = configuration;
	}
	
	@Override
	public String getName() {
		return R_m_to_v_from_f;
	}
	
	/**
	 * @see LBPRewriter#R_m_to_v_from_f
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		// a tuple of the form: (m_V<-F, beingComputed).
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 4) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		
		Expression msgToV_F                  = Tuple.get(expression, 0);
		Expression conditionC                = Tuple.get(expression, 1);
		IndexExpressionsSet indexExpressions = new ExtensionalIndexExpressionsSet(Tuple.getElements(Tuple.get(expression, 2))); 
		Expression beingComputed             = Tuple.get(expression, 3);

		if (!Expressions.hasFunctor(msgToV_F, LPIUtil.FUNCTOR_MSG_TO_FROM)
				|| msgToV_F.numberOfArguments() != 2 
				|| !BracketedExpressionSubExpressionsProvider
						.isBracketedExpression(msgToV_F.get(0))
				|| !BracketedExpressionSubExpressionsProvider
				.isBracketedExpression(msgToV_F.get(1))
				|| !BracketedExpressionSubExpressionsProvider.isRandomVariable(
						msgToV_F.get(0), process)) {
			throw new IllegalArgumentException(
					"msgToV_F is not a message to a variable from a factor:msgToV_F="
							+ msgToV_F);
		}

		Justification.log(msgToV_F);

		Expression randomVariable = msgToV_F.get(0);
		Expression factor         = msgToV_F.get(1);

		Expression result;

		Trace.log("In <- R_in(m_V<-F in beingComputed)");
		Expression pairV_F = Tuple.make(randomVariable, factor);
		Expression in      = null;
		LBPConfiguration.BeliefPropagationUpdateSchedule beliefPropagationUpdateSchedule = configuration.getBeliefPropagationUpdateSchedule();
		if (beliefPropagationUpdateSchedule == LBPConfiguration.BeliefPropagationUpdateSchedule.ASYNCHRONOUS_INDIVIDUAL_BASED_CYCLE_DETECTION) { 
			Trace.log("if asynchronous individual schedule");
			Trace.log("    In <- R_in((V,F) in beingComputed)");
			in = process.rewrite(R_in, LPIUtil.argForInRewriteCall(pairV_F, beingComputed));

			Trace.log("    beingComputed <- R_basic(beingComputed union {{(V,F)}})");
			beingComputed = LPIUtil.extendBeingComputed(beingComputed, pairV_F, process);
		} 
		else if (beliefPropagationUpdateSchedule == LBPConfiguration.BeliefPropagationUpdateSchedule.ASYNCHRONOUS_GROUP_BASED_CYCLE_DETECTION) {
			Trace.log("if asynchronous group schedule");
			Trace.log("    In <- R_in((V,F) in beingComputed)");
			in = process.rewrite(R_in, LPIUtil.argForInRewriteCall(pairV_F, beingComputed));
			Trace.log("    In <- R_complete_normalize(In)");
			in = process.rewrite(R_complete_normalize, in);

			Trace.log("    beingComputed <- beingComputed union {{(V,F) | C }}_I");
			beingComputed = LPIUtil.extendBeingComputedWithIntensionalMultiSet(beingComputed, pairV_F, conditionC, indexExpressions, process);
		} 
		else if (beliefPropagationUpdateSchedule == LBPConfiguration.BeliefPropagationUpdateSchedule.SYNCHRONOUS){
			Trace.log("if synchronous schedule");
			Trace.log("    In <- beingComputed is NOT empty");
			if (!Sets.isEmptySet(beingComputed)) {
				in = Expressions.TRUE;
			} 
			else {
				in = Expressions.FALSE;
			}
			Trace.log("    // In = " + in);
			Trace.log("    beingComputed <- R_basic(beingComputed union {{(V,F)}})");
			beingComputed = LPIUtil.extendBeingComputed(beingComputed, pairV_F, process);
			Trace.log("    // beingComputed = " + beingComputed);
		}
		else {
			throw new UnsupportedOperationException("Belief propagation update schedule "+beliefPropagationUpdateSchedule+ " is not supported.");
		}

		Expression previousRandomVariableBeingNormalized = LPIUtil.setRandomVariableBeingNormalizedAndReturnPreviousOne(randomVariable, process);

		result  = GrinderUtil.branchAndMergeOnACondition(
				in,
				// pm_V<-F
				newThenBranchPreviousMessage(), new Expression[] { randomVariable, factor},
				// M
				newElseBranchMessage(), new Expression[] { randomVariable, factor, beingComputed},
				R_check_branch_reachable, 
				R_basic, process);

		LPIUtil.restorePreviousRandomVariableBeingNormalized(previousRandomVariableBeingNormalized, process);

		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	private RewriteOnBranch newThenBranchPreviousMessage() {
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_PREVIOUS_MSG_TO_FROM, expressions[0], expressions[1]);
				
				return result;
			}
		};
	}
	
	private RewriteOnBranch newElseBranchMessage() {
		return new RewriteOnBranch() {
			// Note: GrinderUtil.branchAndMergeOnACondition will already have extended the
			// contextual constraint by 'not In' before calling this method.
			// Note: The elseBranchProcess may differ from the original request.getRewritingProcess()
			// due to the branching that can occur.
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess elseBranchProcess) {
				Expression V             = expressions[0];
				Expression F             = expressions[1];
				Expression beingComputed = expressions[2];
				Expression E             = LPIUtil.getFactorValueExpression(F, elseBranchProcess);
				Expression VPrime        = Expressions.makeUniqueVariable("V'", F, elseBranchProcess);
				Expression msgToF_VPrime = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_MSG_TO_FROM, F, VPrime);
				Expression neighF        = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_NEIGHBOR, F);
				
				Trace.log("under contextual constraint incremented by 'not In'");
		
				Justification.beginEqualityStep("by the definition of messages to random variables");
				Expression currentExpression = null;
				if (Justification.isEnabled()) {
					Expression NBeforeEvaluation = Expressions.apply(FunctorConstants.SET_DIFFERENCE, neighF, ExtensionalSet.makeSingleton(V));
					currentExpression = makeCurrentExpressionGivenExpressionForN(NBeforeEvaluation, VPrime, E, msgToF_VPrime);
					Justification.endEqualityStep(currentExpression);
				}
				
				if (Justification.isEnabled()) {
					Justification.beginEqualityStep("by computing neighbors of " + F);
				}
				
				Trace.log("    N <- R_set_diff(R_neigh_f(Neigh(F)) \\ V)");
				Expression neighborsOfF = elseBranchProcess.rewrite(R_neigh_f, neighF);
				
				if (Justification.isEnabled()) {
					Expression newDifference = Expressions.apply(FunctorConstants.SET_DIFFERENCE, neighborsOfF, ExtensionalSet.makeSingleton(V));
					currentExpression = makeCurrentExpressionGivenExpressionForN(newDifference, VPrime, E, msgToF_VPrime);
					Justification.endEqualityStep(currentExpression);
				}
				
				Justification.beginEqualityStep("by set difference");
				
				Expression N = LPIUtil.callSetDiff(neighborsOfF, V, elseBranchProcess);
				Trace.log("    // N = {}", N);
				
				if (Justification.isEnabled()) {
					currentExpression = makeCurrentExpressionGivenExpressionForN(N, VPrime, E, msgToF_VPrime);
					Justification.endEqualityStep(currentExpression);
				}
		
				Justification.beginEqualityStep("by summing random variable values out");
				
				Trace.log("    M <- R_sum(sum_N value(F) prod_{V' in N} m_F<-V', V, beingComputed)");
				Expression productOfVariablesToFactor = LPIUtil.makeProductOfMessages(VPrime, N, msgToF_VPrime, Expressions.TRUE);
				Expression M = elseBranchProcess.rewrite(R_sum, LPIUtil.argForSumRewriteCall(N, E, productOfVariablesToFactor, V, beingComputed));
				Trace.log("    // M = {}", M);
				
				Trace.log("    M <- R_normalize_random_variable_condition(M)");
				M = elseBranchProcess.rewrite(R_normalize_random_variable_condition, LPIUtil.argForNormalizeRandomVariableConditionRewriteCall(V, M));
				Trace.log("    // M = {}", M);
				
				Justification.endEqualityStep(M);
				
				return M;
			}
		};
	}

	/**
	 * Method creating current expression for justification purposes.
	 * It abuses notation as N might be a function to be evaluated into a set of indices,
	 * which is not supported by the current language (it used to be supported by the use of a meta-operator 'value of', but
	 * that is not supported anymore).
	 * As a result, this method should be used for presentation purposes only.
	 */
	private static Expression makeCurrentExpressionGivenExpressionForN(
			Expression N, Expression VPrime, Expression E, Expression msgToF_VPrime) {
		Expression currentExpression;
		Expression productOfVariablesToFactorBeforeEvalutionOfN = LPIUtil.makeProductOfMessages(
				VPrime, N, msgToF_VPrime, Expressions.TRUE);
		Expression head = Expressions.apply(FunctorConstants.TIMES, E, productOfVariablesToFactorBeforeEvalutionOfN);
		currentExpression =
			Expressions.apply(
					FunctorConstants.SUM,
					new DefaultIntensionalMultiSet(list(N), head, Expressions.TRUE));
		return currentExpression;
	}
}
