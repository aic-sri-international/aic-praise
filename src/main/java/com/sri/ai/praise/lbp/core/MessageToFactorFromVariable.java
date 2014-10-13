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
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
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
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_m_to_f_from_v}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class MessageToFactorFromVariable extends AbstractLBPHierarchicalRewriter implements LBPRewriter {

	private LBPConfiguration      configuration      = null;

	public MessageToFactorFromVariable(LBPConfiguration configuration) {
		this.configuration  = configuration;
	}
	
	@Override
	public String getName() {
		return R_m_to_f_from_v;
	}

	/**
	 * @see LBPRewriter#R_m_to_f_from_v
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		//  a tuple of the form: (m_F<-V, beingComputed).
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		
		Expression messageToFFromV = Tuple.get(expression, 0);  
		Expression beingComputed   = Tuple.get(expression, 1);
		
		Justification.log(messageToFFromV);
		
		LPIUtil.assertMessageToFromOk(messageToFFromV, process);

		Expression factor         = messageToFFromV.get(0);		
		Expression randomVariable = messageToFFromV.get(1);
		
		LPIUtil.assertFactorOk(factor, process);
		LPIUtil.assertRandomVariableOk(randomVariable, process);
		
		Expression result;
		
		Expression pairF_V = Tuple.make(factor, randomVariable);
		Expression in      = null;
		LBPConfiguration.BeliefPropagationUpdateSchedule beliefPropagationUpdateSchedule = configuration.getBeliefPropagationUpdateSchedule();
		if (LBPConfiguration.BeliefPropagationUpdateSchedule.isAsynchronous(beliefPropagationUpdateSchedule)) {
			Trace.log("if asynchronous schedule");
			Trace.log("    In <- R_in((F,V) in beingComputed)");
			in = process.rewrite(R_in, LPIUtil.argForInRewriteCall(pairF_V, beingComputed));
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
		} 
		else {
			throw new UnsupportedOperationException("Belief propagation update schedule "+beliefPropagationUpdateSchedule+ " is not supported.");
		}

		Trace.log("return R_basic(if In then pm_F<-V else R_prod_factor(prod_F' in R_set_diff(R_neigh_v(Neigh(V))\\{F}) m_V<-F', R_basic(beingComputed union {{(F,V)}})))");

		Expression previousRandomVariableBeingNormalized = LPIUtil.setRandomVariableBeingNormalizedAndReturnPreviousOne(randomVariable, process);
		
		result = GrinderUtil.branchAndMergeOnACondition(
				in,
				// pm_F<-V
				newThenBranchPreviousMessage(), new Expression[] { factor, randomVariable},
				// R_prod_factor(prod_F' in R_set_diff(R_neigh_v(Neigh(V))\\{F}) m_V<-F', R_basic(beingComputed union (F,V)))
				newElseBranchProductFactor(), new Expression[] { factor, randomVariable, pairF_V, beingComputed},
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
	
	private RewriteOnBranch newElseBranchProductFactor() {
		return new RewriteOnBranch() {
			// Note: GrinderUtil.branchAndMergeOnACondition will already have extended the
			// contextual constraint by 'not In' before calling this method.
			// Note: The elseBranchProcess may differ from the original request.getRewritingProcess()
			// due to the branching that can occur.
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess elseBranchProcess) {
				Expression factor           = expressions[0];
				Expression randomVariable   = expressions[1];
				Expression pairF_V          = expressions[2];
				Expression beingComputed    = expressions[3];
				Expression factorIndexPrime = Expressions.makeUniqueVariable("F'", randomVariable, elseBranchProcess);
				Expression msgToV_FPrime    = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_MSG_TO_FROM, randomVariable, factorIndexPrime);
				Expression neighV           = Expressions.apply(LPIUtil.FUNCTOR_NEIGHBOR, randomVariable);
				
				Expression currentExpression = null;
				if (Justification.isEnabled()) {
					Justification.beginEqualityStep("definition of message to a factor from a random variable");

					Expression factorDomainBeforeEvaluation = Expressions.apply(FunctorConstants.SET_DIFFERENCE, neighV, ExtensionalSet.makeSingleton(factor));
					Expression indexExpression              = Expressions.apply(LPIUtil.FUNCTOR_IN, factorIndexPrime, factorDomainBeforeEvaluation);

					currentExpression =
						Expressions.apply(
								FunctorConstants.PRODUCT,
								new DefaultIntensionalMultiSet(list(indexExpression), msgToV_FPrime, Expressions.TRUE));
					
					Justification.endEqualityStep(currentExpression);
				}

				Justification.beginEqualityStep("computing neighbors of " + randomVariable + " and excluding " + factor);
				Expression neighborsOfV       = elseBranchProcess.rewrite(R_neigh_v, neighV);
				Expression neighborsOfVMinusF = LPIUtil.callSetDiff(neighborsOfV, factor, elseBranchProcess);

				if (Justification.isEnabled()) {
					currentExpression =
						Expressions.apply(
								FunctorConstants.PRODUCT,
								new DefaultIntensionalMultiSet(list(Expressions.apply(LPIUtil.FUNCTOR_IN, factorIndexPrime, neighborsOfVMinusF)), msgToV_FPrime, Expressions.TRUE));
					
					Justification.endEqualityStep(currentExpression);
				}

				Expression product = LPIUtil.makeProductOfMessages(factorIndexPrime, neighborsOfVMinusF, msgToV_FPrime, Expressions.TRUE);

				Justification.beginEqualityStep("computing product of incoming messages");
				Expression beingComputedUnionV_F = LPIUtil.extendBeingComputed(beingComputed, pairF_V, elseBranchProcess);
				Expression result                = elseBranchProcess.rewrite(R_prod_factor,
														LPIUtil.argForProductFactorRewriteCall(product, beingComputedUnionV_F));

				result = elseBranchProcess.rewrite(R_normalize_random_variable_condition, LPIUtil.argForNormalizeRandomVariableConditionRewriteCall(pairF_V.get(1), result));
				
				Justification.endEqualityStep(result);
			
				return result;
			}
		};
	}
}
