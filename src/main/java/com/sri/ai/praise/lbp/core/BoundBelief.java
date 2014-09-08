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
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;

/**
 * Default implementation of {@link LBPRewriter#R_bound_belief}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class BoundBelief extends AbstractLBPHierarchicalRewriter implements LBPRewriter {

	public BoundBelief() {	
	}
	
	@Override
	public String getName() {
		return R_bound_belief;
	}
	
	/**
	 * @see LBPRewriter#R_bound_belief
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// R_bound_belief(E, t)
		// Extract E and t
		if (!(Tuple.isTuple(expression) && Tuple.size(expression) == 2)) {
			throw new IllegalArgumentException("Input expression should be a tuple of the form: (E, t) was "+expression);
		}
		Expression belief   = Tuple.get(expression, 0);
		Expression timestep = Tuple.get(expression, 1);
		
		// Assert input arguments
		LPIUtil.assertBeliefOk(belief, process);
		
		Expression randomVariable = belief.get(0);
		
		Expression previousRandomVariableBeingNormalized = LPIUtil.setRandomVariableBeingNormalizedAndReturnPreviousOne(randomVariable, process);
		
		process = LPIUtil.extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(randomVariable, process);
		process = LPIUtil.extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(Tuple.make(Model.getRewritingProcessesModel(process).getParfactorsDeclaration().getParfactors()), process);

		Trace.log("beingComputed    <- empty set");
		Expression beingComputed = LPIUtil.createNewBeingComputedExpression();
		Trace.log("belief_expansion <- R_bound_prod_factor(prod_F in R_neigh_v(Neigh(V)) m_V<-F, beingComputed)");

		// prod_F in R_neigh_v(Neigh(V)) m_V<-F
		Expression factorIndex = Expressions.makeUniqueVariable("F", randomVariable, process);

		if (Justification.isEnabled()) {
			Justification.log(Expressions.apply(LPIUtil.FUNCTOR_BELIEF, randomVariable, Model.getModelDefinition(process)));

			Justification.beginEqualityStep("lifted bound belief");

			Justification.log(belief); // detailed justification

			Justification.beginEqualityStep("definition of bound  belief");

			Expression expandedBelief =
					Expressions.apply(
							LPIUtil.FUNCTOR_NORMALIZE,
							LPIUtil.makeProductOfMessages(
									factorIndex,
									Expressions.apply(LPIUtil.FUNCTOR_NEIGHBOR, randomVariable),
									Expressions.apply(LPIUtil.FUNCTOR_MSG_TO_FROM, randomVariable, factorIndex),
									Expressions.TRUE));

			Justification.endEqualityStep(expandedBelief);
		}

		// R_neigh_v(Neigh(V))
		Justification.beginEqualityStep("neighbors of random variable");

		Expression neighV          = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_NEIGHBOR, randomVariable);
		Expression rewriteOfNeighV = process.rewrite(R_neigh_v, neighV);

		Expression msgToV_F = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_MSG_TO_FROM, randomVariable, factorIndex);
		Expression product = LPIUtil.makeProductOfMessages(factorIndex, rewriteOfNeighV, msgToV_F, Expressions.TRUE);

		if (Justification.isEnabled()) {
			Justification.endEqualityStep(Expressions.apply(LPIUtil.FUNCTOR_NORMALIZE, product));
		}

		// R_prod_factor(prod_F in R_neigh_v(Neigh(V)) m_V<-F)
		Justification.beginEqualityStep("product of messages over factors");

		Expression beliefExpansion = process.rewrite(R_bound_prod_factor, Tuple.make(LPIUtil.argForProductFactorRewriteCall(product, beingComputed), timestep));

		if (Justification.isEnabled()) {
			Justification.endEqualityStep(Expressions.apply(LPIUtil.FUNCTOR_NORMALIZE, beliefExpansion));
		}
		
		Trace.log("belief_expansion <- R_complete_normalize(belief_expansion)");
		beliefExpansion = process.rewrite(R_complete_normalize, beliefExpansion);
		
		Expression result = null;
		if (LPIUtil.containsBoundExpressions(beliefExpansion)) {
			Trace.log("    return beliefExpansion");
			result = beliefExpansion;
		}
		else {
			Trace.log("    return R_normalize_message(V, belief_expansion)");
			Justification.beginEqualityStep("normalization");
			Expression normalizedBeliefExpansion = process.rewrite(R_normalize_message, LPIUtil.argForNormalizeRewriteCall(randomVariable, beliefExpansion));
			Justification.endEqualityStep(normalizedBeliefExpansion);
			
			result = normalizedBeliefExpansion;
		}	
		
		LPIUtil.restorePreviousRandomVariableBeingNormalized(previousRandomVariableBeingNormalized, process);

		Justification.endEqualityStep(result);
		
		return result;
	}
}
