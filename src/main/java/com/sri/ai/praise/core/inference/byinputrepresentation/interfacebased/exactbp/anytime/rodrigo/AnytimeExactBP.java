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
package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo;

import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.IdentityPolytope.identityPolytope;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.DefaultFunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.Simplex;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.computation.anytime.api.Anytime;
import com.sri.ai.util.computation.anytime.api.Approximation;
import com.sri.ai.util.computation.treecomputation.anytime.core.AbstractAnytimeTreeComputation;

/**
 * An anytime version of {@link ExactBPNode} algorithms.
 * This is implemented as a {@link AbstractAnytimeTreeComputationWithDefaultPickingOfSubs}
 * based on an {@link ExactBPNode}, which is gradually expanded.
 * <p>
 * It uses {@link Simplex} as an initial approximation,
 * and computes an approximation to the base's answer by not summing out the indices whose sub-messages
 * are simplexes (justification of this is to be found in the related publications).
 * 
 * @author braz
 *
 */
public class AnytimeExactBP<RootType,SubRootType> extends AbstractAnytimeTreeComputation<Factor> {

	@Override
	protected boolean evenOneSubWithTotalIgnoranceRendersApproximationEqualToTotalIgnorance() {
		boolean result = getBase().getRoot() instanceof Variable;
		return result;
	}

	public AnytimeExactBP(ExactBPNode<RootType,SubRootType> base) {
		super(base, new Simplex(base.getMessageVariable()));
	}

	private Iterator<? extends AnytimeExactBP<SubRootType,RootType>> subRoundRobinIterator;

	@Override
	protected void makeSubsAndIterateThemToTheirFirstApproximation() {
		super.makeSubsAndIterateThemToTheirFirstApproximation();
		subRoundRobinIterator = getSubs().iterator();
	}

	@Override
	protected AnytimeExactBP<SubRootType, RootType> pickNextSubToIterate() {
		
		if (getSubs().isEmpty()) {
			return null;
		}
		
		AnytimeExactBP<SubRootType, RootType> subWeStartedWith = getNextInSubRoundRobin();
		
		AnytimeExactBP<SubRootType, RootType> nextSubThatCanBeRefined = null; 
		AnytimeExactBP<SubRootType, RootType> currentSub = subWeStartedWith; 
		boolean cameBackToTheOneWeStartedWith = false;
		do {
			if (currentSub.hasNext()) {
				nextSubThatCanBeRefined = currentSub;
			}
			else {
				currentSub = getNextInSubRoundRobin();
				if (currentSub == subWeStartedWith) {
					cameBackToTheOneWeStartedWith = true;
				}
			}
		} while (nextSubThatCanBeRefined == null && !cameBackToTheOneWeStartedWith);
		
		return nextSubThatCanBeRefined;
	}

	private AnytimeExactBP<SubRootType, RootType> getNextInSubRoundRobin() {
		if (!subRoundRobinIterator.hasNext()) {
			subRoundRobinIterator = getSubs().iterator();
		}
		AnytimeExactBP<SubRootType, RootType> result = subRoundRobinIterator.next();
		return result;
	}

	@Override
	protected Anytime<Factor> makeAnytimeVersion(NullaryFunction<Factor> baseSub) {
		@SuppressWarnings("unchecked")
		ExactBPNode<SubRootType, RootType> baseExactBP = (ExactBPNode<SubRootType, RootType>) baseSub;
		AnytimeExactBP<SubRootType, RootType> result = new AnytimeExactBP<SubRootType,RootType>(baseExactBP);
		return result;
	}

	@Override
	@SuppressWarnings("unchecked")
	public ExactBPNode<RootType,SubRootType> getBase() {
		return (ExactBPNode<RootType,SubRootType>) super.getBase();
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public ArrayList<? extends AnytimeExactBP<SubRootType,RootType>> getSubs() {
		return (ArrayList<? extends AnytimeExactBP<SubRootType,RootType>>) super.getSubs();
	}
	
	@Override
	public Approximation<Factor> function(List<Approximation<Factor>> subsApproximations) {
		Polytope product = getProductOfAllIncomingPolytopesAndFactorAtRoot(subsApproximations);
		Collection<? extends Variable> freeVariables = product.getFreeVariables();
		List<? extends Variable> variablesSummedOut = getBase().variablesToBeSummedOut(freeVariables);
		Approximation<Factor> result = product.sumOut(variablesSummedOut);
		return result;
	}

	private Polytope getProductOfAllIncomingPolytopesAndFactorAtRoot(List<Approximation<Factor>> subsApproximations) {
		List<Polytope> polytopesToMultiply = getAllPolytopes(subsApproximations);
		Polytope result = accumulate(polytopesToMultiply, Polytope::multiply, identityPolytope());
		return result;
	}

	private List<Polytope> getAllPolytopes(List<Approximation<Factor>> subsApproximations) {
		List<Polytope> polytopesToMultiply = mapIntoList(subsApproximations, a -> (Polytope) a);
		addFactorAtRootPolytope(polytopesToMultiply);
		return polytopesToMultiply;
	}

	private void addFactorAtRootPolytope(List<Polytope> polytopesToMultiply) {
		FunctionConvexHull singletonDefaultFunctionConvexHullAtRoot = getFactorAtRootPolytope();
		polytopesToMultiply.add(singletonDefaultFunctionConvexHullAtRoot);
	}

	private FunctionConvexHull getFactorAtRootPolytope() {
		Factor factorAtRoot = Factor.multiply(getBase().getFactorsAtRoot());
		FunctionConvexHull singletonDefaultFunctionConvexHullAtRoot = new DefaultFunctionConvexHull(list(), factorAtRoot);
		return singletonDefaultFunctionConvexHullAtRoot;
	}

	@Override
	public String toString() {
		return "Anytime " + getBase();
	}
}