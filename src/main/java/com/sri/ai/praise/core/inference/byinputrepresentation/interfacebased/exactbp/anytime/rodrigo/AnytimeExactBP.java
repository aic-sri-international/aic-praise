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
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.union;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.DefaultFunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.Polytopes;
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

	///////////////// DATA MEMBERS
	
	private Iterator<? extends AnytimeExactBP<SubRootType,RootType>> subRoundRobinIterator;

	///////////////// CONSTRUCTOR
	
	public AnytimeExactBP(ExactBPNode<RootType,SubRootType> base) {
		super(base, new Simplex(base.getMessageVariable()));
	}

	///////////////// IMPLEMENTATIONS
	
	@Override
	protected boolean evenOneSubWithTotalIgnoranceRendersApproximationEqualToTotalIgnorance() {
		boolean result = getBase().getRoot() instanceof Variable;
		return result;
	}

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
		
//		var initialExcludedVariables = getCurrentlyExcludedVariables_DEBUG();
		
		Polytope product = getProductOfAllIncomingPolytopesAndFactorAtRoot(subsApproximations);
		Collection<? extends Variable> summandVariables = product.getFreeVariables();
		List<? extends Variable> variablesToBeSummedOut = getBase().variablesToBeSummedOut(summandVariables);
		
//		println("AnytimeExactBP: computing which of these should be summed out: " + summandVariables);
//		println("AnytimeExactBP: These are the ones that should be summed out : " + variablesToBeSummedOut);
//		println("AnytimeExactBP: summand : " + product);
//		println("AnytimeExactBP: excludedVariables : " + getCurrentlyExcludedVariables_DEBUG());
		
		Approximation<Factor> result = product.sumOut(variablesToBeSummedOut);

//		var finalExcludedVariables = getCurrentlyExcludedVariables_DEBUG();
//		
//		Variable indexThatShouldBeFree = violatingIndex_DEBUG(result);
//		
//		if (indexThatShouldBeFree != null) {
//			println("AnytimeExactBP: found index that is free variable but is in polytope's index: " + indexThatShouldBeFree);
//			println("Initial excluded variables: " + initialExcludedVariables);
//			println("Final   excluded variables: " + finalExcludedVariables);
//			println("Excluded variables remained the same: " + (initialExcludedVariables.equals(finalExcludedVariables)));
//			println("Polytope: "  + result);
//			System.exit(-1);
//		}
		
		return result;
	}

	@Override
	protected Approximation<Factor> getCurrentApproximationForSub(Anytime<Factor> sub) {
		
//		var excludedVariablesBeforeSub = getCurrentlyExcludedVariables_DEBUG();
//		
//		println("AnytimeExactBP.getCurrentApproximationForSub: going to compute sub " + sub);
		Approximation<Factor> subCurrentApproximation = super.getCurrentApproximationForSub(sub);
//		println("AnytimeExactBP.getCurrentApproximationForSub: done computing sub, result = " + subCurrentApproximation);
//		
//		var excludedVariablesAfterSub = getCurrentlyExcludedVariables_DEBUG();
//
//		Variable indexThatShouldBeFree = violatingIndex_DEBUG(subCurrentApproximation);
//		
//		if (indexThatShouldBeFree != null) {
//			println("AnytimeExactBP.getCurrentApproximationForSub: found index that is free variable but is in sub's result's index: " + indexThatShouldBeFree);
//			println("Sub: " + sub);
//			println("Excluded variables before sub: " + excludedVariablesBeforeSub);
//			println("Excluded variables after  sub: " + excludedVariablesAfterSub);
//			println("Excluded variables remained the same: " + (excludedVariablesBeforeSub.equals(excludedVariablesAfterSub)));
//			println("Sub's result: "  + subCurrentApproximation);
//			System.exit(-1);
//		}
		
		return subCurrentApproximation;
	}

	private Set<? extends Variable> getCurrentlyExcludedVariables_DEBUG() {
		return union(getBase().getExcludedFactors().getCurrentElements(), Factor::getVariables);
	}

	private Variable violatingIndex_DEBUG(Approximation<Factor> approximation) {
		return getFirst(getIndices_DEBUG((Polytope)approximation), getBase()::isFreeVariable);
	}
	
	private Collection<? extends Variable> getIndices_DEBUG(Polytope polytope) {
		return union(mapIntoList(polytope.getAtomicPolytopes(), a -> a instanceof Simplex? list() : ((FunctionConvexHull)a).getIndices()));
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
	public void updateCurrentApproximationGivenThatExternalContextHasChangedButWithoutIteratingItself() {
		// The external context is the excluded factors, whose variables must be considered free variables in this {@link AnytimeExactBP} object.
		// If a variable V was not free when the current approximation was computed, there are three possible cases:
		// 1) it has either not been reached so far by this branch,
		// 2) it has but became irrelevant for the current bound, or
		// 3) it has and is an index in one of the function convex hulls present in the current approximation.
		// If either of the two first cases is true, the current approximation remains valid.
		// If 3) is true, the current approximation can be incrementally used to compute the updated one by simple removing the newly free variable
		// from indices. It will then be present in the function convex hull's factor as a free variable.

		Polytope polytope = (Polytope) getCurrentApproximation();
		Polytope updated = Polytopes.mapAtomicPolytopes(polytope, this::removeNewlyFreeIndices);
		
		setCurrentApproximation(updated);
//		
//		if ( ! polytope.toString().equals(updated.toString())) {
//			println();
//			println("Excluded: " + getCurrentlyExcludedVariables_DEBUG());
//			println("Original: " + polytope);
//			println(" Updated: " + updated);
//		}
	}

	private AtomicPolytope removeNewlyFreeIndices(AtomicPolytope atomicPolytope) {
		return Polytopes.removeIndicesSatisfying(atomicPolytope, getBase()::isFreeVariable);
	}

	@Override
	public String toString() {
		return "Anytime " + getBase();
	}
}