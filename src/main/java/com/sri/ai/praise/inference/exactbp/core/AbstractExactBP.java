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
package com.sri.ai.praise.inference.exactbp.core;

import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.notNullAndEquals;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.collect.NestedIterator.nestedIterator;
import static com.sri.ai.util.livesets.core.lazy.memoryless.ExtensionalLiveSet.liveSet;
import static com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet.redirectingTo;
import static com.sri.ai.util.livesets.core.lazy.memoryless.Union.union;
import static com.sri.ai.util.livesets.core.lazy.memoryless.Union.unionOfAllButTheOneAt;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.praise.inference.exactbp.api.ExactBP;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.FactorNetwork;
import com.sri.ai.praise.inference.representation.api.Representation;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.livesets.api.LiveSet;
import com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet;

/**
 * Abstract implementation of Exact BP gathering the common functionality
 * of Exact BP rooted in variables and in factors.
 * <p>
 * Each node of Exact BP must provide its sub-Exact BPs.
 * This is done with the method {@link #makeSubExactBP(SubRootType, LiveSet, RedirectingLiveSet)},
 * which is left abstract since variable-rooted Exact BPs will want to create
 * subs which are factor-rooted Exact BPs and vice-versa,
 * so it is not a common functionality.
 * <p>
 * Method {@link #function(List)} is also left unspecified at this level
 * because variable-rooted Exact BPs will multiple messages
 * while factor-rooted ones will sum out certain variables of its factor times incoming messages.
 * <p>
 * Other than that, both types must keep track of
 * included factor nodes (those already assigned to be in its branch)
 * and excluded factor nodes (those in their siblings or parent),
 * so this is implemented at this level.
 * 
 * @param <RootType> the type ({@link Fator} or {@link Variable}) of the root node of the tree.
 * @param <SubRootType> the type ({@link Fator} or {@link Variable}) of the root node of the subs. Must be the opposite of RootType.
 *
 * @author braz
 *
 */
public abstract class AbstractExactBP<RootType,SubRootType> implements ExactBP<RootType,SubRootType> {
	
	/**
	 * The factors residing at the root; typically the root itself if it is a factor, and an empty list otherwise.
	 */
	protected abstract List<Factor> getFactorsAtRoot();

	/**
	 * An abstract method for extensions to define how to create their subs.
	 * @param subRoot
	 * @param subExcludedFactors
	 * @param subIncludedFactors
	 * @return
	 */
	protected abstract ExactBP<SubRootType,RootType> makeSubExactBP(SubRootType subRoot, LiveSet<Factor> subExcludedFactors, RedirectingLiveSet<Factor> subIncludedFactors);

	/**
	 * An abstract method for extensions to define what are their sub's roots.
	 */
	protected abstract ArrayList<? extends SubRootType> makeSubsRoots();

	protected RootType root;
	protected SubRootType parent;
	
	protected ArrayList<ExactBP<SubRootType,RootType>> subs;

	final protected LiveSet<Factor> excludedFactors;
	final protected RedirectingLiveSet<Factor> includedFactors;
	// we must use redirecting live sets for included factor nodes because they initially are just the root factor (if the root is a factor),
	// but then later must be changed to include the union of sub-ExactBPs.
	// However, the same instance must be kept because the parent's included and excluded factor nodes live sets
	// are wired to it.
	// The solution is to have a redirecting live set instance that can be redirected.
	
	protected Representation representation;
	protected FactorNetwork factorNetwork;
	
	protected AbstractExactBP(RootType root, SubRootType parent, LiveSet<Factor> excludedFactors, RedirectingLiveSet<Factor> includedFactors, Representation representation, FactorNetwork factorNetwork) {
		this.root = root;
		this.parent = parent;
		this.subs = null;
		this.excludedFactors = excludedFactors;
		this.includedFactors = includedFactors;
		this.representation = representation;
		this.factorNetwork = factorNetwork;
	}
	
	@Override
	public ArrayList<ExactBP<SubRootType,RootType>> getSubs() {
		if (subs == null) {
			makeSubs();
		}
		return subs;
	}
	
	private void makeSubs() {
		// This method creates the sub-ExactBPs.
		// First, it needs to create the included factor live sets for these subs,
		// because that is required in the constructor of ExactBPs.
		ArrayList<? extends SubRootType> subsRoots = makeSubsRoots();
		ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors = makeInitialSubsIncludedFactors(subsRoots);
		makeSubsFromTheirIncludedFactors(subsRoots, subsIncludedFactors);
	}

	private ArrayList<RedirectingLiveSet<Factor>> makeInitialSubsIncludedFactors(ArrayList<? extends SubRootType> subsRoots) {
		ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors =
				mapIntoArrayList(subsRoots, s -> makeInitialSubIncludedFactors(s));
		redirectRootIncludedFactorsToUnionOfSubsIncludedFactorsAndFactorsAtRoot(subsIncludedFactors);
		return subsIncludedFactors;
	}
	
	private RedirectingLiveSet<Factor> makeInitialSubIncludedFactors(SubRootType subRoot) {
		List<Factor> subIncludedFactors;
		if (subRoot instanceof Factor) {
			subIncludedFactors = list((Factor) subRoot);
		}
		else {
			subIncludedFactors = list();
		}
		RedirectingLiveSet<Factor> result = redirectingTo(liveSet(subIncludedFactors));
		return result;
	}

	private void redirectRootIncludedFactorsToUnionOfSubsIncludedFactorsAndFactorsAtRoot(List<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		LiveSet<Factor> unionOfSubsIncludedFactorsAndFactorsAtRoot = union(subsIncludedFactors).union(getFactorsAtRoot());
		includedFactors.redirectTo(unionOfSubsIncludedFactorsAndFactorsAtRoot);
	}

	private void makeSubsFromTheirIncludedFactors(ArrayList<? extends SubRootType> subsRoots, ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		subs = new ArrayList<>(subsRoots.size());
		int subIndex = 0;
		for (SubRootType subRoot : subsRoots) {
			ExactBP<SubRootType,RootType> sub = makeSubFromItsIncludedFactors(subRoot, subIndex, subsIncludedFactors);
			subs.add(sub);
			subIndex++;
		}
	}

	private ExactBP<SubRootType,RootType> makeSubFromItsIncludedFactors(SubRootType subRoot, int subIndex, ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		RedirectingLiveSet<Factor> subIncludedFactors = subsIncludedFactors.get(subIndex);
		LiveSet<Factor> subExcludedFactors = excludedFactorsForSubAt(subIndex, subsIncludedFactors);
		ExactBP<SubRootType,RootType> sub = makeSubExactBP(subRoot, subExcludedFactors, subIncludedFactors);
		return sub;
	}

	private LiveSet<Factor> excludedFactorsForSubAt(int subIndex, List<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		LiveSet<Factor> unionOfSiblingsExcludedFactors = unionOfAllButTheOneAt(subsIncludedFactors, subIndex);
		LiveSet<Factor> excludedFactorsUnionSiblingsExcludedFactors = excludedFactors.union(unionOfSiblingsExcludedFactors);
		LiveSet<Factor> result = excludedFactorsUnionSiblingsExcludedFactors.union(getFactorsAtRoot());
		return result;
	}

	@Override
	public Factor function(List<Factor> incomingMessages) {
		Factor product = computeProductOfFactorsAtRootAndIncomingMessages(incomingMessages);
		Factor result = keepOnlyFreeVariablesInResultFactor(product);
		return result;
	}

	private Factor computeProductOfFactorsAtRootAndIncomingMessages(List<Factor> incomingMessages) {
		Iterator<Factor> allFactors = nestedIterator(getFactorsAtRoot(), incomingMessages);
		Factor product = representation.multiply(allFactors);
		return product;
	}

	private Factor keepOnlyFreeVariablesInResultFactor(Factor factor) {
		List<? extends Variable> variablesToBeSummedOut = getNonFreeVariables(factor);
		Factor result = factor.sumOut(variablesToBeSummedOut);
		printTracingInformation(factor, variablesToBeSummedOut, result);
		return result;
	}

	private void printTracingInformation(Factor factor, List<? extends Variable> variablesToBeSummedOut, Factor result) {
		printTracingInformationHeader();
		printTracingInformationBody(factor, variablesToBeSummedOut, result);
	}

	private void printTracingInformationHeader() {
		if (getParent() == null) {
			println("Final message on " + getRoot());
		}
		else {
			println("Message from " + getRoot() + " to " + getParent());
		}
	}

	private void printTracingInformationBody(Factor factor, List<? extends Variable> variablesToBeSummedOut, Factor result) {
		println("Received total product " + factor);
		String eliminatedString = variablesToBeSummedOut.isEmpty()? "no variables" : join(variablesToBeSummedOut);
		println("Eliminating " + eliminatedString + " from " + factor + " ---> " + result);
		println("Sending " + result);
	}

	private List<? extends Variable> getNonFreeVariables(Factor factor) {
		List<? extends Variable> allVariablesInFactors = factor.getVariables();
		List<? extends Variable> variablesToBeSummedOut = collectToList(allVariablesInFactors, n -> ! isFreeVariable((Variable) n));
		return variablesToBeSummedOut;
	}

	private boolean isFreeVariable(Variable variable) {
		boolean result = 
				isEqualToRoot(variable)
				||
				isEqualToParentIfThereIsOne(variable)
				||
				isInExternalFactors(variable);
		return result;
	}

	private boolean isEqualToRoot(Variable variable) {
		boolean result = getRoot().equals(variable);
		return result;
	}

	private boolean isEqualToParentIfThereIsOne(Variable variable) {
		boolean result = notNullAndEquals(getParent(), variable);
		return result;
	}

	private boolean isInExternalFactors(Variable variable) {
		boolean result = excludedFactors.thereIsAnElementSatisfying(f -> f.contains(variable));
		return result;
	}

	@Override
	public RootType getRoot() {
		return root;
	}

	@Override
	public SubRootType getParent() {
		return parent;
	}

	public FactorNetwork getFactorNetwork() {
		return factorNetwork;
	}
}