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
package com.sri.ai.praise.inference.anytime;

import static com.sri.ai.praise.inference.anytime.livesets.core.lazymemoryless.Union.union;
import static com.sri.ai.praise.inference.anytime.livesets.core.lazymemoryless.Union.unionOfAllButTheOneAt;
import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.grinder.library.bounds.Bound;
import com.sri.ai.praise.inference.anytime.livesets.api.LiveSet;
import com.sri.ai.praise.inference.anytime.livesets.core.lazymemoryless.RedirectingLiveSet;

public abstract class AbstractExactBP
	extends AbstractFunctionOnIterators<Bound>
	implements ExactBP {
	
	protected abstract AbstractExactBP makeSubExactBP(Node subRoot, LiveSet<Factor> subExcludedFactors, RedirectingLiveSet<Factor> subIncludedFactors);

	protected Node root;
	protected Node parent;
	
	protected Bound bound;

	protected List<ExactBP> subs;

	final protected LiveSet<Factor> excludedFactors;
	final protected RedirectingLiveSet<Factor> includedFactors;
	// we must use redirecting live sets for included factors because they initially are just the root factor (if the root is a factor),
	// but then later must be changed to include the union of sub-ExactBPs.
	// However, the same instance must be kept because the parent's included and excluded factors live sets
	// are wired to it.
	// The solution is to have a redirecting live set instance that can be redirected.
	
	public AbstractExactBP(Node root, Node parent, LiveSet<Factor> excludedFactors, RedirectingLiveSet<Factor> includedFactors) {
		this.root = root;
		this.parent = parent;
		this.bound = null; // TODO: replace by simplex
		this.subs = null;
		this.excludedFactors = excludedFactors;
		this.includedFactors = includedFactors;
	}
	
	protected List<ExactBP> getSubs() {
		if (subs == null) {
			makeSubs();
		}
		return subs;
	}
	
	private void makeSubs() {
		ArrayList<Node> subsRoots = makeSubsRoots();
		ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors = 
				mapIntoArrayList(subsRoots, Node::makeInitialIncludedFactors);
		makeSubsFromTheirIncludedFactors(subsRoots, subsIncludedFactors);
		setIncludedFactorsToUnionOfSubsIncludedFactors(subsIncludedFactors);
	}

	private void makeSubsFromTheirIncludedFactors(ArrayList<Node> subsRoots, ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		int subIndex = 0;
		for (Node subRoot : subsRoots) {
			AbstractExactBP sub = makeSubFromTheirIncludedFactors(subRoot, subIndex, subsIncludedFactors);
			subs.add(sub);
			subIndex++;
		}
	}

	private AbstractExactBP makeSubFromTheirIncludedFactors(Node subRoot, int subIndex, ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		RedirectingLiveSet<Factor> subIncludedFactors = subsIncludedFactors.get(subIndex);
		LiveSet<Factor> subExcludedFactors = excludedFactorsForSubAt(subIndex, subsIncludedFactors);
		AbstractExactBP sub = makeSubExactBP(subRoot, subExcludedFactors, subIncludedFactors);
		return sub;
	}

	private ArrayList<Node> makeSubsRoots() {
		ArrayList<Node> result = collectToArrayList(root.getNeighbors(), n -> n != parent);
		return result;
	}

	private LiveSet<Factor> excludedFactorsForSubAt(int subIndex, List<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		LiveSet<Factor> unionOfSiblingsExcludedFactors = unionOfAllButTheOneAt(subsIncludedFactors, subIndex);
		LiveSet<Factor> excludedFactorsUnionSiblingsExcludedFactors = excludedFactors.union(unionOfSiblingsExcludedFactors);
		LiveSet<Factor> result = excludedFactorsUnionSiblingsExcludedFactors.union(root.getFactors());
		return result;
	}

	private void setIncludedFactorsToUnionOfSubsIncludedFactors(List<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		includedFactors.redirectTo(union(subsIncludedFactors).union(root.getFactors()));
	}

	@Override
	public ExactBP pickNextArgumentIterator() {
		ExactBP result = getFirstSatisfyingPredicateOrNull(getSubs(), Iterator::hasNext);
		return result;
	}

	public Bound getBound() {
		return bound;
	}

	public Node getRoot() {
		return root;
	}

	public List<Factor> getFactors() {
		return getRoot().getFactors();
	}
}