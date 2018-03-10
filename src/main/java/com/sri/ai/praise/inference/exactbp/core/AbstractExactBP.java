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

import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.livesets.core.lazy.memoryless.ExtensionalLiveSet.liveSet;
import static com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet.redirectingTo;
import static com.sri.ai.util.livesets.core.lazy.memoryless.Union.union;
import static com.sri.ai.util.livesets.core.lazy.memoryless.Union.unionOfAllButTheOneAt;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.inference.exactbp.api.ExactBP;
import com.sri.ai.praise.inference.representation.api.FactorNode;
import com.sri.ai.praise.inference.representation.api.Model;
import com.sri.ai.praise.inference.representation.api.Node;
import com.sri.ai.praise.inference.representation.api.Representation;
import com.sri.ai.praise.inference.representation.expression.AbstractExpressionNode;
import com.sri.ai.util.livesets.api.LiveSet;
import com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet;

/**
 * Abstract implementation of Exact BP gathering the common functionality
 * of Exact BP rooted in variables and in factors.
 * <p>
 * Each node of Exact BP must provide its sub-Exact BPs.
 * This is done with the method {@link #makeSubExactBP(AbstractExpressionNode, LiveSet, RedirectingLiveSet)},
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
 * @author braz
 *
 */
public abstract class AbstractExactBP implements ExactBP {
	
	protected abstract List<FactorNode> getFactorNodesAtRoot();

	protected abstract ExactBP makeSubExactBP(Node subRoot, LiveSet<FactorNode> subExcludedFactorNodes, RedirectingLiveSet<FactorNode> subIncludedFactorNodes);

	protected abstract ArrayList<? extends Node> makeSubsRoots();

	protected Node root;
	protected Node parent;
	
	protected ArrayList<ExactBP> subs;

	final protected LiveSet<FactorNode> excludedFactorNodes;
	final protected RedirectingLiveSet<FactorNode> includedFactorNodes;
	// we must use redirecting live sets for included factor nodes because they initially are just the root factor (if the root is a factor),
	// but then later must be changed to include the union of sub-ExactBPs.
	// However, the same instance must be kept because the parent's included and excluded factor nodes live sets
	// are wired to it.
	// The solution is to have a redirecting live set instance that can be redirected.
	
	protected Representation representation;
	protected Model model;
	
	public AbstractExactBP(Node root, Node parent, LiveSet<FactorNode> excludedFactorNodes, RedirectingLiveSet<FactorNode> includedFactorNodes, Representation representation, Model model) {
		this.root = root;
		this.parent = parent;
		this.subs = null;
		this.excludedFactorNodes = excludedFactorNodes;
		this.includedFactorNodes = includedFactorNodes;
		this.representation = representation;
		this.model = model;
	}
	
	@Override
	public ArrayList<ExactBP> getSubs() {
		if (subs == null) {
			makeSubs();
		}
		return subs;
	}
	
	private void makeSubs() {
		// This method creates the sub-ExactBPs.
		// First, it needs to create the included factor live sets for these subs,
		// because that is required in the constructor of ExactBPs.
		ArrayList<? extends Node> subsRoots = makeSubsRoots();
		ArrayList<RedirectingLiveSet<FactorNode>> subsIncludedFactorNodes = makeInitialSubsIncludedFactorNodes(subsRoots.size());
		makeSubsFromTheirIncludedFactorNodes(subsRoots, subsIncludedFactorNodes);
	}

	private ArrayList<RedirectingLiveSet<FactorNode>> makeInitialSubsIncludedFactorNodes(int numberOfSubs) {
		ArrayList<RedirectingLiveSet<FactorNode>> subsIncludedFactorNodes =
				fill(numberOfSubs, () -> makeInitialSubIncludedFactorNodes());
		redirectRootIncludedFactorNodesToUnionOfSubsIncludedFactorNodesAndFactorNodesAtRoot(subsIncludedFactorNodes);
		return subsIncludedFactorNodes;
	}

	private RedirectingLiveSet<FactorNode> makeInitialSubIncludedFactorNodes() {
		return redirectingTo(liveSet(list()));
	}

	private void redirectRootIncludedFactorNodesToUnionOfSubsIncludedFactorNodesAndFactorNodesAtRoot(List<RedirectingLiveSet<FactorNode>> subsIncludedFactorNodes) {
		LiveSet<FactorNode> unionOfSubsIncludedFactorNodesAndFactorsAtRoot = union(subsIncludedFactorNodes).union(getFactorNodesAtRoot());
		includedFactorNodes.redirectTo(unionOfSubsIncludedFactorNodesAndFactorsAtRoot);
	}

	private void makeSubsFromTheirIncludedFactorNodes(ArrayList<? extends Node> subsRoots, ArrayList<RedirectingLiveSet<FactorNode>> subsIncludedFactorNodes) {
		int subIndex = 0;
		for (Node subRoot : subsRoots) {
			ExactBP sub = makeSubFromItsIncludedFactorNodes(subRoot, subIndex, subsIncludedFactorNodes);
			subs.add(sub);
			subIndex++;
		}
	}

	private ExactBP makeSubFromItsIncludedFactorNodes(Node subRoot, int subIndex, ArrayList<RedirectingLiveSet<FactorNode>> subsIncludedFactorNodes) {
		RedirectingLiveSet<FactorNode> subIncludedFactorNodes = subsIncludedFactorNodes.get(subIndex);
		LiveSet<FactorNode> subExcludedFactorNodes = excludedFactorNodesForSubAt(subIndex, subsIncludedFactorNodes);
		ExactBP sub = makeSubExactBP(subRoot, subExcludedFactorNodes, subIncludedFactorNodes);
		return sub;
	}

	private LiveSet<FactorNode> excludedFactorNodesForSubAt(int subIndex, List<RedirectingLiveSet<FactorNode>> subsIncludedFactorNodes) {
		LiveSet<FactorNode> unionOfSiblingsExcludedFactorNodes = unionOfAllButTheOneAt(subsIncludedFactorNodes, subIndex);
		LiveSet<FactorNode> excludedFactorNodesUnionSiblingsExcludedFactorNodes = excludedFactorNodes.union(unionOfSiblingsExcludedFactorNodes);
		LiveSet<FactorNode> result = excludedFactorNodesUnionSiblingsExcludedFactorNodes.union(getFactorNodesAtRoot());
		return result;
	}

	@Override
	public Node getRoot() {
		return root;
	}

	@Override
	public Node getParent() {
		return parent;
	}

	public Model getModel() {
		return model;
	}
}