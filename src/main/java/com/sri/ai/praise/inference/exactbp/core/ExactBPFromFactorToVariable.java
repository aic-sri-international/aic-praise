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

import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.notNullAndEquals;
import static com.sri.ai.util.collect.NestedIterator.nestedIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.FactorNode;
import com.sri.ai.praise.inference.representation.api.Model;
import com.sri.ai.praise.inference.representation.api.Node;
import com.sri.ai.praise.inference.representation.api.Representation;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.livesets.api.LiveSet;
import com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet;

public class ExactBPFromFactorToVariable extends AbstractExactBP {
	
	public ExactBPFromFactorToVariable(Node root, Node parent, LiveSet<FactorNode> excludedFactors, RedirectingLiveSet<FactorNode> includedFactors, Representation representation, Model model) {
		super(root, parent, excludedFactors, includedFactors, representation, model);
	}

	@Override
	protected AbstractExactBP makeSubExactBP(Node subRoot, LiveSet<FactorNode> subExcludedFactors, RedirectingLiveSet<FactorNode> subIncludedFactors) {
		return new ExactBPFromVariableToFactor(subRoot, getRoot(), subExcludedFactors, subIncludedFactors, representation, model);
	}

	@Override
	public FactorNode getRoot() {
		return (FactorNode) super.getRoot();
	}
	
	@Override
	protected ArrayList<? extends Variable> makeSubsRoots() {
		ArrayList<? extends Variable> result = collectToArrayList(getRootNeighbors(), n -> n != parent);
		return result;
	}

	@Override
	public Factor function(List<Factor> incomingMessages) {
		List<? extends Variable> variablesToBeSummedOut = collectToArrayList(getRootNeighbors(), n -> ! isFreeVariable((Variable) n));
		Factor result = sumOut(variablesToBeSummedOut, getFactorNodesAtRoot(), incomingMessages);
		return result;
	}

	private boolean isFreeVariable(Variable variable) {
		boolean result = 
				notNullAndEquals(getParent(), variable)
				||
				excludedFactorNodes.thereIsAnElementSatisfying(f -> getModel().getNeighbors(f).contains(variable));
		return result;
	}

	private Factor sumOut(List<? extends Variable> variablesToBeSummedOut, List<FactorNode> factorNodesAtRoot, List<Factor> incomingMessages) {
		List<Factor> factors = mapIntoList(factorNodesAtRoot, n -> n.getFactor());
		Iterator<Factor> allFactors = nestedIterator(factors, incomingMessages);
		Factor result = representation.multiply(allFactors).sumOut(variablesToBeSummedOut);
		return result;
	}
	
	@SuppressWarnings("unchecked")
	private ArrayList<? extends Variable> getRootNeighbors() {
		return (ArrayList<? extends Variable>) getModel().getNeighbors(getRoot());
	}

	@Override
	protected List<FactorNode> getFactorNodesAtRoot() {
		return list(getRoot());
	}
}