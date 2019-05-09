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
package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core;

import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.computation.treecomputation.api.TreeComputationEvaluator;
import com.sri.ai.util.livesets.api.LiveSet;
import com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet;

public class ExactBPNodeFromFactorToVariable extends AbstractExactBPNode<Factor,Variable> {
	
	protected ExactBPNodeFromFactorToVariable(
			Factor root, 
			Variable parent, 
			LiveSet<Factor> excludedFactors, 
			RedirectingLiveSet<Factor> includedFactors, 
			FactorNetwork model, 
			Predicate<Variable> isParameterPredicate) {
		
		super(root, parent, excludedFactors, includedFactors, model, isParameterPredicate);
	}

	@Override
	protected ExactBPNode<Variable,Factor> makeSubExactBP(Variable subRoot, LiveSet<Factor> subExcludedFactors, RedirectingLiveSet<Factor> subIncludedFactors) {
		return new ExactBPNodeFromVariableToFactor(subRoot, getRoot(), subExcludedFactors, subIncludedFactors, factorNetwork, isParameterPredicate);
	}

	@Override
	protected ArrayList<? extends Variable> makeSubsRoots() {
		ArrayList<? extends Variable> result = collectToArrayList(getRootNeighbors(), n -> ! n.equals(parent));
		return result;
	}

	protected Collection<? extends Variable> getRootNeighbors() {
		return getFactorNetwork().getNeighbors(getRoot());
	}

	@Override
	public List<Factor> getFactorsAtRoot() {
		return list(getRoot());
	}

	@Override
	public Variable getMessageVariable() {
		return getParent();
	}

	@Override
	public TreeComputationEvaluator<Factor> makeNewEvaluator() {
		return new LazyExactBPNodeFromFactorToVariableEvaluator(
				this::getFactorsAtRoot, 
				this::determinedVariablesToBeSummedOut, 
				(variablesToBeSummedOut, product) -> sumOutWithBookkeeping(variablesToBeSummedOut, product));
	}
}