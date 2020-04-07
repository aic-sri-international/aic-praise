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

import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;
import static com.sri.ai.util.livesets.core.lazy.memoryless.ExtensionalLiveSet.liveSet;
import static com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet.redirectingTo;
import static com.sri.ai.util.livesets.core.lazy.memoryless.Union.union;
import static com.sri.ai.util.livesets.core.lazy.memoryless.Union.unionOfAllButTheOneAt;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.PredicateIterator;
import com.sri.ai.util.livesets.api.LiveSet;
import com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet;

/**
 * Abstract implementation of Exact BP gathering the common functionality
 * of Exact BP rooted in indices and in factors.
 * <p>
 * Each node of Exact BP must provide its sub-Exact BPs.
 * This is done with the method {@link #makeSubExactBP(SubRootType, LiveSet, RedirectingLiveSet)},
 * which is left abstract since variable-rooted Exact BPs will want to create
 * subs which are factor-rooted Exact BPs and vice-versa,
 * so it is not a common functionality.
 * The same holds with {@link #getFactorsAtRoot()}.
 * <p>
 * Other than that, both types must keep track of
 * included factor nodes (those already assigned to be in its branch)
 * and excluded factor nodes (those in their siblings or parent),
 * so this is implemented at this level.
 * 
 * @param <RootType> the type ({@link Factor} or {@link Variable}) of the root node of the tree.
 * @param <SubRootType> the type ({@link Factor} or {@link Variable}) of the root node of the subs. Must be the opposite of RootType.
 *
 * @author braz
 *
 */
public abstract class AbstractExactBPNode<RootType,SubRootType> implements ExactBPNode<RootType,SubRootType> {

	// Some definitions:
	// Excluded factor: factor appearing outside this branch
	// External variable: a variable appearing outside this branch (even if it also appears inside)
	// Free variable: a variable that is either external to this branch or its root if it is a variable.
	// Included variable: a variable that appears in this branch as either a node or in a factor node.
	// Blanket: the set of included variables that are free.
	
	public static boolean debug = false;

	////////////////////////////// ABSTRACT METHODS
	
	@Override
	public abstract List<? extends Factor> getFactorsAtRoot();

	/**
	 * An abstract method for extensions to define what are their sub's roots.
	 */
	protected abstract ArrayList<? extends SubRootType> makeSubsRoots();

	/**
	 * An abstract method for extensions to define how to create their subs.
	 * @param subRoot
	 * @param subExcludedFactors
	 * @param subIncludedFactors
	 * @return
	 */
	protected abstract 
	ExactBPNode<SubRootType,RootType> 
	makeSubExactBP(
			SubRootType subRoot, 
			LiveSet<Factor> subExcludedFactors, 
			RedirectingLiveSet<Factor> subIncludedFactors);

	/////////////////////// DATA MEMBERS
	
	protected RootType root;
	protected SubRootType parent;
	protected ExactBPNode<SubRootType, RootType> parentNode;
	
	protected ArrayList<ExactBPNode<SubRootType,RootType>> subs;

	final protected LiveSet<Factor> excludedFactors;
	final protected RedirectingLiveSet<Factor> includedFactors;
	// we must use redirecting live sets for included factor nodes because they initially are just the root factor (if the root is a factor),
	// but then later must be updated to a live set defined as the union of sub-ExactBPs live sets
	// (defining it right away as this union is not possible because at first these sub-live sets have not been created yet).
	// Because live sets are immutable Java objects, this would mean setting this data member to a new live set instance.
	// However, this would break the parent's live set, which is defined in terms of the original includedFactor instance.
	// The solution is to have a redirecting live set instance that is always the same but that that can be redirected to a new instance when needed.
	
	protected FactorNetwork factorNetwork;
	
	protected Predicate<Variable> isParameterPredicate;
	
	///////////////////// CONSTRUCTOR
	
	protected AbstractExactBPNode(
			RootType root, 
			SubRootType parent, // TODO: redundant now that we have parentNode, remove it
			ExactBPNode<SubRootType, RootType> parentNode,
			LiveSet<Factor> excludedFactors, 
			RedirectingLiveSet<Factor> includedFactors, 
			FactorNetwork factorNetwork, 
			Predicate<Variable> isParameterPredicate) {
		
		this.root = root;
		this.parent = parent;
		this.parentNode = parentNode;
		this.subs = null;
		this.excludedFactors = excludedFactors;
		this.includedFactors = includedFactors;
		this.factorNetwork = factorNetwork;
		this.isParameterPredicate = isParameterPredicate;
	}

	//////////////////// APPLY
	
	@Override
	public Factor apply() {
		return
				explanationBlock("Computing message to ", getRoot(), code(() -> {
					return ExactBPNode.super.apply();
				}), "Message to ", getRoot(), " is ", RESULT);
	}
	
	//////////////////// ACCESS TO FACTORS
	
	@Override
	public LiveSet<Factor> getExcludedFactors() {
		return excludedFactors;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Iterator<? extends Factor> getIncludedFactors() {
		List<Object> nestedListOfFactors = list();
		if (root instanceof Factor) {
			nestedListOfFactors.add(root);
		}
		if (hasMadeSubsYet()) {
			nestedListOfFactors.addAll(mapIntoList(getSubs(), s -> (NullaryFunction) () -> s.getIncludedFactors()));
		}
		return new NestedIterator(nestedListOfFactors);
	}

	//////////////////// ACCESS TO VARIABLES
	
	@SuppressWarnings("unchecked")
	@Override
	public Iterator<? extends Variable> getIncludedVariables() {
		return new NestedIterator(list(
				getRoot() instanceof Variable? getRoot() : list(),
				functionIterator(getIncludedFactors(), f -> f.getVariables())));
	}

	@SuppressWarnings("unchecked")
	@Override
	public Iterator<? extends Variable> getVariablesThatAreRootOfNodesExternalTo(ExactBPNode sub) {
		myAssert(hasMadeSubsYet(), () -> "getVariablesThatAreRootOfNodesExternalTo is supposed to be invoked on nodes that have made their subs already.");
		return new NestedIterator(list(
				getRoot() instanceof Variable? getRoot() : list(),
				variablesThatAreRootOfNodesInAllOtherSubs(sub),
				getParentNode() != null? getParentNode().getVariablesThatAreRootOfNodesExternalTo(this) : list()
				));
	}

	private Iterator<?> variablesThatAreRootOfNodesInAllOtherSubs(ExactBPNode sub) {
		return functionIterator(allSubsBut(sub), ExactBPNode::getVariablesThatAreRootOfNodes);
	}

	private PredicateIterator<ExactBPNode<SubRootType, RootType>> allSubsBut(ExactBPNode sub) {
		return predicateIterator(getSubs(), s -> s != sub);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Iterator<? extends Variable> getVariablesThatAreRootOfNodes() {
		return new NestedIterator(list(
				getRoot() instanceof Variable? getRoot() : list(),
				hasMadeSubsYet()? functionIterator(getSubs(), ExactBPNode::getVariablesThatAreRootOfNodes) : list()
				));
		
	}
	
	//////////////////// SUBS MANIPULATION
	
	/**
	 * Provides a way of checking if subs have been made yet, without triggering their constructions as would
	 * happen if {@link #getSubs()} were invoked.
	 */
	@Override
	public boolean hasMadeSubsYet() {
		return subs != null;
	}
	
	@Override
	public ArrayList<ExactBPNode<SubRootType,RootType>> getSubs() {
		if (subs == null) {
			subs = makeSubs();
		}
		return subs;
	}
	
	protected ArrayList<ExactBPNode<SubRootType,RootType>> makeSubs() {
		// This method creates the sub-ExactBPs.
		// First, it needs to create the included factor live sets for these subs,
		// because that is required in the constructor of ExactBPs.
		ArrayList<? extends SubRootType> subsRoots = makeSubsRoots();
		ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors = makeInitialSubsIncludedFactors(subsRoots);
		return makeSubsFromTheirIncludedFactors(subsRoots, subsIncludedFactors);
	}

	private ArrayList<RedirectingLiveSet<Factor>> makeInitialSubsIncludedFactors(ArrayList<? extends SubRootType> subsRoots) {
		ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors = mapIntoArrayList(subsRoots, this::makeInitialSubIncludedFactors);
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
		// TODO: for some reason, removing field includedFactors and the above line breaks the program, even though includedFactors is apparently never used!
	}

	private 
	ArrayList<ExactBPNode<SubRootType,RootType>> 
	makeSubsFromTheirIncludedFactors(ArrayList<? extends SubRootType> subsRoots, ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		ArrayList<ExactBPNode<SubRootType,RootType>> subs = new ArrayList<>(subsRoots.size());
		int subIndex = 0;
		for (SubRootType subRoot : subsRoots) {
			ExactBPNode<SubRootType,RootType> sub = makeSubFromItsIncludedFactors(subRoot, subIndex, subsIncludedFactors);
			subs.add(sub);
			subIndex++;
		}
		return subs;
	}

	private ExactBPNode<SubRootType,RootType> makeSubFromItsIncludedFactors(SubRootType subRoot, int subIndex, ArrayList<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		RedirectingLiveSet<Factor> subIncludedFactors = subsIncludedFactors.get(subIndex);
		LiveSet<Factor> subExcludedFactors = excludedFactorsForSubAt(subIndex, subsIncludedFactors);
		ExactBPNode<SubRootType,RootType> sub = makeSubExactBP(subRoot, subExcludedFactors, subIncludedFactors);
		return sub;
	}

	private LiveSet<Factor> excludedFactorsForSubAt(int subIndex, List<RedirectingLiveSet<Factor>> subsIncludedFactors) {
		LiveSet<Factor> unionOfSiblingsExcludedFactors = unionOfAllButTheOneAt(subsIncludedFactors, subIndex);
		LiveSet<Factor> excludedFactorsUnionSiblingsExcludedFactors = excludedFactors.union(unionOfSiblingsExcludedFactors);
		LiveSet<Factor> result = excludedFactorsUnionSiblingsExcludedFactors.union(getFactorsAtRoot());
		return result;
	}

	///////////////////////////// VARIABLES SUMMING OUT
	
	/**
	 * Given the variables in the summand (the product of incoming messages and factor at root),
	 * returns the variables that must be summed out at the root level,
	 * computed as those not considered free according to {@link #isFreeVariable(Variable)}.
	 */
	@Override
	public List<? extends Variable> variablesToBeSummedOutAmong(Collection<? extends Variable> variablesInSummand) {
		List<? extends Variable> variablesToBeSummedOut = collectToList(variablesInSummand, n -> ! isFreeVariable(n));
		return variablesToBeSummedOut;
	}

	@Override
	public boolean isFreeVariable(Variable variable) {
		boolean result = 
				isEqualToRoot(variable)
				||
				isExternalVariable(variable)
				||
				isParameter(variable);
		return result;
	}

	private boolean isEqualToRoot(Variable variable) {
		boolean result = getRoot().equals(variable);
		return result;
	}

	private boolean isParameter(Variable variable) {
		return isParameterPredicate.test(variable);
	}

	private boolean isExternalVariable(Variable variable) {
		boolean result =
				(getParent() != null && getParent().equals(variable)) // covers the case in which there are no external factors and parent is variable
				||
				excludedFactors.thereIsAnElementSatisfying(f -> f.contains(variable)); // all other external variables will be in an external factor.
		return result;
	}

	/**
	 * Returns the result of eliminating given indices from given factor, plus whatever bookkeeping such as printing information or recording size of computation
	 * (since this is one of the most expensive steps). 
	 * @param variablesToBeSummedOut
	 * @param factor
	 * @return
	 */
	@Override
	public Factor sumOutWithBookkeeping(List<? extends Variable> variablesToBeSummedOut, Factor factor) {
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
			log(() -> "Final message on " + getRoot());
		}
		else {
			log(() -> "Message from " + getRoot() + " to " + getParent());
		}
	}

	private void printTracingInformationBody(Factor factor, List<? extends Variable> variablesToBeSummedOut, Factor result) {
		log(() -> "Received total product " + factor);
		String eliminatedString = variablesToBeSummedOut.isEmpty()? "no indices" : join(variablesToBeSummedOut);
		log(() -> "Eliminating " + eliminatedString + " from " + factor + " ---> " + result);
		log(() -> "Sending " + result);
	}
	
	/**
	 * Logging function; takes a nullary function in order to avoid computing the message string unnecessarily.
	 * @param message
	 */
	private void log(NullaryFunction<String> message) {
		if (debug) {
			Util.println(message.apply());
		}
	}

	/////////////////////////// GETTERS AND OTHER BASIC METHODS
	
	@Override
	public RootType getRoot() {
		return root;
	}

	@Override
	public SubRootType getParent() {
		return parent;
	}

	@Override
	public ExactBPNode<SubRootType, RootType> getParentNode() {
		return parentNode;
	}

	public FactorNetwork getFactorNetwork() {
		return factorNetwork;
	}
	
	@Override
	public String toString() {
		return "Exact BP on " + getRoot();
	}
}