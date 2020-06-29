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
package com.sri.ai.praise.core.representation.interfacebased.polytope.core;

import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.firstPolytopeHasFunctionConvexHullWithoutMatchInSecond;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.polytopesAreEqual;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.thereAreFunctionConvexHullsInSecondPolytopeWithoutMatches;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.AbstractFunctionConvexHull.multiplyIntoSingleFunctionConvexHull;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.IdentityPolytope.identityPolytope;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.collect;
import static com.sri.ai.util.Util.collectToSet;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.mapIntoSet;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.product;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.unionOfCollections;
import static com.sri.ai.util.Util.unionOfResults;
import static com.sri.ai.util.base.IsInstanceOf.isInstanceOf;
import static com.sri.ai.util.base.Pair.pair;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.UniformFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck;
import com.sri.ai.util.Enclosing;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.explainableonetoonematching.ExplainableOneToOneMatching;
import com.sri.ai.util.explainableonetoonematching.LeftOverMerger;
import com.sri.ai.util.explainableonetoonematching.Matcher;
import com.sri.ai.util.explainableonetoonematching.UnmatchedElementMerger;

/**
 * @author braz
 *
 */
public class ProductPolytope extends AbstractNonIdentityPolytope implements Polytope {
	
	private List<? extends AtomicPolytope> alreadySimplifiedAtomicPolytopes;
	// by "already simplified", we mean each pair of atomic polytopes in this list cannot be simplified into an atomic polytope,
	// that is, this is the minimum number of atomic polytopes to represent this product as far as pairwise multiplication is concerned
	// (it is conceivable that for some implementations, three- or higher-wise multiplication may produce simplifications when two-wise multiplication cannot).

	/**
	 * Makes a {@link ProductPolytope}. This is not meant for end-user usage, since it assumes the given atomic polytopes are already checked for pair-wise multiplication.
	 * Users must call {@link Polytope#multiply(Collection)} instead.
	 * @param alreadySimplifiedAtomicPolytopes
	 */
	ProductPolytope(Collection<? extends AtomicPolytope> alreadySimplifiedAtomicPolytopes) {
		super();
		myAssert(alreadySimplifiedAtomicPolytopes.size() != 0, () -> "Cannot define product on an empty set of polytopes. Instead, use makePolytopeEquivalentToProductOfAtomicPolytopes (if the polytopes satisfy the condition in the name), or create an IdentityPolytope (if you know you have zero polytopes), or use the more general static Polytope.multiply which checks everything for you.");
		myAssert(alreadySimplifiedAtomicPolytopes.size() != 1, () -> "Cannot define product on a single element.  Instead, use makePolytopeEquivalentToProductOfAtomicPolytopes if you know for sure there will be just one atomic polytope, or general static Polytope.multiply which checks everything for you.");
		this.alreadySimplifiedAtomicPolytopes = new LinkedList<>(alreadySimplifiedAtomicPolytopes);
	}
	
	/**
	 * Only use this method if you already know that the product of no pair of these polytopes represents a simplification.
	 * If you need that to be checked first, use {@link Polytope#multiply(Collection)} instead.
	 * @param alreadySimplifiedAtomicPolytopes
	 * @return
	 */
	public static Polytope makeEquivalentToProductOf(Collection<? extends AtomicPolytope> alreadySimplifiedAtomicPolytopes) {
		Polytope result;
		if (alreadySimplifiedAtomicPolytopes.isEmpty()) {
			result = identityPolytope();
		}
		else if (alreadySimplifiedAtomicPolytopes.size() == 1) {
			result = getFirst(alreadySimplifiedAtomicPolytopes);
		}
		else {
			result = new ProductPolytope(alreadySimplifiedAtomicPolytopes);
		}
		return result;
	}
	
	public static Polytope makeEquivalentToProductOf(AtomicPolytope... atomicPolytopes) {
		return makeEquivalentToProductOf(Arrays.asList(atomicPolytopes));
	}

	@Override
	public Collection<? extends AtomicPolytope> getAtomicPolytopes() {
		return alreadySimplifiedAtomicPolytopes;
	}

	@Override
	public boolean isIdentity() {
		return false;
	}

	@Override
	public Collection<? extends Variable> getFreeVariables() {
		
		Collection<Collection<? extends Variable>> listOfFreeVariablesCollections = 
				mapIntoList(alreadySimplifiedAtomicPolytopes, Polytope::getFreeVariables);
		
		Set<? extends Variable> allFreeVariables = unionOfCollections(listOfFreeVariablesCollections);
		
		return allFreeVariables;
	}

	/////////////////////// MULTIPLICATION
	
	@Override
	public Polytope multiply(Polytope another) {
		Polytope result;
		if (another.isIdentity()) {
			result = this;
		}
		else if (another instanceof ProductPolytope) {
			Collection<? extends Polytope> anotherSubPolytopes = ((ProductPolytope)another).getAtomicPolytopes();
			result = accumulate(anotherSubPolytopes, Polytope::multiply, this);
		}
		else {
			AtomicPolytope anotherAtomicPolytope = (AtomicPolytope) another;
			result = multiplyListOfAlreadySimplifiedAtomicPolytopesWithANewOne(alreadySimplifiedAtomicPolytopes, anotherAtomicPolytope);
		}
		return result;
	}

	private static Polytope multiplyListOfAlreadySimplifiedAtomicPolytopesWithANewOne(
			Collection<? extends AtomicPolytope> atomicPolytopes,  AtomicPolytope atomicAnother) {
		
		List<AtomicPolytope> resultAtomicPolytopes = list();
		boolean anotherAlreadyIncorporated = false;
		for (AtomicPolytope atomicPolytope : atomicPolytopes) {
			if (anotherAlreadyIncorporated) {
				resultAtomicPolytopes.add(atomicPolytope);
			}
			else {
				anotherAlreadyIncorporated = 
						addToListEitherPolytopeOrProductOfPolytopeIfProductIsAtomic(
								atomicPolytope,
								atomicAnother,
								resultAtomicPolytopes);
			}
		}
		
		includeAnotherByItselfIfMultiplicationsFailed(atomicAnother, anotherAlreadyIncorporated, resultAtomicPolytopes);
		
		Polytope result = makeEquivalentToProductOf(resultAtomicPolytopes);
		
		return result;
	}

	private static boolean addToListEitherPolytopeOrProductOfPolytopeIfProductIsAtomic(
			AtomicPolytope atomicPolytope,
			AtomicPolytope anotherAtomicPolytope,
			List<AtomicPolytope> list) {
		
		var product = atomicPolytope.getProductIfItIsASimplificationOrNullOtherwise(anotherAtomicPolytope);
		var productIsAtomic = product != null;
		list.add(productIsAtomic? product : atomicPolytope);
		return productIsAtomic;
	}

	private static void includeAnotherByItselfIfMultiplicationsFailed(
			AtomicPolytope anotherAtomicPolytope, 
			boolean anotherAlreadyIncorporated, 
			List<AtomicPolytope> resultAtomicPolytopes) {
		
		if (! anotherAlreadyIncorporated) {
			resultAtomicPolytopes.add(anotherAtomicPolytope);
		}
	}
	
	//////////////////// SUMMING OUT
	
	@Override
	public Polytope sumOut(Collection<? extends Variable> eliminated) {
		
		List<AtomicPolytope> independentOfEliminated = list();
		List<AtomicPolytope> dependentOfEliminated = list();

		collect(
				/* original collection: */ this.getAtomicPolytopes(), 
				/* criterion: */ isIndependentOf(eliminated), 
				/* satisfy criterion: */ independentOfEliminated, 
				/* do not satisfy criterion: */ dependentOfEliminated);

		Polytope summedOutFromDependents = sumOutEliminatedVariablesFromPolytopesDependingOnThem(eliminated, dependentOfEliminated);

		List<AtomicPolytope> allAtomicPolytopesInResult = independentOfEliminated; // re-using independentOfEliminated
		allAtomicPolytopesInResult.addAll(summedOutFromDependents.getAtomicPolytopes());
		Polytope result = makeEquivalentToProductOf(allAtomicPolytopesInResult);

		return result;
	}
	
	private static Predicate<? super AtomicPolytope> isIndependentOf(Collection<? extends Variable> variables) {
		return p -> ! Util.intersect(p.getFreeVariables(), variables);
	}
	
	private Polytope sumOutEliminatedVariablesFromPolytopesDependingOnThem(
			Collection<? extends Variable> eliminated,
			Collection<? extends AtomicPolytope> dependentAtomicPolytopes) {

		var nonSimplex = getFirst(dependentAtomicPolytopes, p -> !( p instanceof Simplex));
		
		var allAreSimplices = nonSimplex == null;

		if (allAreSimplices) {
			return makeEquivalentToProductOf(dependentAtomicPolytopes);
		}
		else {
			return sumOutFromDependentAtomicPolytopes(eliminated, dependentAtomicPolytopes);
		}
	}

	/**
	 * Method for summing out a set of variables from a product polytope.

	 * <pre>
	 * The problem is as follows:
	 * 
	 * sum_V polytope
	 * 
	 * A polytope may be of three types: atomic polytopes simplex and intensional convex hull, and products of polytopes.
	 * 
	 * We break product of polytopes into their atomic component and obtain the form:
	 * 
	 * sum_V CH_1...CH_m S_1 ... S_n
	 * 
	 * where
	 * CH_i = {(on U_i) phi_i } is an intensional convex hull, and
	 * S_j is a simplex on variable W_j.
	 * 
	 * We can easily factor out polytopes whose free variables do not intersect with V,
	 * and assume the form above in which free variables always intersect with V.
	 * Then we have:
	 * 
	 * sum_V { (on U_1) phi_1 }...{ (on U_m) phi_m }   S_1 ... S_n
	 * 
	 * =
	 * 
	 * Union_{W_1,...,W_n} sum_{V \ {W_1,...,W_n}} {(on U_1) phi_1 }...{(on U_m) phi_m } 
	 * 
	 * =
	 * 
	 * Union_{W_1,...,W_n, U_1...U_m} { sum_{V \ ({W_1,...,W_n} union Union_I U_i)} phi_1...phi_m }
	 *  
	 * =
	 * 
	 * {(on W_1,...,W_n, U_1...U_m) sum_{V \ ({W_1,...,W_n} union Union_I U_i)} phi_1...phi_m } 
	 *  
	 * =
	 * 
	 * {(on W_1,...,W_n, U_1...U_m) phi' } 
	 * 
	 * which is an intensional convex hull representing the result.
	 * 
	 * The result can be further simplified by eliminating indices that do no appear in phi',
	 * and by considering {(on ) 1} a multiplication identity polytope that can be eliminated from products.
	 * 
	 * Examples:
	 *
	 * Example 1:
	 * 
	 * sum_{I,K} {(on J) if I = K and I = M then 1 else 0} S_K S_M
	 * =
	 * S_M * sum_{I,K} {(on J) if I = K and I = M then 1 else 0} S_K
	 * =
	 * S_M * Union_K sum_I {(on J) if I = K and I = M then 1 else 0}
	 * =
	 * S_M * Union_{J,K} { sum_I if I = K and I = M then 1 else 0 }
	 * =
	 * S_M * Union_{J,K} { phi(K,M) }  for some phi
	 * =
	 * S_M * {(on J,K) phi(K,M) }
	 * =
	 * S_M * {(on K) phi(K,M) }
	 * 
	 * 
	 * Example 2: m = 0.
	 * 
	 * sum_{I,J,K} S_K S_M
	 * =
	 * S_M * sum_{I,J,K} S_K
	 * =
	 * S_M * Union_K { sum_{I,J} 1 }
	 * =
	 * S_M * Union_K { 1 }
	 * =
	 * S_M * {(on K) 1 }
	 * =
	 * S_M
	 * </pre>
	 *
	 * 
	 * Example 3:
	 * 
	 * sum_{I,J,K} phi(I,K) S_K S_M
	 * =
	 * sum_{I,J,K} {(on ) phi(I,K)} S_K S_M
	 * =
	 * S_M * sum_{I,J,K} {(on ) phi(I,K)} S_K
	 * =
	 * S_M * Union_K { sum_{I,J} phi(I,K) }
	 * =
	 * S_M * Union_K { phi'(K) }
	 * =
	 * S_M * {(on K) phi'(K) }
	 * </pre>

	 * @author braz
	 *
	 */
	private static Polytope sumOutFromDependentAtomicPolytopes(Collection<? extends Variable> eliminated, Collection<? extends AtomicPolytope> polytopesDependentOnEliminated) {
	
		// 1. separate simplices and convex hulls
		// 2. atomicConvexHull = compute atomic convex hull equivalent to the product of the convex hulls
		// 3. sum out (eliminated - simplex variables) from atomicConvexHull
		// 4. add simplices indices to result
		// Note that line 2. is crucial because it is at forming an atomic polytope that convex hull representation might be simplified.
		// Note that one intuition is that turning the product of polytopes into an atomic one and invoking sumOut would suffice,
		// but it is not true. That would create a convex hull with the simplex free variables that are also to be eliminated
		// as indices of the resulting convex hull,
		// and then try to sum out those same variables from the convex hull with them as indices.
		// This requires special care because the eliminated variables must be free variables in the polytopes and the indices
		// are not free variables.
		// This would still be well-defined because indices are a new scope so they could just be standardized apart,
		// but the method sumOut does not attempt to standardize apart all the time as that would be expensive.
		// To use sumOut without having to worry about standardizing apart, we instead use a technique
		// that takes care of the simplices separately, and explicitly uses the fact that this is a summation.

		var simplicesAndFunctionConvexHulls = separateSimplicesAndFunctionConvexHulls(polytopesDependentOnEliminated);
		var simplices = simplicesAndFunctionConvexHulls.first;
		var functionConvexHulls = simplicesAndFunctionConvexHulls.second;
		
		var productOfConvexHulls = multiplyIntoSingleFunctionConvexHull(functionConvexHulls);
		
		var simplexVariables = mapIntoList(simplices, Simplex::getVariable);
		var eliminatedMinusSimplexVariables = subtract(eliminated, simplexVariables);
		var productOfConvexHullsAfterSummingOut = productOfConvexHulls.sumOut(eliminatedMinusSimplexVariables);
	
		var indicesToAdd = 
				intersection(
						simplexVariables, 
						productOfConvexHullsAfterSummingOut.getFreeVariables());
		
		var result = productOfConvexHullsAfterSummingOut.addIndices(indicesToAdd);
		
		return result;
	}

	private static Pair<List<? extends Simplex>, Collection<? extends FunctionConvexHull>> 
	separateSimplicesAndFunctionConvexHulls(Collection<? extends AtomicPolytope> atomicPolytopes) {
		
		List<Simplex> simplices = list();
		List<FunctionConvexHull> functionConvexHulls = list();
		for (var p : atomicPolytopes) {
			if (p instanceof Simplex) {
				simplices.add((Simplex) p);
			}
			else {
				functionConvexHulls.add((FunctionConvexHull) p);
			}
		}

		return pair(simplices, functionConvexHulls);
	}
	
	//////////////////////// GET ATOMIC POLYTOPE
	
	@Override
	public AtomicPolytope getEquivalentAtomicPolytope() {
		AtomicPolytope result;
		if (getFreeVariables().size() == 1) {
			result = getEquivalentAtomicPolytopeForOneFreeVariableCase();
		}
		else {
			result = getEquivalentAtomicPolytopeForMultipleFreeVariablesCase();
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private AtomicPolytope getEquivalentAtomicPolytopeForOneFreeVariableCase() {
		var variable = getFirst(getFreeVariables());

		Simplex simplexOnVariableIfAny = (Simplex) getFirst(getAtomicPolytopes(), p -> isSimplexOn(p, variable));
		
		boolean thereIsSimplexOnVariableSoItDominates = simplexOnVariableIfAny != null;
		
		AtomicPolytope result;
		if (thereIsSimplexOnVariableSoItDominates) {
			result = simplexOnVariableIfAny;
		}
		else {
			// all atomic polytopes are non-simplex, or otherwise we would have simplexes on other variables and there would be more than only free variable
			result = multiplyIntoSingleFunctionConvexHull((Collection<? extends FunctionConvexHull>) getAtomicPolytopes());
		}
		return result;
	}
	
	private AtomicPolytope getEquivalentAtomicPolytopeForMultipleFreeVariablesCase() {
		// We get atomic polytopes with hulls coming first.
		// This is due to simplices being converted to function convex hulls being based on Kronecker delta factors.
		// If two simplices occur before any function convex hulls, 
		// we would try to multiply the Kronecker delta factors, 
		// which is not currently supported because this would require 
		// either the ability to represent a product factor with multiple Kronecker delta factors, 
		// or a multi-variable-pair Kronecker.
		// Here, if all atomic polytopes are simplices an error is thrown.
		// Upper code must deal with that case separately.
		// The ultimate solution for this is to have something like ProductFactor 
		// that can hold multiple Kronecker factors.
		var atomicPolytopesWithHullsComingFirst = getAtomicPolytopesWithFunctioConvexHullsComingFirst();
		var functionConvexHulls = mapIntoArrayList(atomicPolytopesWithHullsComingFirst, this::getCorrespondingFunctionConvexHull);
		var allFactors = mapIntoList(functionConvexHulls, FunctionConvexHull::getFactor);
		var allIndices = unionOfResults(functionConvexHulls, FunctionConvexHull::getIndices);
		var productFactor = Factor.multiply(allFactors);
		var result = new DefaultFunctionConvexHull(allIndices, productFactor);
		return result;
	}

	private ArrayList<? extends AtomicPolytope> getAtomicPolytopesWithFunctioConvexHullsComingFirst() {
		var atomicPolytopes = arrayListFrom(getAtomicPolytopes());
		atomicPolytopes.sort((f1, f2) -> f1 instanceof FunctionConvexHull? -1 : f2 instanceof FunctionConvexHull? +1 : 0);
		if (atomicPolytopes.get(0) instanceof Simplex) {
			throw new Error("Trying to obtain an atomic polytope equivalent to a product polytope composed by simplices only. This is not supported because the result is a multi-variable simplex, which is not yet supported or implemented. For now, code should check for this case beforehand and get around it. For example, a common case for obtaining an equivalent atomic polytope is to compute a polytope's length. But if the polytope is a simplex-only product, then the length is trivially 1.");
		}
		return atomicPolytopes;
	}
	
	private FunctionConvexHull getCorrespondingFunctionConvexHull(AtomicPolytope atomicPolytope) {
		if (atomicPolytope instanceof Simplex) {
			return makeKroneckerDeltaPolytopeFrom((Simplex) atomicPolytope);
		}
		else if (atomicPolytope instanceof FunctionConvexHull) {
			return (FunctionConvexHull) atomicPolytope;
		}
		else {
			throw new Error("Unexpected atomic polytope type in " + (new Enclosing() {}).methodName() + ": " + atomicPolytope.getClass() + ", atomic polytope: " + atomicPolytope);
		}
	}

	private FunctionConvexHull makeKroneckerDeltaPolytopeFrom(Simplex simplex) {
		var factor = Polytopes.makeKroneckerDeltaFactorFrom(simplex);
		var kroneckerDeltaPolytopeIndex = factor.getVariables().get(1);
		var hull = new DefaultFunctionConvexHull(list(kroneckerDeltaPolytopeIndex), factor);
		return hull;
	}
	
	private static boolean isSimplexOn(AtomicPolytope atomicPolytope, Variable variable) {
		boolean result = 
				atomicPolytope instanceof Simplex
				&&
				((Simplex)atomicPolytope).getVariable().equals(variable);
		return result;
	}
	
	////////////////// ANCILLARY

	@Override
	public String toString() {
		String result = join(alreadySimplifiedAtomicPolytopes, "*");
		return result;
	}
	
	@Override
	public int hashCode() {
		return getAtomicPolytopes().hashCode();
	}

	@Override
	public boolean equals(Object another) {
		boolean result =
				another instanceof ProductPolytope
				&&
				((ProductPolytope) another).getAtomicPolytopes().equals(getAtomicPolytopes());
		return result;
	}

	@Override
	public boolean equalsModuloPermutations(Object another) {
		
		if (another instanceof Polytope) {
			
			Polytope anotherPolytope = (Polytope) another;

			List<AtomicPolytope> simplices1 = list();
			List<AtomicPolytope> hulls1 = list();
			collect(getAtomicPolytopes(), a -> a instanceof Simplex, simplices1, hulls1);

			List<AtomicPolytope> simplices2 = list();
			List<AtomicPolytope> hulls2 = list();
			collect(anotherPolytope.getAtomicPolytopes(), a -> a instanceof Simplex, simplices2, hulls2);

			var simplexVariables1 = mapIntoSet(simplices1, a -> ((Simplex)a).getVariable());
			var simplexVariables2 = mapIntoSet(simplices2, a -> ((Simplex)a).getVariable());

			var simplicesAreEqual = simplexVariables1.equals(simplexVariables2);

			if (simplicesAreEqual) {
				return Util.thereIsAOneToOneMatching(hulls1, hulls2, Polytope::equalsModuloPermutations);
			}
			else {
				return false;
			}
		}
		else {
			return false;
		}
	}
	
	@Override
	public PolytopesEqualityCheck checkEquality(Polytope another) {
		
		List<AtomicPolytope> simplices1 = list();
		List<AtomicPolytope> hulls1 = list();
		collect(getAtomicPolytopes(), a -> a instanceof Simplex, simplices1, hulls1);

		List<AtomicPolytope> simplices2 = list();
		List<AtomicPolytope> hulls2 = list();
		collect(another.getAtomicPolytopes(), a -> a instanceof Simplex, simplices2, hulls2);

		var simplexVariables1 = mapIntoSet(simplices1, a -> ((Simplex)a).getVariable());
		var simplexVariables2 = mapIntoSet(simplices2, a -> ((Simplex)a).getVariable());
		
		if (simplexVariables1.equals(simplexVariables2)) {
			return checkEqualityGivenSameSimplices(another, hulls1, hulls2);
		}
		else {
			return makeEqualityCheckForDifferentSimplices(another, simplexVariables1, simplexVariables2);
		}
		
	}

	private PolytopesEqualityCheck checkEqualityGivenSameSimplices(Polytope another, List<AtomicPolytope> hulls1, List<AtomicPolytope> hulls2) {
		if (hulls1.size() == hulls2.size()) {
			return checkEqualityGivenTheSameNumberOfFunctionConvexHulls(another, hulls1, hulls2);
		}
		else {
			return PolytopesEqualityCheck.polytopesHaveADifferentNumberOfFunctionConvexHulls(this, another);
		}
	}

	private PolytopesEqualityCheck checkEqualityGivenTheSameNumberOfFunctionConvexHulls(
			Polytope another,
			List<AtomicPolytope> hulls1,
			List<AtomicPolytope> hulls2) {
		
		Matcher<FunctionConvexHull, PolytopesEqualityCheck> 
		matcher = 
		(f1, f2) -> { var equalityCheck = f1.checkEquality(f2); return equalityCheck.areEqual()? null : equalityCheck; };
		// Note: ExplainableOneToOneMatching requires matcher to return null in case of equality.
		
		UnmatchedElementMerger<FunctionConvexHull, Set<PolytopesEqualityCheck>, PolytopesEqualityCheck> 
		unmatchedElementMerger = 
		(f, c) -> firstPolytopeHasFunctionConvexHullWithoutMatchInSecond(this, another, f, c);
		
		LeftOverMerger<Set<FunctionConvexHull>, PolytopesEqualityCheck> 
		leftOverMerger = 
		l -> thereAreFunctionConvexHullsInSecondPolytopeWithoutMatches(this, another, l);
		
		PolytopesEqualityCheck polytopesAreEqual = polytopesAreEqual(this, another);

		List<FunctionConvexHull> hullsAsHulls1 = mapIntoList(hulls1, h -> (FunctionConvexHull) h);
		List<FunctionConvexHull> hullsAsHulls2 = mapIntoList(hulls2, h -> (FunctionConvexHull) h);
		
		return ExplainableOneToOneMatching.match(hullsAsHulls1, hullsAsHulls2, matcher, unmatchedElementMerger, leftOverMerger, polytopesAreEqual);
		
		// Note: as of February 2020 left-over merger is never used because at this point the number of function convex hulls in both polytopes is identical.
		// However, we're leaving it anyway in case future modifications make it necessary.
		
	}

	private PolytopesEqualityCheck makeEqualityCheckForDifferentSimplices(
			Polytope another,
			Set<Variable> simplexVariables1,
			Set<Variable> simplexVariables2) {
		
		var simplexVariablesInFirstButNotInSecond = collectToSet(simplexVariables1, v -> !simplexVariables2.contains(v));
		var simplexVariablesInSecondButNotInFirst = collectToSet(simplexVariables2, v -> !simplexVariables1.contains(v));
		return PolytopesEqualityCheck.polytopesHaveDifferentSimplices(this, another, simplexVariablesInFirstButNotInSecond, simplexVariablesInSecondButNotInFirst);
	}

	@Override
	public Factor probabilityRange() {
		
		// Since getEquivalentAtomicPolytope is invalid for product with simplices only, we check this case first
		if (forAll(getAtomicPolytopes(), isInstanceOf(Simplex.class))) {
			return new UniformFactor();
		}
		
		try {
			return getEquivalentAtomicPolytope().probabilityRange();
		}
		catch (Error e) {
			throw new Error((new Enclosing() {}).methodName() + " not supported in this case", e);
		}
	}
	
	@Override
	public double length() {
		// We override the default implementation because it uses {@link #probabilityRange()}
		// which does not currently support simplex-only product factors.
		// So we check for this case separately here.
		if (forAll(getAtomicPolytopes(), a -> a instanceof Simplex)) {
			return 1.0;
		}
		else {
			return super.length();
		}
	}

	@Override
	public Polytope normalize(Collection<? extends Variable> variablesToNormalize) {
		throw new Error("normalize is not valid for ProductPolytope");
	}

	@Override
	public int memory() {
		return Util.product(functionIterator(getAtomicPolytopes(), Polytope::memory)).intValue();
	}
}