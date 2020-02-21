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

import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.AbstractFunctionConvexHull.multiplyIntoSingleFunctionConvexHullWithoutSimplifying;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.IdentityPolytope.identityPolytope;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.collect;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.unionOfCollections;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.util.Util;

/**
 * @author braz
 *
 */
public class ProductPolytope extends AbstractPolytope implements Polytope {
	
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
	public static Polytope makePolytopeEquivalentToProductOfAtomicPolytopes(Collection<? extends AtomicPolytope> alreadySimplifiedAtomicPolytopes) {
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
		
		Polytope result = makePolytopeEquivalentToProductOfAtomicPolytopes(resultAtomicPolytopes);
		
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
		Polytope result = makePolytopeEquivalentToProductOfAtomicPolytopes(allAtomicPolytopesInResult);

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
			return makePolytopeEquivalentToProductOfAtomicPolytopes(dependentAtomicPolytopes);
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
		
		List<AtomicPolytope> simplexPolytopes = list();
		List<AtomicPolytope> functionConvexHullPolytopes = list();
		collect(polytopesDependentOnEliminated, p -> p instanceof Simplex, simplexPolytopes, functionConvexHullPolytopes);
	
		@SuppressWarnings("unchecked")
		List<? extends Simplex> simplices = (List<? extends Simplex>) simplexPolytopes;

		var simplexVariables = mapIntoList(simplices, Simplex::getVariable);
	
		@SuppressWarnings("unchecked")
		var functionConvexHulls = (Collection<? extends FunctionConvexHull>) functionConvexHullPolytopes;
		
		// it's important to wait for simplification until the end because simplification will often depend
		// on the indices and we want a polytope equivalent to the one with all the simplex variables as indices.
		var productOfConvexHulls = multiplyIntoSingleFunctionConvexHullWithoutSimplifying(functionConvexHulls);
		
		var eliminatedMinusSimplexVariables = subtract(eliminated, simplexVariables);
		var productOfConvexHullsAfterSummingOut = productOfConvexHulls.sumOut(eliminatedMinusSimplexVariables);
	
		var indicesToAdd = 
				intersection(
						simplexVariables, 
						productOfConvexHullsAfterSummingOut.getFreeVariables());
		
		var resultBeforeSimplifying = productOfConvexHullsAfterSummingOut.addIndices(indicesToAdd);
		
		Polytope result = resultBeforeSimplifying.simplify();
		
		return result;
	}

	//////////////////////// GET SINGLE ATOMIC POLYTOPE FOR A VARIABLE
	
	@Override
	public AtomicPolytope getEquivalentAtomicPolytopeOn(Variable variable) {
		var nonSimplex = getFirst(getAtomicPolytopes(), p -> !( p instanceof Simplex));
		
		var allAreSimplices = nonSimplex == null;

		if (allAreSimplices) {
			var simplexOnVariable = getFirst(getAtomicPolytopes(), p -> ((Simplex)p).getVariable().equals(variable));
			if (simplexOnVariable == null) {
				throw new Error("ProductPolytope has variables " + getFreeVariables() + " but getEquivalentAtomicPolytopeOn was requested for one not in it: " + variable);
			}
			else {
				return simplexOnVariable;
			}
		}
		else {
			return getEquivalentAtomicPolytopeOn(variable, getAtomicPolytopes());
		}
	}
	
	@SuppressWarnings("unchecked")
	public static AtomicPolytope getEquivalentAtomicPolytopeOn(Variable variable, Collection<? extends AtomicPolytope> atomicPolytopes) {
		Simplex simplexOnVariableIfAny = (Simplex) getFirst(atomicPolytopes, p -> isSimplexOn(p, variable));
		
		boolean thereIsSimplexOnQuerySoItDominates = simplexOnVariableIfAny != null;
		
		AtomicPolytope result;
		if (thereIsSimplexOnQuerySoItDominates) {
			result = simplexOnVariableIfAny;
		}
		else {
			// all atomicPolytopes are non-simplex, or otherwise we would have simplexes on non-query variables and the query would not be the only free variable
			result = multiplyIntoSingleFunctionConvexHullWithoutSimplifying((Collection<? extends FunctionConvexHull>) atomicPolytopes);
		}
		
		return result;
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

}