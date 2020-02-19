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
package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base;

import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.IdentityPolytope.identityPolytope;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.collect;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.unionOfCollections;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.NonSimplexAtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.util.Util;

/**
 * @author braz
 *
 */
public class ProductPolytope extends AbstractPolytope implements Polytope {
	
	private List<? extends AtomicPolytope> alreadyMultipledAtomicPolytopes;
	// by "already multiplied", we mean each pair of atomic polytopes in this list cannot be multiplied into an atomic polytope,
	// that is, this is the minimum number of atomic polytopes to represent this product as far as pairwise multiplication is concerned
	// (it is conceivable that for some implementations, three- or higher-wise multiplication may produce atomic polytopes when two-wise multiplication cannot).

	/**
	 * Makes a {@link ProductPolytope}. This is not meant for end-user usage, since it assumes the given atomic polytopes are already checked for pair-wise multiplication.
	 * Users must call {@link Polytope#multiply(Collection)} instead.
	 * @param alreadyMultipledAtomicPolytopes
	 */
	ProductPolytope(Collection<? extends AtomicPolytope> alreadyMultipledAtomicPolytopes) {
		super();
		myAssert(alreadyMultipledAtomicPolytopes.size() != 0, () -> "Cannot define product on an empty set of polytopes. Instead, use makeProductFromAlreadyMultipliedAtomicPolytopes (if the polytopes satisfy the condition in the name), or create an IdentityPolytope (if you know you have zero polytopes), or use the more general static Polytope.multiply which checks everything for you.");
		myAssert(alreadyMultipledAtomicPolytopes.size() != 1, () -> "Cannot define product on a single element.  Instead, use makeProductFromAlreadyMultipliedAtomicPolytopes if you know for sure there will be just one atomic polytope, or general static Polytope.multiply which checks everything for you.");
		this.alreadyMultipledAtomicPolytopes = new LinkedList<>(alreadyMultipledAtomicPolytopes);
	}
	
	/**
	 * Only use this method if you already know that the product of no pair of these polytopes generates an atomic polytope.
	 * If you need that to be checked first, use {@link Polytope#multiply(Collection)} instead.
	 * @param alreadyMultipliedAtomicPolytopes
	 * @return
	 */
	public static Polytope makeProductPolytopeFromAlreadyMultipliedAtomicPolytopes(Collection<? extends AtomicPolytope> alreadyMultipliedAtomicPolytopes) {
		
		if (alreadyMultipliedAtomicPolytopes.isEmpty()) {
			return identityPolytope();
		}
		else if (alreadyMultipliedAtomicPolytopes.size() == 1) {
			return getFirst(alreadyMultipliedAtomicPolytopes);
		}
		else {
			return new ProductPolytope(alreadyMultipliedAtomicPolytopes);
		}
	}

	public Collection<? extends Polytope> getPolytopes() {
		return alreadyMultipledAtomicPolytopes;
	}
	
	@Override
	public Collection<? extends AtomicPolytope> getAtomicPolytopes() {
		return alreadyMultipledAtomicPolytopes;
	}

	@Override
	public boolean isIdentity() {
		return false;
	}

	@Override
	public Collection<? extends Variable> getFreeVariables() {
		
		Collection<Collection<? extends Variable>> listOfFreeVariablesCollections = 
				mapIntoList(alreadyMultipledAtomicPolytopes, Polytope::getFreeVariables);
		
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
			Collection<? extends Polytope> anotherSubPolytopes = ((ProductPolytope)another).getPolytopes();
			result = accumulate(anotherSubPolytopes, Polytope::multiply, this);
		}
		else {
			AtomicPolytope anotherAtomicPolytope = (AtomicPolytope) another;
			result = ProductPolytope.multiplyListOfAlreadyMultipliedAtomicPolytopesWithANewOne(alreadyMultipledAtomicPolytopes, anotherAtomicPolytope);
		}
		return result;
	}

	private static Polytope multiplyListOfAlreadyMultipliedAtomicPolytopesWithANewOne(
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
		
		var product = atomicPolytope.getProductIfItIsAAtomicPolytopeOrNullOtherwise(anotherAtomicPolytope);
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

	private static Polytope makePolytopeEquivalentToProductOfAtomicPolytopes(List<? extends AtomicPolytope> resultAtomicPolytopes) {
		Polytope result;
		if (resultAtomicPolytopes.isEmpty()) {
			result = identityPolytope();
		}
		else if (resultAtomicPolytopes.size() == 1) {
			result = getFirst(resultAtomicPolytopes);
		}
		else {
			result = new ProductPolytope(resultAtomicPolytopes);
		}
		return result;
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
		Polytope result = makeProductPolytopeFromAlreadyMultipliedAtomicPolytopes(allAtomicPolytopesInResult);

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
			return makeProductPolytopeFromAlreadyMultipliedAtomicPolytopes(dependentAtomicPolytopes);
		}
		else {
			return ((NonSimplexAtomicPolytope) nonSimplex).sumOutFromDependentAtomicPolytopes(eliminated, dependentAtomicPolytopes);
		}
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
			return ((NonSimplexAtomicPolytope) nonSimplex).getEquivalentAtomicPolytopeOn(variable, getAtomicPolytopes());
		}
		
	}
	
	////////////////// ANCILLARY

	@Override
	public String toString() {
		String result = join(alreadyMultipledAtomicPolytopes, "*");
		return result;
	}
	
	@Override
	public boolean equals(Object another) {
		boolean result =
				another instanceof ProductPolytope
				&&
				((ProductPolytope) another).getPolytopes().equals(getPolytopes());
		return result;
	}
	
	@Override
	public int hashCode() {
		return getPolytopes().hashCode();
	}

}