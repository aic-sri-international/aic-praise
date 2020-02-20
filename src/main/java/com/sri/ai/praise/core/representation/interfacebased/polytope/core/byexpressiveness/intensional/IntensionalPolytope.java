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
package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.intensional;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.listFrom;
import static com.sri.ai.util.Util.makeListWithElementsOfTwoCollections;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.Util.subtract;
import static java.util.stream.Collectors.toList;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.NonSimplexAtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.AbstractAtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.ProductPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.Simplex;

/**
 * @author braz
 *
 */
public class IntensionalPolytope extends AbstractAtomicPolytope implements NonSimplexAtomicPolytope {
	
	private Set<? extends Variable> indices;

	private Factor factor;
	
	public IntensionalPolytope(Collection<? extends Variable> indices, Factor factor) {
		Set<Variable> indicesAppearingInFactor = intersection(indices, factor.getVariables());
		this.indices = indicesAppearingInFactor;
		this.factor = factor;
	}

	public Collection<? extends Variable> getIndices() {
		return indices;
	}

	private List<Variable> free;
	@Override
	public Collection<? extends Variable> getFreeVariables() {
		if (free == null) {
			List<? extends Variable> all = factor.getVariables();
			free = subtract(all, getIndices());
		}
		return free;
	}

	public Factor getFactor() {
		return factor;
	}
	
	@Override
	public boolean isIdentity() {
		boolean result = 
				indices.isEmpty()
				&&
				factor.isIdentity();
		return result;
	}

	@Override
	public AtomicPolytope getProductIfItIsASimplificationOrNullOtherwise(AtomicPolytope anotherAtomicPolytope) {
		AtomicPolytope result;
		if (anotherAtomicPolytope instanceof IntensionalPolytope) {
			result = multiplyByIntensionalPolytopeIfSameIndicesOrNull(anotherAtomicPolytope);
		}
		else {
			result = null;
		}
		return result;
	}

	private AtomicPolytope multiplyByIntensionalPolytopeIfSameIndicesOrNull(AtomicPolytope another) {
		AtomicPolytope result;
		IntensionalPolytope anotherIntensionalPolytope = (IntensionalPolytope) another;
		if (indices.equals(anotherIntensionalPolytope.getIndices())) {
			Factor productFactor = factor.multiply(anotherIntensionalPolytope.getFactor());
			result = new IntensionalPolytope(indices, productFactor);
		}
		else {
			result = null;
		}
		return result;
		
		// We could multiply intensional polytopes with *unifiable* indices, that is,
		// indices with the same type but different names.
		// However whether that is worth it seems to be an empirical question.
	}

	/////////////////////////////// SUMMING OUT
	
	@Override
	public Polytope sumOut(Collection<? extends Variable> eliminated) {
		var eliminatedOccurringInPolytope = intersection(eliminated, getFreeVariables());
		if (eliminatedOccurringInPolytope.isEmpty()) {
			return this;
		}
		else {
			var newFactor = getFactor().sumOut(listFrom(eliminatedOccurringInPolytope)); // TODO: does Factor.sumOut really need a list? It should work with just a collection.
			return new IntensionalPolytope(getIndices(), newFactor);
		}
		// Note that this implementation considers polytopes equivalent modulo normalization.
		// This plays a role here because sum_V Polytope_on_U for V containing variables other than U will result in their cardinality multiplying the result.
		// If we want to represent that, we must rely on the specific polytope implementation used.
	}

	/**
	 * Method for summing out a set of variables from a product polytope.
	 * This is implemented at this class because {@link ProductPolytope} is a generic class that does not know the representation details of the polytopes it multiplies,
	 * so it delegates to the class implementing non-simplex atomic polytopes.

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
	@Override
	public Polytope sumOutFromDependentAtomicPolytopes(Collection<? extends Variable> eliminated, Collection<? extends Polytope> polytopesDependentOnEliminated) {
		
		// This is a bit tricky to understand, but one thing to keep in mind is that eliminated simplex variables become intensional convex hell indices by this process.
		// See full explanation in class javadoc.
		
		var simplexVariables = collectSimplexVariables(polytopesDependentOnEliminated);
		// because each simplex has a single variable and all simplices depend on eliminated, all simplex variables are in eliminated.
		
		var indicesFromIntensionalPolytopes = collectIndicesFromTheIntensionalPolytopesAmongThese(polytopesDependentOnEliminated);
		
		var factors = collectFactorsFromPolytopesThatAreIntensionalPolytopes(polytopesDependentOnEliminated);
		
		var productOfFactors = Factor.multiply(factors);
		
		var variablesToBeEliminatedOnceSimplexesAreDealtWith = subtract(eliminated, simplexVariables);
		
		var summedOutFactor = productOfFactors.sumOut(variablesToBeEliminatedOnceSimplexesAreDealtWith);
		
		var finalIndices = makeListWithElementsOfTwoCollections(indicesFromIntensionalPolytopes, simplexVariables);
		
		return new IntensionalPolytope(finalIndices, summedOutFactor);
	}

	private static List<Variable> collectSimplexVariables(Collection<? extends Polytope> polytopes) {
		return 
				polytopes.stream()
				.filter(p -> p instanceof Simplex)
				.flatMap(p -> p.getFreeVariables().stream())
				.collect(toList());
	}

	private static List<Variable> collectIndicesFromTheIntensionalPolytopesAmongThese(Collection<? extends Polytope> polytopes) {
		return 
				polytopes.stream()
				.filter(p -> p instanceof IntensionalPolytope)
				.flatMap(c -> ((IntensionalPolytope)c).getIndices().stream())
				.collect(toList());
	}


	private static List<Factor> collectFactorsFromPolytopesThatAreIntensionalPolytopes(Collection<? extends Polytope> polytopes) {
		List<Factor> factors = list();
		for (Polytope polytope : polytopes) {
			collectFactorIfIntensionalPolytope(polytope, factors);
		}
		return factors;
	}


	private static void collectFactorIfIntensionalPolytope(Polytope polytope, List<Factor> factors) {
		if (polytope instanceof IntensionalPolytope) {
			IntensionalPolytope intensionalPolytope = (IntensionalPolytope) polytope;
			factors.add(intensionalPolytope.getFactor());
		}
	}
	
	//////////////////////// GET SINGLE ATOMIC POLYTOPE FOR A VARIABLE
	
	@Override
	public AtomicPolytope getEquivalentAtomicPolytopeOn(Variable variable) {
		if (getFreeVariables().contains(variable)) {
			if (getFreeVariables().size() == 1) {
				// already in desired form, nothing to do
				return this;
			}
			else {
				// sum the other ones out
				return (AtomicPolytope) sumOut(setDifference(getFreeVariables(), list(variable)));
			}
		}
		else {
			// variable is required to be in polytope
			throw new Error("IntensionalPolytope has variables " + getFreeVariables() + " but getEquivalentAtomicPolytopeOn was requested for one not in it: " + variable);
		}
	}	

	@Override
	public AtomicPolytope getEquivalentAtomicPolytopeOn(Variable variable, Collection<? extends AtomicPolytope> atomicPolytopes) {
		Simplex simplexOnVariableIfAny = (Simplex) getFirst(atomicPolytopes, p -> isSimplexOn(p, variable));
		
		boolean thereIsSimplexOnQuerySoItDominates = simplexOnVariableIfAny != null;
		
		AtomicPolytope result;
		if (thereIsSimplexOnQuerySoItDominates) {
			result = simplexOnVariableIfAny;
		}
		else {
			// all atomicPolytopes are non-simplex, or otherwise we would have simplexes on non-query variables and the query would not be the only free variable
			result = makeAtomicPolytopeEquivalentToProductOfNonSimplexAtomicPolytopes(atomicPolytopes);
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

	private static IntensionalPolytope makeAtomicPolytopeEquivalentToProductOfNonSimplexAtomicPolytopes(Collection<? extends AtomicPolytope> nonSimplexAtomicPolytopes) {
		@SuppressWarnings("unchecked")
		Collection<? extends IntensionalPolytope> intensionalPolytopes = (Collection<? extends IntensionalPolytope>) nonSimplexAtomicPolytopes;
		// The only non-simplex atomic polytopes in this implementation are intensional polytopes.
		
		List<Variable> indicesFromIntensionalPolytopes = collectIndicesFromIntensionalPolytopesGivenTheyAreAllIntensionalPolytopes(intensionalPolytopes);
		Factor productOfFactors = makeProductOfFactorsOf(intensionalPolytopes);
		return new IntensionalPolytope(indicesFromIntensionalPolytopes, productOfFactors);
	}

	private static List<Variable> collectIndicesFromIntensionalPolytopesGivenTheyAreAllIntensionalPolytopes(Collection<? extends IntensionalPolytope> intensionalPolytopes) {
		List<Variable> indices = list();
		for (IntensionalPolytope intensionalPolytope : intensionalPolytopes) {
			indices.addAll(intensionalPolytope.getIndices());
		}
		return indices;
	}

	private static Factor makeProductOfFactorsOf(Collection<? extends IntensionalPolytope> intensionalPolytopes) {
		List<Factor> factors = mapIntoList(intensionalPolytopes, IntensionalPolytope::getFactor);
		return Factor.multiply(factors);
	}

	//////////////////////// ANCILLARY
	
	@Override
	public String toString() {
		String indicesString = indices.isEmpty()? "" : "(on " + join(indices) + ") ";
		String result = "{" + indicesString + factor + "}";
		return result;
	}
	
	@Override
	public boolean equals(Object another) {
		boolean result =
				another instanceof IntensionalPolytope
				&&
				((IntensionalPolytope) another).getIndices().equals(getIndices())
				&&
				((IntensionalPolytope) another).getFactor().equals(getFactor());
		return result;
	}
	
	@Override
	public int hashCode() {
		return getIndices().hashCode() + getFactor().hashCode();
	}

}