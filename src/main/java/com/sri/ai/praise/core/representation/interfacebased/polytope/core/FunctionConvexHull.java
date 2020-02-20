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

import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.ProductPolytope.makeProductPolytopeFromAlreadySimplifiedAtomicPolytopes;
import static com.sri.ai.util.Util.collect;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.listFrom;
import static com.sri.ai.util.Util.makeListWithElementsOfTwoCollections;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.Util.subtract;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.NonSimplexAtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;

/**
 * A polytope equal to the convex hull of points provided by a {@link Factor},
 * of which certain arguments (provided by {@link #getIndices()}) represent the indices of vertices
 * and the remaining ones (provided by {@link #getFreeVariables()}) represent the dimensions of the vectors
 * in the polytope.
 * <p>
 * More precisely, given factor <code>phi(I,V)</code> where <code>I</code> are the indices and <code>V</code>
 * are the remaining variables, the represented polytope is the convex hull of <code>{ phi(I,V) }_I</code>.
 * 
 * @author braz
 *
 */
	
public class FunctionConvexHull extends AbstractAtomicPolytope implements NonSimplexAtomicPolytope {
	private Set<? extends Variable> indices;

	private Factor factor;
	
	public FunctionConvexHull(Collection<? extends Variable> indices, Factor factor) {
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
		if (anotherAtomicPolytope instanceof FunctionConvexHull) {
			result = multiplyByFunctionConvexHullIfSameIndicesOrNull(anotherAtomicPolytope);
		}
		else {
			result = null;
		}
		return result;
	}

	private AtomicPolytope multiplyByFunctionConvexHullIfSameIndicesOrNull(AtomicPolytope another) {
		AtomicPolytope result;
		FunctionConvexHull anotherFunctionConvexHull = (FunctionConvexHull) another;
		if (indices.equals(anotherFunctionConvexHull.getIndices())) {
			Factor productFactor = factor.multiply(anotherFunctionConvexHull.getFactor());
			result = new FunctionConvexHull(indices, productFactor);
		}
		else {
			result = null;
		}
		return result;
		
		// We could multiply function convex hulls with *unifiable* indices, that is,
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
			return new FunctionConvexHull(getIndices(), newFactor);
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
	public Polytope sumOutFromDependentAtomicPolytopes(Collection<? extends Variable> eliminated, Collection<? extends AtomicPolytope> polytopesDependentOnEliminated) {

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
		
		List<AtomicPolytope> simplices = list();
		List<AtomicPolytope> functionConvexHulls = list();
		collect(polytopesDependentOnEliminated, p -> p instanceof Simplex, simplices, functionConvexHulls);

		@SuppressWarnings("unchecked")
		var simplexVariables = mapIntoList((List<? extends Simplex>) simplices, Simplex::getVariable);

		var functionConvexHullsProduct = makeProductPolytopeFromAlreadySimplifiedAtomicPolytopes(functionConvexHulls);
		var atomicFunctionConvexHullsProduct = (FunctionConvexHull) functionConvexHullsProduct.getEquivalentAtomicPolytope();
		
		var eliminatedMinusSimplexVariables = subtract(eliminated, simplexVariables);
		var atomicFunctionConvexHullsProductWithoutEliminatedMinusSimplexVariables = (FunctionConvexHull) 
				atomicFunctionConvexHullsProduct.sumOut(eliminatedMinusSimplexVariables);

		var finalIndices = makeListWithElementsOfTwoCollections(atomicFunctionConvexHullsProduct.getIndices(), simplexVariables);
		var result = new FunctionConvexHull(finalIndices, atomicFunctionConvexHullsProductWithoutEliminatedMinusSimplexVariables.getFactor());
		
		return result;
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
			throw new Error("FunctionConvexHull has variables " + getFreeVariables() + " but getEquivalentAtomicPolytopeOn was requested for one not in it: " + variable);
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

	private static FunctionConvexHull makeAtomicPolytopeEquivalentToProductOfNonSimplexAtomicPolytopes(Collection<? extends AtomicPolytope> nonSimplexAtomicPolytopes) {
		@SuppressWarnings("unchecked")
		Collection<? extends FunctionConvexHull> functionConvexHulls = (Collection<? extends FunctionConvexHull>) nonSimplexAtomicPolytopes;
		// The only non-simplex atomic polytopes in this implementation are function convex hulls.
		
		List<Variable> indicesFromFunctionConvexHulls = collectIndicesFromFunctionConvexHullsGivenTheyAreAllFunctionConvexHulls(functionConvexHulls);
		Factor productOfFactors = makeProductOfFactorsOf(functionConvexHulls);
		return new FunctionConvexHull(indicesFromFunctionConvexHulls, productOfFactors);
	}

	private static List<Variable> collectIndicesFromFunctionConvexHullsGivenTheyAreAllFunctionConvexHulls(Collection<? extends FunctionConvexHull> functionConvexHulls) {
		List<Variable> indices = list();
		for (FunctionConvexHull functionConvexHull : functionConvexHulls) {
			indices.addAll(functionConvexHull.getIndices());
		}
		return indices;
	}

	private static Factor makeProductOfFactorsOf(Collection<? extends FunctionConvexHull> functionConvexHulls) {
		List<Factor> factors = mapIntoList(functionConvexHulls, FunctionConvexHull::getFactor);
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
				another instanceof FunctionConvexHull
				&&
				((FunctionConvexHull) another).getIndices().equals(getIndices())
				&&
				((FunctionConvexHull) another).getFactor().equals(getFactor());
		return result;
	}
	
	@Override
	public int hashCode() {
		return getIndices().hashCode() + getFactor().hashCode();
	}

}