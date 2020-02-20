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

import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.listFrom;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.union;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.util.Util;

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
	
public class FunctionConvexHull extends AbstractAtomicPolytope implements AtomicPolytope {
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

	private List<Variable> freeVariables;
	@Override
	public Collection<? extends Variable> getFreeVariables() {
		if (freeVariables == null) {
			List<? extends Variable> all = factor.getVariables();
			freeVariables = subtract(all, getIndices());
		}
		return freeVariables;
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

	//////////////////////// MULTIPLY INTO A SINGLE FUNCTION CONVEX HULL
	
	/**
	 * Multiplies a {@link FunctionConvexHull} from a non-empty collection of {@link FunctionConvexHull}s.
	 * This simply delegates to {@link FunctionConvexHull#multiplyIntoSingleFunctionConvexHull(Collection)} of the first element for convenient.
	 * The reason we have a non-static method is so it can be overridden.
	 * @param functionConvexHulls
	 * @return
	 */
	public static FunctionConvexHull staticMultiplyIntoSingleFunctionConvexHull(Collection<? extends FunctionConvexHull> functionConvexHulls) {
		var first = Util.getFirstOrNull(functionConvexHulls);
		if (first == null) {
			throw new Error(FunctionConvexHull.class + ".staticGetEquivalentAtomicPolytope should not receive empty argument.");
		}
		else {
			return first.multiplyIntoSingleFunctionConvexHull(functionConvexHulls);
		}
	}

	/**
	 * Multiplies {@link FunctionConvexHull} from a non-empty collection of {@link FunctionConvexHull}s.
	 * All other methods should perform such multiplications through this method so that overridden versions take effect.
	 * @param functionConvexHulls
	 * @return
	 */
	protected FunctionConvexHull multiplyIntoSingleFunctionConvexHull(Collection<? extends FunctionConvexHull> functionConvexHulls) {
		var indices = union(functionIterator(functionConvexHulls, FunctionConvexHull::getIndices));
		var factors = mapIntoList(functionConvexHulls, FunctionConvexHull::getFactor);
		var factorsProduct = Factor.multiply(factors);
		return new FunctionConvexHull(indices, factorsProduct);
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