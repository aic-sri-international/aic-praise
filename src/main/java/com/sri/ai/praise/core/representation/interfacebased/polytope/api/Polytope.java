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
package com.sri.ai.praise.core.representation.interfacebased.polytope.api;

import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.IdentityPolytope.identityPolytope;
import static com.sri.ai.util.Util.accumulate;

import java.util.Arrays;
import java.util.Collection;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck;
import com.sri.ai.util.computation.anytime.api.Approximation;

/**
 * An interface for approximations to factors consisting of a
 * polytope set of factors to which the true factor is guaranteed to belong. 
 * 
 * @author braz
 *
 */
public interface Polytope extends Approximation<Factor> {
	
	Collection<? extends Variable> getFreeVariables();
	
	boolean isIdentity();

	Polytope multiply(Polytope another);

	/**
	 * Returns the atomic polytopes whose product is equivalent to this one.
	 * @return
	 */
	Collection<? extends AtomicPolytope> getAtomicPolytopes();
	
	static Polytope multiply(Collection<? extends Polytope> polytopes) {
		Polytope result = accumulate(polytopes, Polytope::multiply, identityPolytope());
		return result;
	}

	static Polytope product(Polytope... polytopes) {
		Polytope result = accumulate(Arrays.asList(polytopes), Polytope::multiply, identityPolytope());
		return result;
	}

	Polytope sumOut(Collection<? extends Variable> eliminated);
	
	/**
	 * Returns a polytope <code>p</code> such that <code>this == p.sumOut(simplexVariables)</code>
	 * where each element of <code>simplexVariables</code> satisfies the given predicate.
	 * <p>
	 * Intuitively, this is simply a reversal of summing out simplex polytope-factors in a product polytope,
	 * and is useful when we need to reverse summation after learning that some of the simplex variables
	 * were ultimately free variables (the motivating application for this is Anytime Exact Belief Propagation).
	 * <p>
	 * This assumes that the polytope implementation keeps somehow the relevant information,
	 * which may be expensive, and is therefore an optional method.
	 * @param isSimplexVariabledToBeUnSummedOut
	 * @return
	 */
	Polytope unSumOutSimplexVariables(Predicate<? super Variable> isSimplexVariabledToBeUnSummedOut);
	
	/**
	 * Returns an {@link AtomicPolytope} equivalent to this polytope.
	 * Currently supported only for polytopes with a single free variable.
	 */
	AtomicPolytope getEquivalentAtomicPolytope();
	// The general case (more than one free variable) would require a KroneckerDeltaFactor
	// since simplices can be seen as function convex hulls on that kind
	// of factor, and this operation requires all factors to be multiplied.
	// This would be expensive and it is unclear how useful it is it,
	// although it would be nice to support generality.
	// PS: it would be not as expensive if Factors, like polytopes,
	// could be represented by products of atomic factors.
	// Then the KroneckerDeltaFactor becomes one element in a product
	// and can probably be eliminated in more efficient ways downstream.

	/** 
	 * Returns the polytope obtained by normalizing all its factor elements according to the variables provided
	 */
	Polytope normalize(Collection<? extends Variable> variablesToNormalize);

	/**
	 * Assumes this polytope is of the form <code>convex_hull({ phi(i,v) }_i)</code> for <code>v</code> a tuple of variables,
	 * and returns <code>lambda v: (max_i phi(i,v)) - (min_i phi(i,v))</code>,
	 * that is, a function on <code>v</code> for the difference between maximum and minimum probabilities
	 * given to v by each the vertices of the polytope. 
	 * Taking the maximum of this factor is a relatively cheap way to estimate the volume of the polytope
	 * and will be 0 if and only if its volume is 0.   
	 */
	Factor probabilityRange();
	
	/** 
	 * Convenience method for <code>probabilityRange().max(getFreeVariables()).value()</code>,
	 * that is, the maximum probability range over all assignments to the free variables of the polytope.
	 */
	default double length() {
		return probabilityRange().max(getFreeVariables()).value();
	}

	/**
	 * Decides whether this polytope is equal to another one minus permutations in indices, variables, and multiplication factors.
	 */
	boolean equalsModuloPermutations(Object another);
	
	PolytopesEqualityCheck checkEquality(Polytope another);
}
