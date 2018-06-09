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
package com.sri.ai.test.praise.inference.anytimeexactbp.polytope;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope.multiply;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.Polytopes;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.Simplex;

public class PolytopeTest {

	Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(
			"U", "Boolean",
			"V", "Boolean",
			"W", "Boolean");

	ExpressionVariable u = new ExpressionVariable(parse("U"));
	ExpressionVariable v = new ExpressionVariable(parse("V"));
	ExpressionVariable w = new ExpressionVariable(parse("W"));
	
	Polytope identity = Polytopes.identityPolytope();
	
	AtomicPolytope simplexU = new Simplex(u);
	AtomicPolytope simplexV = new Simplex(v);
	AtomicPolytope simplexW = new Simplex(w);
	
	ExpressionFactor factorU = new ExpressionFactor(parse("if U then 2 else 3"), context);
	ExpressionFactor factorV = new ExpressionFactor(parse("if V then 2 else 3"), context);
	ExpressionFactor factorUV = new ExpressionFactor(parse("if U and V then 2 else 3"), context);
	ExpressionFactor factorVW = new ExpressionFactor(parse("if V and W then 4 else 5"), context);
	
	ExpressionFactor factorU2 = new ExpressionFactor(parse("if U then 20 else 30"), context);
	ExpressionFactor factorV2 = new ExpressionFactor(parse("if V then 20 else 30"), context);
	ExpressionFactor factorUV2 = new ExpressionFactor(parse("if U and V then 20 else 30"), context);
	
	AtomicPolytope convexHullU = new IntensionalConvexHullOfFactors(list(u), factorU);
	AtomicPolytope convexHullV = new IntensionalConvexHullOfFactors(list(v), factorV);
	AtomicPolytope convexHullUV = new IntensionalConvexHullOfFactors(list(u, v), factorUV);
	AtomicPolytope convexHullVW = new IntensionalConvexHullOfFactors(list(v, w), factorVW);
	
	AtomicPolytope convexHullU2 = new IntensionalConvexHullOfFactors(list(u), factorU2);
	AtomicPolytope convexHullV2 = new IntensionalConvexHullOfFactors(list(v), factorV2);
	AtomicPolytope convexHullUV2 = new IntensionalConvexHullOfFactors(list(u, v), factorUV2);
	
	AtomicPolytope convexHullUFreeVBound = new IntensionalConvexHullOfFactors(list(v), factorUV);
	AtomicPolytope convexHullUBoundVFree = new IntensionalConvexHullOfFactors(list(u), factorUV);
	AtomicPolytope convexHullUFreeVBound2 = new IntensionalConvexHullOfFactors(list(v), factorUV);
	AtomicPolytope convexHullUBoundVFree2 = new IntensionalConvexHullOfFactors(list(u), factorUV);
	
	Polytope product;

	Polytope expected;
	Polytope actual;
	
	@Test
	public void test() {
		actual = identity.multiply(identity);
		expected = multiply(list(identity));
		runTest(expected, actual);

		actual = identity.multiply(simplexU);
		expected = multiply(list(simplexU));
		runTest(expected, actual);

		actual = identity.multiply(convexHullU);
		expected = multiply(list(convexHullU));
		runTest(expected, actual);

		actual = identity.multiply(simplexU).multiply(simplexV);
		expected = multiply(list(simplexU, simplexV));
		runTest(expected, actual);

		actual = identity.multiply(convexHullU).multiply(convexHullV);
		expected = multiply(list(convexHullU, convexHullV));
		runTest(expected, actual);

		actual = identity.multiply(simplexU).multiply(simplexV).multiply(convexHullU);
		expected = multiply(list(simplexU, simplexV, convexHullU));
		runTest(expected, actual);

		actual = identity
				.multiply(simplexU).multiply(simplexV).multiply(convexHullU)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullU);
		expected = multiply(list(simplexU, simplexV, convexHullU, convexHullU));
		runTest(expected, actual);

		actual = identity
				.multiply(simplexU).multiply(simplexV).multiply(convexHullU)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullU)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullU2)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullU2)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullV)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullV)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullV2)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullV2)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullUV)
				.multiply(simplexU).multiply(simplexV).multiply(convexHullUV2)
				;
		expected = multiply(list(
				simplexU, simplexV, 
				convexHullU, convexHullU, convexHullU2, convexHullU2,
				convexHullV, convexHullV, convexHullV2, convexHullV2,
				convexHullUV, convexHullUV2));
		runTest(expected, actual);
	}

	private void runTest(Polytope expected, Polytope actual) {
		println("expected: " + expected);
		println("actual  : " + actual);
		assertEquals(expected, actual);
	}
	
	@Test
	public void testGetEquivalentAtomicPolytope() {
		
		product = Polytope.multiply(list(simplexU));
		actual = Polytopes.getEquivalentAtomicPolytopeOn(u, product);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		product = Polytope.multiply(list(simplexU, convexHullUV));
		actual = Polytopes.getEquivalentAtomicPolytopeOn(u, product);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		product = Polytope.multiply(list(simplexU, convexHullUFreeVBound, convexHullVW));
		actual = Polytopes.getEquivalentAtomicPolytopeOn(u, product);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		product = Polytope.multiply(list(simplexU, convexHullUV));
		actual = Polytopes.getEquivalentAtomicPolytopeOn(u, product);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		product = Polytope.multiply(list(simplexU, convexHullUFreeVBound));
		actual = Polytopes.getEquivalentAtomicPolytopeOn(u, product);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		try {
			product = Polytope.multiply(list(simplexU, convexHullUBoundVFree));
			actual = Polytopes.getEquivalentAtomicPolytopeOn(u, product);
			fail("Should have failed because V is free in polytope but query is U");
		}
		catch (AssertionError e) {
			if ( ! e.getMessage().contains("free variables")) {
				fail("Should have complained about free variables");
			}
		}

		product = Polytope.multiply(list(convexHullUFreeVBound, convexHullVW));
		actual = Polytopes.getEquivalentAtomicPolytopeOn(u, product);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		ExpressionFactor expectedExpressionFactor = new ExpressionFactor(parse("if U then if V then if W then 8 else 10 else 15 else if V then if W then 12 else 15 else 15"), context);
		expected = new IntensionalConvexHullOfFactors(list(v,w), expectedExpressionFactor);
		println(expected.toString());
		println(actual.toString());
		assertEquals(expected.toString(), actual.toString()); // factor are compared by reference, not value
	}

}
