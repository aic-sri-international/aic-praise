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
package com.sri.ai.test.praise.core.representation.interfacebased.polytope;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck.factorsHaveDifferentValues;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope.multiply;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.firstPolytopeHasFunctionConvexHullWithoutMatchInSecond;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.functionConvexHullsHaveDifferentFactors;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.functionConvexHullsHaveDifferentIndices;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.polytopesAreEqual;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.polytopesAreOfIncomparableClasses;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.polytopesHaveADifferentNumberOfFunctionConvexHulls;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck.polytopesHaveDifferentSimplices;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.IdentityPolytope.identityPolytope;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.set;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.jupiter.api.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.DefaultFunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.ProductPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.Simplex;

public class PolytopeTest {

	Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(
			"U", "Boolean",
			"V", "Boolean",
			"W", "Boolean");

	ExpressionVariable u = DefaultExpressionVariable.expressionVariable(parse("U"));
	ExpressionVariable v = DefaultExpressionVariable.expressionVariable(parse("V"));
	ExpressionVariable w = DefaultExpressionVariable.expressionVariable(parse("W"));
	
	Polytope identity = identityPolytope();
	
	AtomicPolytope simplexU = new Simplex(u);
	AtomicPolytope simplexV = new Simplex(v);
	AtomicPolytope simplexW = new Simplex(w);
	
	ExpressionFactor factorU = new DefaultExpressionFactor(parse("if U then 2 else 3"), context);
	ExpressionFactor factorV = new DefaultExpressionFactor(parse("if V then 2 else 3"), context);
	ExpressionFactor factorUV = new DefaultExpressionFactor(parse("if U and V then 2 else 3"), context);
	ExpressionFactor factorVW = new DefaultExpressionFactor(parse("if V and W then 4 else 5"), context);
	
	ExpressionFactor factorU2 = new DefaultExpressionFactor(parse("if U then 20 else 30"), context);
	ExpressionFactor factorV2 = new DefaultExpressionFactor(parse("if V then 20 else 30"), context);
	ExpressionFactor factorUV2 = new DefaultExpressionFactor(parse("if U and V then 20 else 30"), context);
	
	AtomicPolytope convexHullU = new DefaultFunctionConvexHull(list(u), factorU);
	AtomicPolytope convexHullV = new DefaultFunctionConvexHull(list(v), factorV);
	AtomicPolytope convexHullUV = new DefaultFunctionConvexHull(list(u, v), factorUV);
	AtomicPolytope convexHullVW = new DefaultFunctionConvexHull(list(v, w), factorVW);
	
	AtomicPolytope convexHullU2 = new DefaultFunctionConvexHull(list(u), factorU2);
	AtomicPolytope convexHullV2 = new DefaultFunctionConvexHull(list(v), factorV2);
	AtomicPolytope convexHullUV2 = new DefaultFunctionConvexHull(list(u, v), factorUV2);
	
	AtomicPolytope convexHullUFreeVBound = new DefaultFunctionConvexHull(list(v), factorUV);
	AtomicPolytope convexHullUBoundVFree = new DefaultFunctionConvexHull(list(u), factorUV);
	AtomicPolytope convexHullUFreeVBound2 = new DefaultFunctionConvexHull(list(v), factorUV);
	AtomicPolytope convexHullUBoundVFree2 = new DefaultFunctionConvexHull(list(u), factorUV);
	
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
		actual = product.getEquivalentAtomicPolytopeOn(u);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		product = Polytope.multiply(list(simplexU, convexHullUV));
		actual = product.getEquivalentAtomicPolytopeOn(u);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		product = Polytope.multiply(list(simplexU, convexHullUFreeVBound, convexHullVW));
		actual = product.getEquivalentAtomicPolytopeOn(u);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		product = Polytope.multiply(list(simplexU, convexHullUV));
		actual = product.getEquivalentAtomicPolytopeOn(u);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		product = Polytope.multiply(list(simplexU, convexHullUFreeVBound));
		actual = product.getEquivalentAtomicPolytopeOn(u);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU;
		assertEquals(expected, actual);
		
		product = Polytope.multiply(list(simplexU, convexHullUBoundVFree));
		actual = product.getEquivalentAtomicPolytopeOn(u);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		expected = simplexU; // simplexU dominates
		assertEquals(expected, actual);

		product = Polytope.multiply(list(convexHullUFreeVBound, convexHullVW));
		actual = product.getEquivalentAtomicPolytopeOn(u);
		println("Atomic polytope on u equivalent to " + product + ": " + actual);
		ExpressionFactor expectedExpressionFactor = new DefaultExpressionFactor(parse("if U then if V then if W then 8 else 10 else 15 else if V then if W then 12 else 15 else 15"), context);
		expected = new DefaultFunctionConvexHull(list(v,w), expectedExpressionFactor);
		println(expected.toString());
		println(actual.toString());
		assertEquals(expected.toString(), actual.toString()); // factor are compared by reference, not value
	}
	
	@Test
	public void testMathematicalEquivalence() {

		TableVariable u = new TableVariable("U", 2);
		TableVariable v = new TableVariable("V", 2);
		TableVariable x = new TableVariable("X", 2);
		TableVariable y = new TableVariable("Y", 2);
		TableVariable z = new TableVariable("Z", 2);
		
		Polytope p1;
		Polytope p2;
		
		p1 = new Simplex(x);
		p2 = new Simplex(y);
		assertTrue(p1.equalsModuloPermutations(p1));
		assertFalse(p1.equalsModuloPermutations(p2));
		
		// Note: Factor.mathematicallyEquals is tested more extensively in {@link TableFactorTest}.
		
		
		p1 = new DefaultFunctionConvexHull(
				list(x, y), 
				new ArrayTableFactor(list(u,v), new double[] {0., 0., 0., 0., }));
		p2 = new DefaultFunctionConvexHull(
				list(x, y), 
				new ArrayTableFactor(list(u,v), new double[] {1., 0., 0., 0., }));
		assertTrue(p1.equalsModuloPermutations(p1));
		assertFalse(p1.equalsModuloPermutations(p2));
		
		
		p1 = new DefaultFunctionConvexHull(
				list(x, y), 
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		p2 = new DefaultFunctionConvexHull(
				list(y, x), // inverse order
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		assertTrue(p1.equalsModuloPermutations(p2));
		
		
		p1 = new DefaultFunctionConvexHull(
				list(), 
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		p2 = new DefaultFunctionConvexHull(
				list(),
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		assertTrue(p1.equalsModuloPermutations(p2));
		
		
		p1 = new DefaultFunctionConvexHull(
				list(x, y), 
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		p2 = new DefaultFunctionConvexHull(
				list(),
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		assertFalse(p1.equalsModuloPermutations(p2));
		
		
		p1 = new DefaultFunctionConvexHull(
				list(x, y), 
				new ArrayTableFactor(
						list(x,y,z), 
						new double[] {1., 2., 3., 4., 5., 6., 7., 8., }));
		p2 = new DefaultFunctionConvexHull(
				list(x, y),
				new ArrayTableFactor(
						list(z,x,y) /* permutation*/, 
						new double[] {1., 3., 5., 7., 2., 4., 6., 8., }));
		assertTrue(p1.equalsModuloPermutations(p2));
		
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		assertTrue(p1.equalsModuloPermutations(p2));
		
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., })),
				new Simplex(u),
				new Simplex(v)
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(v),
				new Simplex(u),
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		assertTrue(p1.equalsModuloPermutations(p2));
		
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., })),
				new Simplex(v)
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(v),
				new Simplex(u),
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		assertFalse(p1.equalsModuloPermutations(p2));
		
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(u),
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., })),
				new Simplex(v),
				new DefaultFunctionConvexHull(
						list(x), 
						new ArrayTableFactor(
								list(z), 
								new double[] {1., 2.}))
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x), 
						new ArrayTableFactor(
								list(z), 
								new double[] {1., 2.})),
				new Simplex(v),
				new Simplex(u),
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		assertTrue(p1.equalsModuloPermutations(p2));
	}

	@Test
	public void testCheckEquality() {

		TableVariable u = new TableVariable("U", 2);
		TableVariable v = new TableVariable("V", 2);
		TableVariable x = new TableVariable("X", 2);
		TableVariable y = new TableVariable("Y", 2);
		TableVariable z = new TableVariable("Z", 2);
		
		Polytope p1;
		Polytope p2;
		PolytopesEqualityCheck expected;
		PolytopesEqualityCheck actual;
		
		p1 = new Simplex(x);
		expected = PolytopesEqualityCheck.polytopesAreEqual(p1, p1);
		actual = p1.checkEquality(p1);
		assertEquals(expected, actual);
		
		p1 = new Simplex(x);
		p2 = new Simplex(y);
		expected = PolytopesEqualityCheck.polytopesAreSimplicesOnDifferentVariables(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = new DefaultFunctionConvexHull(
				list(x, y), 
				new ArrayTableFactor(list(u,v), new double[] {0., 0., 0., 0., }));
		p2 = new DefaultFunctionConvexHull(
				list(x, y), 
				new ArrayTableFactor(list(u,v), new double[] {1., 0., 0., 0., }));
		expected = polytopesAreEqual(p1, p1);
		actual = p1.checkEquality(p1);
		assertEquals(expected, actual);

		FunctionConvexHull f1 = (FunctionConvexHull) p1;
		FunctionConvexHull f2 = (FunctionConvexHull) p2;
		expected = functionConvexHullsHaveDifferentFactors(p1, p2, f1.getFactor().checkEquality(f2.getFactor()));
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = new DefaultFunctionConvexHull(
				list(x, y), 
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		p2 = new DefaultFunctionConvexHull(
				list(y, x), // inverse order
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		expected = polytopesAreEqual(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = new DefaultFunctionConvexHull(
				list(), 
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		p2 = new DefaultFunctionConvexHull(
				list(),
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		expected = polytopesAreEqual(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = new DefaultFunctionConvexHull(
				list(), 
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		p2 = new DefaultFunctionConvexHull(
				list(x, y),
				new ArrayTableFactor(list(x,y,z), new double[] {0., 0., 0., 0., 0., 0., 0., 0., }));
		expected = functionConvexHullsHaveDifferentIndices(p1, p2, set(), set(x,y));
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = new DefaultFunctionConvexHull(
				list(x, y), 
				new ArrayTableFactor(
						list(x,y,z), 
						new double[] {1., 2., 3., 4., 5., 6., 7., 8., }));
		p2 = new DefaultFunctionConvexHull(
				list(x, y),
				new ArrayTableFactor(
						list(z,x,y) /* permutation*/, 
						new double[] {1., 3., 5., 7., 2., 4., 6., 8., }));
		expected = polytopesAreEqual(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);

		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		expected = polytopesAreEqual(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., })),
				new Simplex(u),
				new Simplex(v)
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(v),
				new Simplex(u),
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		expected = polytopesAreEqual(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., })),
				new Simplex(v)
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(v),
				new Simplex(u),
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		expected = polytopesHaveDifferentSimplices(p1, p2, set(), set(u));
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(u),
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., })),
				new Simplex(v),
				new DefaultFunctionConvexHull(
						list(x), 
						new ArrayTableFactor(
								list(z), 
								new double[] {1., 2.}))
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new DefaultFunctionConvexHull(
						list(x), 
						new ArrayTableFactor(
								list(z), 
								new double[] {1., 2.})),
				new Simplex(v),
				new Simplex(u),
				new DefaultFunctionConvexHull(
						list(x, y), 
						new ArrayTableFactor(
								list(x,y,z), 
								new double[] {1., 2., 3., 4., 5., 6., 7., 8., }))
				);
		expected = polytopesAreEqual(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);

		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes();
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes();
		expected = polytopesAreEqual(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(u)
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(); // becomes IdentityPolytope
		expected = polytopesAreOfIncomparableClasses(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(x),
				new Simplex(u)
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(x),
				new Simplex(v)
				);
		expected = PolytopesEqualityCheck.polytopesHaveDifferentSimplices(p1, p2, set(u), set(v));
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		// Next tests use f1 and f2 defined above
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				f2,
				new Simplex(x),
				f1,
				new Simplex(x),
				f1,
				new Simplex(y),
				f2
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(x),
				f2,
				f2,
				new Simplex(y),
				f1,
				f1
				);
		expected = polytopesAreEqual(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				f2,
				new Simplex(x),
				f1,
				new Simplex(x),
				f1,
				new Simplex(y),
				f2
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(x),
				f2,
				f2,
				new Simplex(y),
				f1,
				f2 // <-------- extra f2 instead of an f1
				);
		expected = firstPolytopeHasFunctionConvexHullWithoutMatchInSecond(
				p1, p2, f1,
				set(
						functionConvexHullsHaveDifferentFactors(
								f1, f2, 
								factorsHaveDifferentValues(
										f1.getFactor(), f2.getFactor(), 
										list(0,0), 0.0, 1.0))
						)
				);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);
		
		p1 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(x),
				new Simplex(y),
				f2
				);
		p2 = ProductPolytope.makePolytopeEquivalentToProductOfAtomicPolytopes(
				new Simplex(x),
				f2,
				new Simplex(y),
				f1
				);
		expected = polytopesHaveADifferentNumberOfFunctionConvexHulls(p1, p2);
		actual = p1.checkEquality(p2);
		assertEquals(expected, actual);

		
	}
	
}
