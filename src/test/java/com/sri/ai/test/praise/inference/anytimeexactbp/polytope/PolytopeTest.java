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
import static com.sri.ai.praise.inference.anytimeexactbp.polytope.api.Polytope.multiply;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.AtomicPolytope;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.Polytope;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.Polytopes;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.Simplex;
import com.sri.ai.praise.inference.representation.expression.ExpressionFactor;
import com.sri.ai.praise.inference.representation.expression.ExpressionVariable;

public class PolytopeTest {

	@Test
	public void test() {
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(
				"U", "Boolean",
				"V", "Boolean");

		ExpressionVariable u = new ExpressionVariable(parse("U"));
		ExpressionVariable v = new ExpressionVariable(parse("V"));
		
		Polytope identity = Polytopes.identityPolytope();
		
		AtomicPolytope simplexU = new Simplex(u);
		AtomicPolytope simplexV = new Simplex(v);
		
		ExpressionFactor factorU = new ExpressionFactor(parse("if U then 2 else 3"), context);
		ExpressionFactor factorV = new ExpressionFactor(parse("if V then 2 else 3"), context);
		ExpressionFactor factorUV = new ExpressionFactor(parse("if U and V then 2 else 3"), context);
		
		ExpressionFactor factorU2 = new ExpressionFactor(parse("if U then 20 else 30"), context);
		ExpressionFactor factorV2 = new ExpressionFactor(parse("if V then 20 else 30"), context);
		ExpressionFactor factorUV2 = new ExpressionFactor(parse("if U and V then 20 else 30"), context);
		
		AtomicPolytope convexHullU = new IntensionalConvexHullOfFactors(list(u), factorU);
		AtomicPolytope convexHullV = new IntensionalConvexHullOfFactors(list(v), factorV);
		AtomicPolytope convexHullUV = new IntensionalConvexHullOfFactors(list(u, v), factorUV);
		
		AtomicPolytope convexHullU2 = new IntensionalConvexHullOfFactors(list(u), factorU2);
		AtomicPolytope convexHullV2 = new IntensionalConvexHullOfFactors(list(v), factorV2);
		AtomicPolytope convexHullUV2 = new IntensionalConvexHullOfFactors(list(u, v), factorUV2);
		
		Polytope expected;
		Polytope actual;

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

}
