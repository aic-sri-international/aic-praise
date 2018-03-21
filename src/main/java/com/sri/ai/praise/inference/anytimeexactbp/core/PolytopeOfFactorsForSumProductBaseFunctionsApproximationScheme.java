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
package com.sri.ai.praise.inference.anytimeexactbp.core;

import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.anytimeexactbp.api.PolytopeOfFactors;
import com.sri.ai.praise.inference.exactbp.api.ExactBP;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.computation.anytime.api.Approximation;
import com.sri.ai.util.computation.treecomputation.anytime.api.ApproximationScheme;
import com.sri.ai.util.computation.treecomputation.api.TreeComputation;

/**
 * An {@link ApproximationScheme} for {@link PolytopeOfFactors} {@link Approximation}s,
 * restricted to base functions that are summations over the product of their arguments
 * (the type of function used in {@link ExactBP}.
 * <p>
 * For a convex function <code>f</code> and polytopes <code>A_i</code> containing arguments
 * <code>a_i</code> for <code>i=1,...,n</code>,
 * we know that <code>f(a_1,...,a_n) \in convexHull({ f(v_1,...,v_n) | v_i one of the vertices of A_i, i = 1,...,n })</code>,
 * so the latter is returned as a bound on the operation.
 * <p>
 * Naturally, some of <code>A_i</code> may be simplexes.
 * In this library, we have made the design decision of not storing the variables of simplexes in their objects.
 * Instead, those variables must be known by the code using those {@link Simplex} objects.
 * This is similar to the design option of representing factors by numeric expressions: <code>if X = 1 then 0.8 else 0.2</code>
 * can be a factor on <code>X</code>, but also on <code>X, Y</code> with <code>Y</code> being irrelevant.
 * <p>
 * The derivation for the bound on <code>f(a_1,...,a_n)</code> would require us to iterate over the vertices of simplexes.
 * However, it turns out that simplexes in sum-product functions can be ignored.
 * This makes the operation more efficient and also spares us from the bookkeeping required to keep track of which
 * variables the simplex is defined on. The demonstration of this fact is not trivial and will be demonstrated on
 * the related publications.
 * 
 * @author braz
 *
 */
public class PolytopeOfFactorsForSumProductBaseFunctionsApproximationScheme implements ApproximationScheme<Factor> {

	@Override
	public Approximation<Factor> totalIgnorance(TreeComputation<Factor> components) {
		return new Simplex();
	}

	@Override
	public Approximation<Factor> apply(Function<List<Factor>, Factor> function, List<? extends Approximation<Factor>> argumentApproximations) {
	
		Iterator<ArrayList<Factor>> iteratorToVerticesCombinationOfNonSimplexArguments = 
				makeIteratorForVerticesCombinations(argumentApproximations);
		
		ArrayList<Factor> verticesOfResultBound = 
				mapIntoArrayList(iteratorToVerticesCombinationOfNonSimplexArguments, function::apply);
		// Note that we possibly pass a smaller number of arguments to function than we received approximations for.
		// This is is valid because of the known sum-product structure of the base functions.
		
		PolytopeOfFactors result = new ConvexHullOfSomeFactors(verticesOfResultBound);
		return result;
	}

	private static Iterator<ArrayList<Factor>> makeIteratorForVerticesCombinations(List<? extends Approximation<Factor>> argumentApproximations) {
		
		ArrayList<? extends Approximation<Factor>> nonSimplexBoundsAmongArguments = 
				collectToArrayList(argumentApproximations, a -> !(a instanceof Simplex));
		
		ArrayList<NullaryFunction<Iterator<Factor>>> listForEachArgumentOfMakerOfIteratorForVerticesList = 
				mapIntoArrayList(nonSimplexBoundsAmongArguments, makeIteratorOnVertices());
		
		Iterator<ArrayList<Factor>> iteratorOnVerticesCombinations = 
				new CartesianProductIterator<>(listForEachArgumentOfMakerOfIteratorForVerticesList);
		
		return iteratorOnVerticesCombinations;
	}

	private static Function<? super Approximation<Factor>, NullaryFunction<Iterator<Factor>>> makeIteratorOnVertices() {
		return b -> () -> ((ConvexHullOfSomeFactors)b).getVertices().iterator();
	}
}
