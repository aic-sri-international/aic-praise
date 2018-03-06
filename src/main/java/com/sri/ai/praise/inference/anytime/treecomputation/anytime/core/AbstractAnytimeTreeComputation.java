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
package com.sri.ai.praise.inference.anytime.treecomputation.anytime.core;

import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.inference.anytime.anytime.api.Anytime;
import com.sri.ai.praise.inference.anytime.anytime.api.Approximation;
import com.sri.ai.praise.inference.anytime.treecomputation.anytime.api.AnytimeTreeComputation;
import com.sri.ai.praise.inference.anytime.treecomputation.anytime.api.ApproximationScheme;
import com.sri.ai.praise.inference.anytime.treecomputation.api.TreeComputation;
import com.sri.ai.util.base.NullaryFunction;

/**
 * An abstract implementation of {@link AnytimeTreeComputation}
 * that borrows most functionality based on a given base {@link TreeComputation}
 * and an {@link ApproximationScheme}.
 * <p>
 * Essentially, this creates an anytime tree computation by
 * lazily expanding, node by node, the base tree computation, and creating
 * anytime versions of them that are used to compute the next approximation
 * to the final root value.
 * <p>
 * Initially, the value of the anytime root computation is
 * the approximation provided by the approximation scheme's
 * {@link ApproximationScheme#totalIgnorance()} method.
 * <p>
 * Then, in order to create the next approximation,
 * this class creates sub-anytime approximations
 * by requesting the base tree computation's children
 * and computing their anytime versions with
 * {@link #makeAnytimeVersion(NullaryFunction)}
 * (which must be implemented by extending classes).
 * The sub-anytime tree computations's approximations
 * are then used by the approximation scheme to compute the new
 * approximation to the root, by also using the base tree computation's function.
 * <p>
 * To update an even better next approximation,
 * the class picks the next anytime sub-computation
 * by using the abstract method {@link #pickNextSubWithNext()},
 * which is also to be specified by extensions.
 * <p>
 * It then iterates the picked next anytime sub-computation,
 * obtaining a new approximation to the corresponding argument,
 * and delegates the computation of the new approximation
 * of the root anytime tree computation to the approximation scheme.
 *
 * @author braz
 *
 * @param <T>
 */
public abstract class AbstractAnytimeTreeComputation<T> implements AnytimeTreeComputation<T> {
	
	protected abstract Anytime<T> makeAnytimeVersion(NullaryFunction<T> baseSub);

	protected abstract Anytime<T> pickNextSubWithNext();

	private TreeComputation<T> base;
	private ApproximationScheme<T> approximationScheme;
	private ArrayList<? extends Anytime<T>> subs;
	private Approximation<T> currentApproximation;
	
	public AbstractAnytimeTreeComputation(TreeComputation<T> base, ApproximationScheme<T> approximationScheme) {
		this.base = base;
		this.approximationScheme = approximationScheme;
		this.subs = null;
		this.currentApproximation = approximationScheme.totalIgnorance();
	}
	
	public TreeComputation<T> getBase() {
		return base;
	}

	public Approximation<T> getCurrentApproximation() {
		return currentApproximation;
	}

	@Override
	public ArrayList<Anytime<T>> getSubs() {
		ArrayList<Anytime<T>> result = mapIntoArrayList(base.getSubs(), this::makeAnytimeVersion);
		return result;
	}

	@Override
	public boolean hasNext() {
		boolean result = pickNextSubWithNext() != null;
		return result;
	}
	
	@Override
	public Approximation<T> next() {
		Anytime<T> nextSub = pickNextSubWithNext();
		myAssert(nextSub != null, () -> this.getClass() + ": next invoked when hasNext is false");
		nextSub.next();
		List<Approximation<T>> subsApproximations = mapIntoArrayList(subs, Anytime::getCurrentApproximation); 
		currentApproximation = function(subsApproximations);
		return currentApproximation;
	}

	@Override
	public Approximation<T> function(List<Approximation<T>> subsApproximations) {
		Approximation<T> approximation = approximationScheme.apply(base::function, subsApproximations);
		return approximation;
	}
}