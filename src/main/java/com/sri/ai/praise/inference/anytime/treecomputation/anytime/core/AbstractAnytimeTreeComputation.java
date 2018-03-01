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

import com.google.common.base.Function;
import com.sri.ai.praise.inference.anytime.treecomputation.anytime.api.Anytime;
import com.sri.ai.praise.inference.anytime.treecomputation.anytime.api.AnytimeTreeComputation;
import com.sri.ai.praise.inference.anytime.treecomputation.anytime.api.Bound;
import com.sri.ai.praise.inference.anytime.treecomputation.api.TreeComputation;

/**
 * @author braz
 *
 * @param <T>
 */
public abstract class AbstractAnytimeTreeComputation<T> implements AnytimeTreeComputation<T> {
	
	protected abstract Bound<T> makeInitialBound();

	protected abstract AnytimeTreeComputation<T> makeAnytimeVersionOfBaseSub(TreeComputation<T> baseSub);

	protected abstract Anytime<T> pickNextSubWithNext();

	protected abstract Bound<T> computeBoundFromSubsBoundsUsingBaseComputation(
			ArrayList<? extends Bound<T>> subsBounds, 
			Function<ArrayList<? extends T>, T> computeBaseValueFromBaseSubsValues);

	private TreeComputation<T> base;
	private ArrayList<? extends AnytimeTreeComputation<T>> subs;
	private Bound<T> currentBound;
	
	public AbstractAnytimeTreeComputation(TreeComputation<T> base) {
		this.base = base;
		this.subs = null;
		this.currentBound = makeInitialBound();
	}
	
	public TreeComputation<T> getBase() {
		return base;
	}

	public Bound<T> getCurrentBound() {
		return currentBound;
	}

	public Bound<T> apply() {
		while (hasNext()) {
			next();
		}
		return getCurrentBound();
	}
	
	@Override
	public ArrayList<AnytimeTreeComputation<T>> getSubs() {
		ArrayList<AnytimeTreeComputation<T>> result = 
				mapIntoArrayList(base.getSubs(), s -> makeAnytimeVersionOfBaseSub(s));
		return result;
	}

	@Override
	public boolean hasNext() {
		boolean result = pickNextSubWithNext() != null;
		return result;
	}
	
	@Override
	public Bound<T> next() {
		Anytime<T> nextSub = pickNextSubWithNext();
		myAssert(nextSub != null, () -> this.getClass() + ": next invoked when hasNext is false");
		nextSub.next();
		ArrayList<Bound<T>> subsBounds = mapIntoArrayList(subs, AnytimeTreeComputation::getCurrentBound); 
		currentBound = computeValueFromSubsValues(subsBounds);
		return currentBound;
	}

	@Override
	public Bound<T> computeValueFromSubsValues(ArrayList<? extends Bound<T>> subsBounds) {
		Function<ArrayList<? extends T>, T>
		computeBaseValueFromBaseSubsValues = baseSubsValues -> base.computeValueFromSubsValues(baseSubsValues);
		Bound<T> bound = computeBoundFromSubsBoundsUsingBaseComputation(subsBounds, computeBaseValueFromBaseSubsValues);
		return bound;
	}
}