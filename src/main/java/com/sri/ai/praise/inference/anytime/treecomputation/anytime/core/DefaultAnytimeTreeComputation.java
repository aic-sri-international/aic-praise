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

import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;

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
public class DefaultAnytimeTreeComputation<T> extends AbstractAnytimeTreeComputation<T> {
	
	public DefaultAnytimeTreeComputation(TreeComputation<T> base) {
		super(base);
	}
	
	@Override
	protected Bound<T> makeInitialBound() {
		return null; // TODO
	}

	@Override
	protected AnytimeTreeComputation<T> makeAnytimeVersionOfBaseSub(TreeComputation<T> sub) {
		Constructor<?> constructor = getClass().getConstructors()[0];
		try {
			@SuppressWarnings("unchecked")
			AnytimeTreeComputation<T> result = (AnytimeTreeComputation<T>) constructor.newInstance(sub);
			return result;
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new Error(e);
		}
	}

	@Override
	protected Anytime<T> pickNextSubWithNext() {
		Anytime<T> result = getFirstSatisfyingPredicateOrNull(getSubs(), Iterator<Bound<T>>::hasNext);
		return result;
	}

	@Override
	protected Bound<T> computeBoundFromSubsBoundsUsingBaseComputation(ArrayList<? extends Bound<T>> subsBounds, Function<ArrayList<? extends T>, T> computeValueFromSubsValues) {
		return null; // TODO
	}
}