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
package com.sri.ai.praise.inference.anytime.livesets.api;

import static com.sri.ai.praise.inference.anytime.livesets.core.lazymemoryless.ExtensionalSetBound.liveSet;

import java.util.Collection;

import com.google.common.base.Predicate;
import com.sri.ai.praise.inference.anytime.livesets.core.lazymemoryless.ComplementOfSetBound;
import com.sri.ai.praise.inference.anytime.livesets.core.lazymemoryless.IntersectionOfSetBounds;
import com.sri.ai.praise.inference.anytime.livesets.core.lazymemoryless.SubtractionOfSetBounds;
import com.sri.ai.praise.inference.anytime.livesets.core.lazymemoryless.UnionOfSetBounds;

/**
 * A live set is a simple interface for deciding whether an element belong to a set
 * and that automatically reflects modifications to other sets on which it is defined.
 * For example, if a live set is defined as <code>union = set1.union(set2)</code>,
 * modifications to either <code>set1</code> and <code>set2</code> will automatically be reflected
 * in <code>union</code>.
 * <p>
 * Implementations are free to realize this behavior eagerly or lazily,
 * and by keeping elements of a live set stored in memory or using the sets defining a set to decide if elements belong to it.
 * The default methods in the interface use a lazy and memoryless implementation.
 * 
 * @author braz
 *
 * @param <T>
 */
public interface LiveSet<T> {
	
	boolean contains(T element);
	
	default boolean thereIsElementSatisfying(Predicate<T> predicate) {
		// TODO: implement
		return true;
	}
	
	default LiveSet<T> complement() {
		return ComplementOfSetBound.complement(this);
	}
	
	default LiveSet<T> minus(LiveSet<T> another) {
		return SubtractionOfSetBounds.minus(this, another);
	}
	
	default LiveSet<T> minus(Collection<T> elements) {
		return SubtractionOfSetBounds.minus(this, liveSet(elements));
	}
	
	default LiveSet<T> union(LiveSet<T> another) {
		return UnionOfSetBounds.union(this, another);
	}
	
	default LiveSet<T> union(Collection<T> elements) {
		return UnionOfSetBounds.union(this, liveSet(elements));
	}
	
	default LiveSet<T> intersection(LiveSet<T> another) {
		return IntersectionOfSetBounds.intersection(this, another);
	}
	
	default LiveSet<T> intersection(Collection<T> elements) {
		return IntersectionOfSetBounds.intersection(this, liveSet(elements));
	}
}