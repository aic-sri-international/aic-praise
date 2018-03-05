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
package com.sri.ai.test.praise.inference.anytime.treecomputation.api;

import static com.sri.ai.util.Util.arrayList;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.praise.inference.anytime.treecomputation.api.TreeComputation;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;

/**
 * @author braz
 *
 */
public class TreeComputationTest {
	
	static class OneTwoTreeComputation implements TreeComputation<Integer> {
		
		private int depth;
		
		public OneTwoTreeComputation(int depth) {
			this.depth = depth;
		}

		@Override
		public ArrayList<? extends NullaryFunction<Integer>> getSubs() {
			if (depth == 0) {
				return arrayList(
						() -> 1,
						() -> 2,
						() -> 3
						);
			}
			else {
				return arrayList(
						new OneTwoTreeComputation(depth - 1),
						new OneTwoTreeComputation(depth - 1),
						new OneTwoTreeComputation(depth - 1)
						);
			}
		}

		@Override
		public Integer function(List<Integer> subsValues) {
			return (Integer) Util.sum(subsValues);
		}
	}

	@Test
	public void test() {
		Assert.assertEquals(6,  new OneTwoTreeComputation(0).apply().intValue());
		Assert.assertEquals(18, new OneTwoTreeComputation(1).apply().intValue());
	}
}