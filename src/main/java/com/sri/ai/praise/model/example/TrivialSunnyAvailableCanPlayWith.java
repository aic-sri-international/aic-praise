/*
 * Copyright (c) 2013, SRI International
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
package com.sri.ai.praise.model.example;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.model.Model;

/**
 * The point of this model is that the query variable ('canPlayWith') will
 * request a message from 'sunny', which will be false. The condition then
 * fails and the 0-1 message on the query is determined, without a need for
 * examining 'available'.
 * 
 * <pre>
 * partition(
 *       {{ [if sunny then 0 else 1] }},
 *       {{(on X in People) [ if sunny and available(X) then (if canPlayWith(X) then 0.8 else 0.2) else (if canPlayWith(X) then 0 else 1) ] }},
 *       {{(on X in People) [ if available(X) then 0.3 else 0.7 ] }})
 *       
 * random variable names=sunny/0,available/1,canPlayWith/1.
 * </pre>
 */
@Beta
public class TrivialSunnyAvailableCanPlayWith extends Model {

	/**
	 * The point of this model is that the query variable ('canPlayWith') will
	 * request a message from 'sunny', which will be false. The condition then
	 * fails and the 0-1 message on the query is determined, without a need for
	 * examining 'available'.
	 * 
	 * <pre>
	 * partition(
	 *       {{ [if sunny then 0 else 1] }},
	 *       {{(on X in People) [ if sunny and available(X) then (if canPlayWith(X) then 0.8 else 0.2) else (if canPlayWith(X) then 0 else 1) ] }},
	 *       {{(on X in People) [ if available(X) then 0.3 else 0.7 ] }})
	 *       
	 * random variable names=sunny/0,available/1,canPlayWith/1.
	 * </pre>
	 */
	public TrivialSunnyAvailableCanPlayWith() {
		super(
				"partition("
						+ "{{ [if sunny then 0 else 1] }}, "
						+ "{{(on X in People) [ if sunny and available(X) then (if canPlayWith(X) then 0.8 else 0.2) else (if canPlayWith(X) then 0 else 1) ] }}, "
						+ "{{(on X in People) [ if available(X) then 0.3 else 0.7 ] }})",
				"sunny/0", "available/1", "canPlayWith/1");
	}
}
