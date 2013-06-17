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
 * <pre>
 * union( 
 * {{ (on X) [if 'there exists Y : p(X0, Y)'(X) and 'there exists Y : q(X0, Y)'(X) then 1 else 0]}}
 * {{ (on Y) [if 'there exists Y : p(X0, Y)'(Y) then 0.2 else 0.8] }},
 * {{ (on Z) [if 'there exists Y : q(X0, Y)'(Z) then 0.3 else 0.7] }}
 * )
 *       
 * random variable names='there exists Y : p(X0, Y)'/1, 'there exists Y : q(X0, Y)'/1.
 * </pre>
 */
@Beta
public class TrivialThereExistsPQWithPriors extends Model {

	/**
	 * <pre>
	 * union( 
	 * {{ (on X) [if 'there exists Y : p(X0, Y)'(X) and 'there exists Y : q(X0, Y)'(X) then 1 else 0]}}
	 * {{ (on Y) [if 'there exists Y : p(X0, Y)'(Y) then 0.2 else 0.8] }},
	 * {{ (on Z) [if 'there exists Y : q(X0, Y)'(Z) then 0.3 else 0.7] }}
	 * )
	 *       
	 * random variable names='there exists Y : p(X0, Y)'/1, 'there exists Y : q(X0, Y)'/1.
	 * </pre>
	 */
	public TrivialThereExistsPQWithPriors() {
		super(
				"model("
				+ "'Trivial There Exists PQ with priors.',"
				+ "'A basic model with priors that is the same as TrivialPQWithPriors but uses random variables names that are quoted.',"
				+ "sort(UniverseOfDiscourse, Unknown, {}), "
				+ "randomVariable('there exists Y : p(X0, Y)', 1, UniverseOfDiscourse, Boolean), "
				+ "randomVariable('there exists Y : q(X0, Y)', 1, UniverseOfDiscourse, Boolean), "
			    + "parfactors(union(" 
				+ "{{ (on X) [if 'there exists Y : p(X0, Y)'(X) and 'there exists Y : q(X0, Y)'(X) then 1 else 0]}}" + ","
				+ "{{ (on Y) [if 'there exists Y : p(X0, Y)'(Y) then 0.2 else 0.8] }}" + ","
				+ "{{ (on Z) [if 'there exists Y : q(X0, Y)'(Z) then 0.3 else 0.7] }}" + "))"
				+ ")");
	}
}
