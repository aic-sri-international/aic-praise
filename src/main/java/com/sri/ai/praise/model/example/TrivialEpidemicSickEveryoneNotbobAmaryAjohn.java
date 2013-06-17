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
 * partition(
 *       { [if epidemic then 0.1 else 0.9] },
 *       {{(on X in People) [if epidemic then if sick(X) then 0.7 else 0.3 else if sick(X) then 0.2 else 0.8]}}
 *       // Everybody who is not bob or mary or john is sick.
 *       {{(on X in People) [if sick(X) then 1 else 0] | X != bob and X != mary and X != john}},
 *       // bob is not sick
 *       { [if sick(bob) then 0 else 1] })
 *       
 * random variable names=epidemic/0,sick/1.
 * </pre>
 */
@Beta
public class TrivialEpidemicSickEveryoneNotbobAmaryAjohn extends Model {

	/**
	 * <pre>
	 * partition(
	 *       { [if epidemic then 0.1 else 0.9] },
	 *       {{(on X in People) [if epidemic then if sick(X) then 0.7 else 0.3 else if sick(X) then 0.2 else 0.8]}}
	 *       // Everybody who is not bob or mary or john is sick.
	 *       {{(on X in People) [if sick(X) then 1 else 0] | X != bob and X != mary and X != john}},
	 *       // bob is not sick
	 *       { [if sick(bob) then 0 else 1] })
	 *       
	 * random variable names=epidemic/0,sick/1.
	 * </pre>
	 */
	public TrivialEpidemicSickEveryoneNotbobAmaryAjohn() {
		super(
				"partition("
						+ "{ [if epidemic then 0.1 else 0.9] }, "
						+ "{{(on X in People) [if epidemic then if sick(X) then 0.7 else 0.3 else if sick(X) then 0.2 else 0.8]}}, "
						// Everybody who is not bob or mary or john is sick.
						+ "{{(on X in People) [if sick(X) then 1 else 0] | X != bob and X != mary and X != john}}, "
						// bob is not sick
						+ "{ [if sick(bob) then 0 else 1] })", "epidemic/0",
				"sick/1");
	}
}
