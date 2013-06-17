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
 * Lifted version of 'Misconception Example' model from 'Probabilistic Graphical
 * Models - Principles and Techniques', Chapter 4 pgs. 104 and 105.
 *
 * <pre>
 * union(
 * {{ (on X) [if gA(X) and (gB(X) or gC(X) or gD(X)) then 0 else 1] | true }},
 * {{ (on X) [if gB(X) and (gA(X) or gC(X) or gD(X)) then 0 else 1] | true }},
 * {{ (on X) [if gC(X) and (gA(X) or gB(X) or gD(X)) then 0 else 1] | true }},
 * {{ (on X) [if gD(X) and (gA(X) or gB(X) or gC(X)) then 0 else 1] | true }},
 * {{(on A,B) [if gA(A) and gB(B) then (if m(A) then (if m(B) then 10  else 1  ) else (if m(B) then 5   else 30 )) else 1] | A != B}},
 * {{(on B,C) [if gB(B) and gC(C) then (if m(B) then (if m(C) then 100 else 1  ) else (if m(C) then 1   else 100)) else 1] | B != C}},
 * {{(on C,D) [if gC(C) and gD(D) then (if m(C) then (if m(D) then 1   else 100) else (if m(D) then 100 else 1  )) else 1] | C != D}},
 * {{(on D,A) [if gD(D) and gA(A) then (if m(D) then (if m(A) then 100 else 1  ) else (if m(A) then 1   else 100)) else 1] | D != A}}
 * )
 * random variable names=m/1,gA/1,gB/1,gC/1,gD/1
 * 
 * Note: using short names to ease reading trace output when debugging -
 * m  = misconception
 * gA = group A
 * gB = group B
 * gC = group C
 * gD = group D
 *
 * Note: The constraints on the parfactors state group membership is 1 and only 1.
 * 
 * </pre>
 * 
 * @author oreilly
 * 
 */
@Beta
public class TrivialLoopyMisconceptionExample extends Model {

	/**
	 * <pre>
	 * union(
	 * {{ (on X) [if gA(X) and (gB(X) or gC(X) or gD(X)) then 0 else 1] | true }},
	 * {{ (on X) [if gB(X) and (gA(X) or gC(X) or gD(X)) then 0 else 1] | true }},
	 * {{ (on X) [if gC(X) and (gA(X) or gB(X) or gD(X)) then 0 else 1] | true }},
	 * {{ (on X) [if gD(X) and (gA(X) or gB(X) or gC(X)) then 0 else 1] | true }},
	 * {{(on A,B) [if gA(A) and gB(B) then (if m(A) then (if m(B) then 10  else 1  ) else (if m(B) then 5   else 30 )) else 1] | A != B}},
	 * {{(on B,C) [if gB(B) and gC(C) then (if m(B) then (if m(C) then 100 else 1  ) else (if m(C) then 1   else 100)) else 1] | B != C}},
	 * {{(on C,D) [if gC(C) and gD(D) then (if m(C) then (if m(D) then 1   else 100) else (if m(D) then 100 else 1  )) else 1] | C != D}},
	 * {{(on D,A) [if gD(D) and gA(A) then (if m(D) then (if m(A) then 100 else 1  ) else (if m(A) then 1   else 100)) else 1] | D != A}}
	 * )
	 * random variable names=m,gA,gB,gC,gD
	 * 
	 * Note: using short names to ease reading trace output when debugging -
	 * m  = misconception
	 * gA = group A
	 * gB = group B
	 * gC = group C
	 * gD = group D
	 *
	 * Note: The constraints on the parfactors state group membership is 1 and only 1.
	 * 
	 * </pre>
	 */
	public TrivialLoopyMisconceptionExample() {
		super("union("
				+ "{{ (on X) [if gA(X) and (gB(X) or gC(X) or gD(X)) then 0 else 1] | true }}, "
				+ "{{ (on X) [if gB(X) and (gA(X) or gC(X) or gD(X)) then 0 else 1] | true }}, "
				+ "{{ (on X) [if gC(X) and (gA(X) or gB(X) or gD(X)) then 0 else 1] | true }}, "
				+ "{{ (on X) [if gD(X) and (gA(X) or gB(X) or gC(X)) then 0 else 1] | true }}, "
				+ "{{(on A,B) [if gA(A) and gB(B) then (if m(A) then (if m(B) then 10  else 1  ) else (if m(B) then 5   else 30 )) else 1] | A != B}}, "
				+ "{{(on B,C) [if gB(B) and gC(C) then (if m(B) then (if m(C) then 100 else 1  ) else (if m(C) then 1   else 100)) else 1] | B != C}}, "
				+ "{{(on C,D) [if gC(C) and gD(D) then (if m(C) then (if m(D) then 1   else 100) else (if m(D) then 100 else 1  )) else 1] | C != D}}, "
				+ "{{(on D,A) [if gD(D) and gA(A) then (if m(D) then (if m(A) then 100 else 1  ) else (if m(A) then 1   else 100)) else 1] | D != A}}"
				+ ")", 
				"m/1", "gA/1", "gB/1", "gC/1", "gD/1");
	}
}
