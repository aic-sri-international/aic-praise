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
 * Neither the name of the aic-expresso nor the names of its
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
package com.sri.ai.praise.demo.model;

import com.google.common.annotations.Beta;

@Beta
public class Example1 extends Example {
	public Example1() {
		super("Example 1");
		
		setModel(
		"/**\n"+
		" * Example 1: Epidemic and Sick with Symtoms.\n"+
		" * An example of the interplay between symtoms.\n" +
		" * Using Atomic and Conditional Rule Syntax.\n" +
		" */\n"+
		"//\n"+
		"// SORT DECLARATIONS:\n"+
		"sort People: 10, bob, dave, rodrigo, ciaran;\n"+
		"\n"+
		"//\n"+
		"// RANDOM VARIABLE DECLARATIONS:\n"+
		"random epidemic: -> Boolean;\n"+
		"random sick: People -> Boolean;\n"+
		"random fever: People -> Boolean;\n"+
		"random rash: People -> Boolean;\n"+
		"random notAtWork: People -> Boolean;\n" +
		"\n"+
		"//\n"+
		"// RULES\n" +
		"if epidemic then sick(X) 0.6 else sick(X) 0.05;\n" +
		"if sick(X) then fever(X) 0.7 else fever(X) 0.01;\n"+
		"if sick(X) then rash(X) 0.6 else rash(X) 0.07;\n"+
		"if sick(X) then notAtWork(X) 0.8 else notAtWork(X) 0.05;\n"+
		"\n"+
		"// By default, how likely is an epidemic?\n" +
		"epidemic 0.001;\n" +
		"\n"+
		"//\n"+
		"// By default, how likely are the following conditions?\n" +
		"sick(X) 0.009;\n"+
		"rash(X) 0.005;\n"+
		"fever(X) 0.001;\n"
		);
		
		setEvidence(
		"//\n"+
		"// EVIDENCE:\n"+
		"// Dave rarely misses work compared to everyone else\n"+
		"notAtWork(dave) 0.001;\n"+
		"notAtWork(X) and X != dave 0.008;\n"
		);
		
		setQueryToRun("belief([sick(X)])");
	}
}
