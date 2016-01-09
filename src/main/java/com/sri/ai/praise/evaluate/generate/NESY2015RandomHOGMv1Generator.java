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
package com.sri.ai.praise.evaluate.generate;

/**
 * Codifies the generation of the HOGMv1 models used as the basis for doing experimental runtime comparisons
 * between SGDPLL and the cutting edge propositional solver, VEC, as reported in the NESY 2015 workshop 
 * paper submission.
 * 
 * @author oreilly
 *
 */
public class NESY2015RandomHOGMv1Generator {
	static long _seed = 3;
	//
	static int [] _domainSizes = new int[] {2, 4, 8, 16};
	//
	static int [][] _params = new int[][] {	
		// #potentials, #variables, #constants, _depth, _breadth
		//                          (uniquely 
		//                           named)  
		{         5,          8,          0,      2,        2},
		{         5,          8,          0,      2,        4},
		{         5,          8,          0,      2,        8},
		{         5,          8,          0,      4,        4},
		{         5,          8,          0,      4,        8},
		{         5,          8,          2,      2,        2},
		{         5,          8,          4,      4,        4},
		{         5,          8,          8,      4,        8},
		//
		{         5,         10,          0,      2,        2},
		{         5,         10,          0,      2,        4},
		{         5,         10,          0,      2,        8},
		{         5,         10,          0,      4,        4},
		{         5,         10,          0,      4,        8},
		{         5,         10,          2,      2,        2},
		{         5,         10,          4,      4,        4},
		{         5,         10,          8,      4,        8},
		//
		{         5,         12,          0,      2,        2},
		{         5,         12,          0,      2,        4},
		{         5,         12,          0,      2,        8},
		{         5,         12,          0,      4,        4},
		{         5,         12,          0,      4,        8},
		{         5,         12,          2,      2,        2},
		{         5,         12,          4,      4,        4},
		{         5,         12,          8,      4,        8},
	};
	
	public static void main(String[] args) {
		NESY2015RandomHOGMv1Generator generator = new NESY2015RandomHOGMv1Generator();
		generator.run(args);
	}
	
	//
	// PROTECTED
	//
	protected void run(String[] args) {
		if (args.length != 1) {
			throw new IllegalArgumentException("the Root model output directory must be specified");
		}
		RandomHOGMv1Generator.generate(args[0], getTheoryTypeForGeneratedProblems(), _seed, _domainSizes, _params);
	}
	
	protected RandomHOGMv1Generator.TheoryType getTheoryTypeForGeneratedProblems() {
		return RandomHOGMv1Generator.TheoryType.HistoricalEqualityFormula;
	}
}