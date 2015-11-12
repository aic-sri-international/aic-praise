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
 * between SGDPLL and the cutting edge propositional solver, VEC, as reported in the invited paper submitted
 * to the German AI magazine "KI Zeitschrift" on "Challenges for Reasoning under Uncertainty, Inconsistency, 
 * Vagueness, and Preferences", November 2015.
 * 
 * NOTE: Uses similar generation properties to the random problems generated for the NESY 2015 paper (i.e. it 
 * extends that class). However, it uses a different formula generation routine (inequalities) and so do not
 * expect the two sets of generated problems to be comparable in any way.
 * 
 * @author oreilly
 *
 */
public class KIZeitschrift2015RandomHOGMv1Generator extends NESY2015RandomHOGMv1Generator {
	
	public static void main(String[] args) {
		KIZeitschrift2015RandomHOGMv1Generator generator = new KIZeitschrift2015RandomHOGMv1Generator();
		generator.run(args);
	}
	
	@Override
	protected RandomHOGMv1Generator.TheoryType getTheoryTypeForGeneratedProblems() {
		return RandomHOGMv1Generator.TheoryType.Inequality;
	}
}
