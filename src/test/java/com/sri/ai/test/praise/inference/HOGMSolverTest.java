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
package com.sri.ai.test.praise.inference;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.List;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.HOGMProblemResult;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.HOGMSolver;

public class HOGMSolverTest {
	
	@Test
	public void linearRealArithmeticOnPosition() {
		String model = 
				"random position         : Real; // real, unobserved position of an object\n" + 
				"random observedPosition : Real; // observed, noisy position of the same object\n" + 
				"random event : Boolean;\n" + 
				"\n" + 
				"// p(position) proportional to inverted parabola around 0 + 10\n" + 
				"if position > -10 and position < 10\n" + 
				"   then -position^2 + 100\n" + 
				"   else 0;\n" + 
				"\n" + 
				"// p(observedPosition | position) proportional to parabola around position + 1\n" + 
				"if observedPosition - position > -1 and observedPosition - position < 1\n" + 
				"   then -(observedPosition - position)^2 + 1\n" + 
				"   else 0;\n" + 
				"\n" + 
				"// observed position is between 4 and 5; note that zero-mass events such as observedPosition = 4 will not work currently\n" + 
				"observedPosition > 4 and observedPosition < 5;\n" + 
				"\n" + 
				"// event of position being between 3 and 6 has probability 1; anything shorter has probability less than 1\n" + 
				"event <=> position > 3 and position < 6;\n" + 
				"";
		
		String query = "event";
		HOGMSolver solver = new HOGMSolver(model, query);
		List<HOGMProblemResult> results = solver.getResults();
	
		assertEquals(1, results.size());
		
		HOGMProblemResult result = getFirst(results);
		result.getErrors().stream().forEach(e -> println(e));
		Expression resultValue = result.getResult();
		println(resultValue);
		assertFalse(result.hasErrors());
		assertEquals(parse("if event then 1 else 0"), result.getResult());
	}
}
