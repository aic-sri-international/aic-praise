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
package com.sri.ai.test.praise.model.imports.church;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.model.imports.church.SimplifyWithRelationsAtBottom;
import com.sri.ai.util.Util;

@Beta
public class SimplifyWithRelationsAtBottomTest {
	
	@Test
	public void test() {
		Expression expression;
		Expression expected;
		Expression result;
		RewritingProcess process;
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		process = LBPFactory.newLBPProcessWithHighLevelModel(
				"random condition: Everything -> Boolean;" +
				"random target: Everything -> Boolean;");
		process = GrinderUtil.extendContextualSymbols(
				Util.map(
						parse("X"), parse("Everything"),
						parse("Y"), parse("Everything")
						),
						process);

		expression = parse("if X = Y then 2 else 3");
		expected   = parse("if X = Y then 2 else 3");
		result = SimplifyWithRelationsAtBottom.simplify(expression, parse("target"), process);
		assertEquals(expected, result);
		
		expression = parse("if condition(X) then 2 else 3");
		expected   = parse("if condition(X) then 2 else 3");
		result = SimplifyWithRelationsAtBottom.simplify(expression, parse("target"), process);
		assertEquals(expected, result);
		
		expression = parse("if target(X) then 2 else 3");
		expected   = parse("if target(X) then 2 else 3");
		result = SimplifyWithRelationsAtBottom.simplify(expression, parse("target"), process);
		assertEquals(expected, result);
		
		expression = parse("if target(X) and X = Y then 2 else 3");
		expected   = parse("if X = Y then if target(Y) then 2 else 3 else 3");
		result = SimplifyWithRelationsAtBottom.simplify(expression, parse("target"), process);
		assertEquals(expected, result);
		
		expression = parse("if target(X) and condition(X) and X = Y then 2 else 3");
		expected   = parse("if X = Y then if condition(Y) then if target(Y) then 2 else 3 else 3 else 3");
		result = SimplifyWithRelationsAtBottom.simplify(expression, parse("target"), process);
		assertEquals(expected, result);
		
		expression = parse("(if target(X) then if X = Y then 0.3 else 0.7 else 0.5) +"
						 + "(if not target(X) and condition(X) then 0.2 else 0.8)");
		expected   = parse("if X = Y then if condition(Y) then if target(Y) then 1.1 else 0.7 else if target(Y) then 1.1 else 1.3 else if condition(X) then if target(X) then 1.5 else 0.7 else if target(X) then 1.5 else 1.3");
		result = SimplifyWithRelationsAtBottom.simplify(expression, parse("target"), process);
		assertEquals(expected, result);

		// tests equality between atoms
		expression = parse("if target(X) = condition(X) then 1 else 0");
		expected   = parse("if condition(X) then if target(X) then 1 else 0 else if target(X) then 0 else 1");
		result = SimplifyWithRelationsAtBottom.simplify(expression, parse("target"), process);
		assertEquals(expected, result);

		// tests equality between propositions
		process = LBPFactory.newLBPProcessWithHighLevelModel(
				"random earthquake: Boolean;" +
				"random burglary: Boolean;");
		
		expression = parse("if earthquake = burglary then 1 else 0");
		expected   = parse("if earthquake then if burglary then 1 else 0 else if burglary then 0 else 1");
		result = SimplifyWithRelationsAtBottom.simplify(expression, parse("burglary"), process);
		assertEquals(expected, result);
	}
}
