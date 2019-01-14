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
package com.sri.ai.test.praise.core.representation.translation.ciaranframework.core.uai;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static org.junit.Assert.assertEquals;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.table.core.data.FunctionTable;
import com.sri.ai.praise.core.representation.translation.ciaranframework.core.uai.TranslationOfTableToInequalities;

public class TranslationOfTableToInequalitiesTest {

	@Test
	public void test() {
		FunctionTable table;
		String expected;
		
		table = new FunctionTable(
				list(2),
				list(1.0, 1.0));
		expected = "1";
		run(table, expected);
		
		table = new FunctionTable(
				list(5),
				list(0.0, 1.0, 1.0, 0.0, 0.0));
		expected = "if (g0 = 0) or (g0 >= 3) then 0 else 1";
		run(table, expected);
		
		table = new FunctionTable(
				list(4, 3),
				list(
						0.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						0.0, 0.0, 0.0)
				);
		expected = 
				"if (g0 = 0) and (g1 >= 1) or (g0 >= 1) and (g0 <= 2) then 1 else 0";
		run(table, expected);

		
		table = new FunctionTable(
				list(1, 3),
				list(0.0, 1.0, 1.0)
				);
		expected = 
				"if (g1 >= 1) then 1 else 0";
		run(table, expected);

		
		table = new FunctionTable(
				list(4, 1),
				list(
						0.0,
						1.0,
						1.0,
						0.0)
				);
		expected = 
				"if (g0 >= 1) and (g0 <= 2) then 1 else 0";
		run(table, expected);

		
		table = new FunctionTable(
				list(4, 3),
				list(
						0.0, 0.0, 0.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						0.0, 0.0, 0.0)
				);
		expected = 
				"if (g0 >= 1) and (g0 <= 2) then 1 else 0";
		run(table, expected);

		table = new FunctionTable(
				list(3, 3),
				list(
						0.0, 0.0, 0.0,
						0.0, 1.0, 1.0,
						0.0, 0.0, 0.0)
				);
		expected = 
				"if (g0 = 0) or (g0 = 1) and (g1 = 0) or (g0 = 2) then 0 else 1";
		run(table, expected);


		table = new FunctionTable(
				list(3, 3),
				list(
						0.0, 0.0, 0.0,
						0.0, 0.0, 0.0,
						0.0, 0.0, 0.0)
				);
		expected = 
				"0";
		run(table, expected);

		
		table = new FunctionTable(
				list(3, 3),
				list(
						0.0, 0.0, 0.0,
						1.0, 1.0, 1.0,
						0.0, 0.0, 0.0)
				);
		expected = 
				"if g0 = 0 or g0 = 2 then 0 else 1";
		run(table, expected);

		
		table = new FunctionTable(
				list(10, 3),
				list(
						0.0, 0.0, 0.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						0.0, 0.0, 0.0)
				);
		expected = 
				"if g0 >= 1 and g0 <= 8 then 1 else 0";
		run(table, expected);

		
		table = new FunctionTable(
				list(10, 3),
				list(
						0.0, 0.0, 0.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						0.0, 1.0, 1.0, // <--- different
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						0.0, 0.0, 0.0)
				);
		expected = 
				"if (g0 >= 1) and (g0 <= 4) or (g0 = 5) and (g1 >= 1) or (g0 >= 6) and (g0 <= 8) then 1 else 0";
		run(table, expected);

		
		table = new FunctionTable(
				list(2, 5),
				list(
						0.0, 1.0, 1.0, 0.0, 0.0,
						0.0, 1.0, 1.0, 0.0, 0.0)
				);
		expected = 
				"if (g0 = 0) and (g1 = 0) or " + 
						"   (g0 = 0) and (g1 >= 3) or (g0 = 1) and (g1 = 0) or " + 
						"   (g0 = 1) and (g1 >= 3) then 0 else 1";
		run(table, expected);

		
		table = new FunctionTable(
				list(2, 5),
				list(
						0.0, 1.0, 1.0, 0.0, 0.0,
						0.0, 1.0, 2.0, 0.0, 0.0)
				);
		expected = 
				"if (g0 = 0) and (g1 = 0) or "
				+ "(g0 = 0) and (g1 >= 3) or (g0 = 1) and (g1 = 0) or (g0 = 1) and (g1 >= 3) then 0 else if (g0 = 0) and (g1 >= 1) and (g1 <= 2) or (g0 = 1) and (g1 = 1) then 1 else 2";
		run(table, expected);
		
		table = new FunctionTable(
				list(2, 5),
				list(
						1.0, 1.0, 1.0, 1.0, 1.0,
						2.0, 2.0, 2.0, 2.0, 2.0)
				);
		expected = "if g0 = 1 then 2 else 1";
		run(table, expected);
		
		table = new FunctionTable(
				list(5, 4, 3),
				list(
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						
						1.0, 1.0, 1.0,
						0.0, 0.0, 0.0,
						0.0, 0.0, 0.0,
						1.0, 1.0, 1.0,
						
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						0.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0,
						1.0, 1.0, 1.0
						));
		expected = "if "
				+ "(g0 = 0) or "
				+ "(g0 = 1) and (g1 = 0) or "
				+ "(g0 = 1) and (g1 = 3) or "
				+ "(g0 = 2) and (g1 <= 1) or "
				+ "(g0 = 2) and ((g1 = 2) and (g2 >= 1) or (g1 = 3)) or (g0 >= 3) "
				+ "then 1 else 0";
		run(table, expected);
	}

	/**
	 * @param table
	 * @param expected
	 */
	private void run(FunctionTable table, String expected) {
		Expression actual;
		actual = TranslationOfTableToInequalities.constructGenericTableExpressionUsingInequalities(table);
		Expression expectedExpression = parse(expected);
		System.out.println(actual);	
		assertEquals(expectedExpression, actual);
	}
}
