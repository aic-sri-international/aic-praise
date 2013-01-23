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
package com.sri.ai.test.praise.rules.antlr;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.brewer.core.Brewer;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.praise.rules.antlr.RuleParserWrapper;
import com.sri.ai.test.praise.rules.AbstractParserTest;

public class RuleParserTest extends AbstractParserTest {

	public RuleParserTest () {
		parser = new RuleParserWrapper();
	}
	
	@Test
	public void testPotentialExpression () {
		String string;
		string = "sick(X);";
		test(string, new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("sick", "X"), "1"));

		string = "sick(X) 0.3;";
		test(string, new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("sick", "X"), "0.300000000"));

		string = "sick(X) and happy(X);";
		test(string, new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("happy", "X")), "1"));

		string = "sick(X) and happy(X) 0.1;";
		test(string, new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("happy", "X")), "0.100000000"));


		System.out.println("test count = " + testCount);
	}

	@Test
	public void testConditionalExpression () {
		String string;
		string = "if circle(X) then round(X);";
		test(string, new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("circle", "X"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("round", "X"), "1")));

		string = "if epidemic then sick(X) 0.7;";
		test(string, new DefaultCompoundSyntaxTree("conditional rule", "epidemic", 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "X"), "0.700000000")));

		string = "if epidemic then sick(X) and unhappy(X) 0.9;";
		test(string, new DefaultCompoundSyntaxTree("conditional rule", "epidemic", 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("and", 
								new DefaultCompoundSyntaxTree("sick", "X"), 
								new DefaultCompoundSyntaxTree("unhappy", "X")), "0.900000000")));

		string = "if chilly(P) and live(X, P) then sick(X) 0.6;";
		test(string, new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("chilly", "P"), 
						new DefaultCompoundSyntaxTree("live", "X", "P")), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "X"), "0.600000000")));

		string = "if colleagues(X,Y) then likes(X,Y) 0.8;";
		test(string, new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("colleagues", "X", "Y"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("likes", "X", "Y"), "0.800000000")));

		string = "if epidemic then if sick(X) and friends(X,Y) then sick(Y) 0.8;";
		test(string, new DefaultCompoundSyntaxTree("conditional rule", "epidemic", 
				new DefaultCompoundSyntaxTree("conditional rule", 
						new DefaultCompoundSyntaxTree("and", 
								new DefaultCompoundSyntaxTree("sick", "X"), 
								new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
						new DefaultCompoundSyntaxTree("atomic rule", 
								new DefaultCompoundSyntaxTree("sick", "Y"), "0.800000000"))));

		string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y);";
		test(string, new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "0.800000000"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "Y"), 1)));

		string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y) 0.3;";
		test(string, new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "0.800000000"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "0.300000000")));

		string = "if epidemic then 0.7 sick(X) :- not vaccinated(X). else 0.7 sick(X).;";
		test(string, new DefaultCompoundSyntaxTree("conditional rule", "epidemic", 
				new DefaultCompoundSyntaxTree("prolog rule", "0.700000000", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("not", 
								new DefaultCompoundSyntaxTree("vaccinated", "X"))), 
				new DefaultCompoundSyntaxTree("prolog rule", "0.700000000", 
						new DefaultCompoundSyntaxTree("sick", "X"))));

		System.out.println("test count = " + testCount);
	}

	@Test
	public void testPrologExpression () {
		String string;
		string = "sick(john).";
		test(string, new DefaultCompoundSyntaxTree("prolog rule", "1", 
				new DefaultCompoundSyntaxTree("sick", "john")));

		string = "sick(X).";
		test(string, new DefaultCompoundSyntaxTree("prolog rule", "1", 
				new DefaultCompoundSyntaxTree("sick", "X")));

		string = "not sick(mary).";
		test(string, new DefaultCompoundSyntaxTree("prolog rule", "1", 
				new DefaultCompoundSyntaxTree("not", 
				new DefaultCompoundSyntaxTree("sick", "mary"))));

		string = "0.3 sick(X).";
		test(string, new DefaultCompoundSyntaxTree("prolog rule", "0.300000000", 
				new DefaultCompoundSyntaxTree("sick", "X")));

		string = "round(X) :- circle(X).";
		test(string, new DefaultCompoundSyntaxTree("prolog rule", "1", 
				new DefaultCompoundSyntaxTree("round", "X"), 
				new DefaultCompoundSyntaxTree("circle", "X")));

		string = "0.7 sick(X) :- epidemic and not vaccinated(X).";
		test(string, new DefaultCompoundSyntaxTree("prolog rule", "0.700000000", 
				new DefaultCompoundSyntaxTree("sick", "X"), 
				new DefaultCompoundSyntaxTree("and", "epidemic", 
						new DefaultCompoundSyntaxTree("not", 
								new DefaultCompoundSyntaxTree("vaccinated", "X")))));

		System.out.println("test count = " + testCount);
	}

	@Test
	public void testRandomVariableExpression () {
		String string;
		string = "random +: Number x Number -> Number;";
		test(string, new DefaultCompoundSyntaxTree("random variable", "+", "2", "Number", "Number", "Number"));

		string = "random father: People -> People;";
		test(string, new DefaultCompoundSyntaxTree("random variable", "father", "1", "People", "People"));

		string = "random happy: People -> Boolean;";
		test(string, new DefaultCompoundSyntaxTree("random variable", "happy", "1", "People", "Boolean"));

		string = "random president: -> People;";
		test(string, new DefaultCompoundSyntaxTree("random variable", "president", "0", "People"));

		System.out.println("test count = " + testCount);
	}

	@Test
	public void testSortExpression () {
		String string;
		string = "sort People: 1000, bob, ann, mary;";
		test(string, new DefaultCompoundSyntaxTree("sort", "People", "1000", 
				new DefaultCompoundSyntaxTree("kleene list", "bob", "ann", "mary")));

		string = "sort Dogs: 1000;";
		test(string, new DefaultCompoundSyntaxTree("sort", "Dogs", "1000", 
				new DefaultCompoundSyntaxTree("kleene list")));

		string = "sort Dogs: 1000, rover;";
		test(string, new DefaultCompoundSyntaxTree("sort", "Dogs", "1000", "rover"));

		string = "sort Cats: Unknown;";
		test(string, new DefaultCompoundSyntaxTree("sort", "Cats", "Unknown", 
				new DefaultCompoundSyntaxTree("kleene list")));

		string = "sort Rats;";
		test(string, new DefaultCompoundSyntaxTree("sort", "Rats", "Unknown", 
				new DefaultCompoundSyntaxTree("kleene list")));

		System.out.println("test count = " + testCount);
	}

	@Test
	public void testMulti () {
		String string;
		ArrayList<Expression> expected = new ArrayList<Expression>();

		string = "sick(X);sick(Y);";
		expected.add(new DefaultCompoundSyntaxTree("atomic rule", 
					new DefaultCompoundSyntaxTree("sick", "X"), "1"));
		expected.add(new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("sick", "Y"), "1"));
		testAll(string, expected);
		expected.clear();

		string = "if circle(X) then round(X); sick(X);";
		expected.add(new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("circle", "X"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("round", "X"), "1")));
		expected.add(new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("sick", "X"), "1"));
		testAll(string, expected);
		expected.clear();

		string = "if circle(X) then round(X); sick(X); sort Dogs: 1000, rover; random +: Number x Number -> Number;";
		expected.add(new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("circle", "X"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("round", "X"), "1")));
		expected.add(new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("sick", "X"), "1"));
		expected.add(new DefaultCompoundSyntaxTree("sort", "Dogs", "1000", "rover"));
		expected.add(new DefaultCompoundSyntaxTree("random variable", "+", "2", "Number", "Number", "Number"));
		testAll(string, expected);
		expected.clear();

		System.out.println("test count = " + testCount);
	}




	protected void testAll (String input) {
		testAll(input, true);
	}

	protected void testAll (String input, List<Expression> expectedResult) {
		testAll(input, true, true, expectedResult);
	}

	protected void testAllFail (String input) {
		testAll(input, false);
	}

	protected void testAll (String input, boolean expectSucceed) {
		testAll(input, expectSucceed, false, null);
	}

	protected void testAll (String input, boolean expectSucceed, boolean checkResult, List<Expression> expectedResult) {
		testCount ++;
		List<Expression> result = ((RuleParserWrapper)parser).parseAll(input);
		if (expectSucceed) {
			if (checkResult) {
				assertEquals(expectedResult, result);
			}
			else {
				if(result != null) {
					System.out.println("generated string for \"" + input + "\": ");
					for(Expression expr : result)
						System.out.println(Brewer.generateBuildString(expr));
					System.out.println("\n\n");
				}
				Assert.assertNotNull(result);
			}
		}
		else {
			Assert.assertNull(result);
		}
	}

}
