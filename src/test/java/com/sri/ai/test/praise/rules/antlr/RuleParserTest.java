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
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.praise.rules.antlr.RuleParserWrapper;
import com.sri.ai.test.praise.rules.AbstractParserTest;

public class RuleParserTest extends AbstractParserTest {

	public RuleParserTest () {
		parser = new RuleParserWrapper();
	}
	
	@Test
	public void testSortExpression () {
		String string;
		string = "sort People: 1000, bob, ann, mary;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sort", "People", "1000", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "bob", "ann", "mary"))));

		string = "sort Dogs: 1000;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sort", "Dogs", "1000", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

		string = "sort Dogs: 1000, rover;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sort", "Dogs", "1000", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "rover")));

		string = "sort Cats: Unknown;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sort", "Cats", "Unknown", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

		string = "sort Rats;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sort", "Rats", "Unknown", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

		System.out.println("test count = " + testCount);
	}
	
	@Test
	public void testRandomVariableExpression () {
		String string;
		string = "random grade: People x Class -> Number;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "grade", "2", "People", "Class", "Number"));

		string = "random father: People -> People;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "father", "1", "People", "People"));

		string = "random happy: People -> Boolean;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "happy", "1", "People", "Boolean"));

		string = "random president: -> People;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "president", "0", "People"));

		System.out.println("test count = " + testCount);
	}
	
	@Test 
	public void testComment () {
		String string;
		string = "sick(X); // This is a test.\n";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "// This is a test.\n sick(X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "sick(X); // This is a test.";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "sick // This is a test.\n (X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "// Test\n sick( // This is a test.\n X); // Test";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "sick(X); /* This is a test. */";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "/* This is a test. */ sick(X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "sick /* This is a test. */ (X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "sick( /* This is a test. */ X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "sick(X /* This is a test. */ );";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));


	}
	
	@Test
	public void testPotentialExpression () {
		String string;
		string = "sick(X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));

		string = "sick(X) 0.3;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.3"));

		string = "sick(X) 0.3 + 0.1;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "0.3", "0.1")));

		string = "sick(X) 0.3+0.1;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "0.3", "0.1")));

		string = "sick(X) 0.3 * 0.1;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "0.3", "0.1")));

		string = "sick(X) 0.3*0.1;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "0.3", "0.1")));

		string = "sick(X) 0.3 - 0.1;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "0.3", "0.1")));

		string = "sick(X) 0.3-0.1;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "0.3", "0.1")));

		string = "sick(X) 0.3 / 2;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "0.3", "2")));

		string = "sick(X) 0.3/2;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "0.3", "2")));

		string = "sick(X) 0.3 ^ 2;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "0.3", "2")));

		string = "sick(X) 0.3^2;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "0.3", "2")));

		string = "sick(X) and happy(X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("happy", "X")), "1"));

		string = "sick(X) and happy(X) 0.1;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("happy", "X")), "0.1"));

		string = "sick(john) = sick(bob);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "bob")), "1"));

		string = "sick(john) != sick(bob);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "bob")), "1"));

		System.out.println("test count = " + testCount);
	}

	@Test
	public void testConditionalExpression () {
		String string;
		
		string = "if circle(X) then round(X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("circle", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("round", "X"), "1")));

		string = "if epidemic then sick(X) 0.7;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.7")));

		string = "if epidemic then sick(X) and unhappy(X) 0.9;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("unhappy", "X")), "0.9")));
		
		string = "if chilly(P) and live(X, P) then sick(X) 0.6;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("chilly", "P"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("live", "X", "P")), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.6")));

		string = "if colleagues(X,Y) then likes(X,Y) 0.8;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("colleagues", "X", "Y"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("likes", "X", "Y"), "0.8")));

		string = "if epidemic then if sick(X) and friends(X,Y) then sick(Y) 0.8;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.8"))));

		string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.8"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), 1)));

		string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y) 0.3;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.8"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.3")));

		string = "if epidemic then 0.7 sick(X) :- not vaccinated(X). else 0.7 sick(X).;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "0.7", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("vaccinated", "X"))), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "0.7", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"))));
		
		string = "if X may be same as Y and Y = obama then entityOf(X, Y);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(". may be same as .", "X", "Y"),
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "obama")), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("entityOf", "X", "Y"), "1")));

		System.out.println("test count = " + testCount);
	}
	
	@Test
	public void testConditionalRuleWithConjunction() {
		String string;
		
		string = "sick(X) 0.4 and if epidemic then sick(john) 0.3 and sick(mary) 0.4;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.4"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic",
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )",
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), "0.3"),
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "mary"), "0.4")))))));
		
		string = "if epidemic then sick(X) 0.7 and (if panic then sick(X) 0.8 and flu(Y) 0.9) else sick(john) 0.2 and sick(mary) 0.3;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.7"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "panic", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
												Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
																Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.8"), 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
																Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("flu", "Y"), "0.9")))))), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), "0.2"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "mary"), "0.3")))));
	
		
		string = "if epidemic then (if panic then sick(X) 0.8 and flu(Y) 0.9) else if panic then sick(john) 0.2 and sick(mary) 0.3;";
		// 'conditional rule'(epidemic, 'conditional rule'(panic, ( 'atomic rule'(sick(X), 0.8), 'atomic rule'(flu(Y), 0.9) )), 'conditional rule'(panic, ( 'atomic rule'(sick(john), 0.2), 'atomic rule'(sick(mary), 0.3) )))
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "panic",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )",
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list",
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.8"),
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("flu", "Y"), "0.9")))),
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "panic",
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )",
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list",
												Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), "0.2"),
												Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "mary"), "0.3")))) 
				));

		System.out.println("test count = " + testCount);
	}
	
	@Test
	public void testMayBeSameAs() {
		String string;
		
		string = "there exists Y : mother(X,Y) and Y may be same as X;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule",  
				ThereExists.make(Expressions.createSymbol("Y"), 
						Expressions.apply("and",
								Expressions.apply("mother", "X", "Y"),
								Expressions.apply(". may be same as .", "Y", "X"))), 
				"1"));
	}
	
	//http://code.google.com/p/aic-praise/issues/detail?id=19
	@Test
	public void testIssue19() {
		testFail("if sick(X) and friends(X,Y) then 0.5 else sick(Y);");
	}

	@Test
	public void testPrologExpression () {
		String string;
		string = "sick(john).";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "1", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john")));

		string = "sick(X).";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "1", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X")));

		string = "not sick(mary).";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "1", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "mary"))));

		string = "0.3 sick(X).";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "0.3", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X")));

		string = "round(X) :- circle(X).";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "1", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("round", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("circle", "X")));

		string = "0.7 sick(X) :- epidemic and not vaccinated(X).";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "0.7", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "epidemic", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("vaccinated", "X")))));

		System.out.println("test count = " + testCount);
	}

	@Test
	public void testStandardProbabilityExpression () {
		String string;
		string = "P(sick(X) | epidemic) = 0.8;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("standard probability rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "epidemic", "0.8"));

		string = "P(sick(X) and happy(Y) | mother(Z)) = 0.4;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("standard probability rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("happy", "Y")), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("mother", "Z"), "0.4"));

		string = "P(sick(X) | epidemic);";
		testFail(string);

	}

	@Test
	public void testCausalExpression () {
		String string;
		string = "sick(X) -> fever(X) 0.6;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("causal rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("fever", "X"), "0.6")));
		
		string = "sick(X) and happy(Y) -> fever(X) 0.6;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("causal rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("happy", "Y")), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("fever", "X"), "0.6")));

	}

	@Test
	public void testConjunctions () {
		String string;
		string = "sick(X) 0.8 and sick(john) 0.3;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.8"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), "0.3"))));

		// Testing associativity.
		string = "sick(X) 0.8 and sick(john) 0.3 and sick(mary) 0.5 and sick(peter);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.8"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), "0.3"),
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "mary"), "0.5"),
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "peter"), 1))));

		string = "sick(X) 0.8 and happy(X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.8"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("happy", "X"), "1"))));

		// This is not a conjunction.
		string = "sick(X) and happy(X);";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("happy", "X")), "1"));

		// Test parsing of conjunctions in conditional rules.
		string = "if epidemic then sick(john) 0.3 and sick(mary) 0.4;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), "0.3"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "mary"), "0.4")))));

		string = "if epidemic then sick(X) 0.7 else sick(X) 0.4;";  // Not a conjunction.
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.7"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.4")));

		string = "if epidemic then sick(X) 0.7 else sick(john) 0.2 and sick(mary) 0.3;";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.7"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), "0.2"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "mary"), "0.3")))));
	}

	@Test
	public void testMulti () {
		String string;
		ArrayList<Expression> expected = new ArrayList<Expression>();

		string = "sick(X);sick(Y);";
		expected.add(
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));
		expected.add(Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "1"));
		testAll(string, expected);
		expected.clear();

		string = "if circle(X) then round(X); sick(X);";
		expected.add(Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("circle", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("round", "X"), "1")));
		expected.add(Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));
		testAll(string, expected);
		expected.clear();

		string = "if circle(X) then round(X); sick(X); sort Dogs: 1000, rover; random grade: People x Class -> Number;";
		expected.add(Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("circle", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("round", "X"), "1")));
		expected.add(Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"));
		expected.add(Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sort", "Dogs", "1000", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "rover")));
		expected.add(Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "grade", "2", "People", "Class", "Number"));
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
					for(Expression expression : result)
						System.out.println(Brewer.generateBuildString(expression.getSyntaxTree()));
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
