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
package com.sri.ai.test.praise.rules;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.grinder.ui.TreeUtil;
import com.sri.ai.praise.LPIGrammar;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.Model.ModelException;
import com.sri.ai.praise.rules.ReservedWordException;
import com.sri.ai.praise.rules.RuleConverter;
import com.sri.ai.praise.rules.antlr.RuleParserWrapper;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

public class RuleConverterTest {
	private RuleParserWrapper         ruleParser;
	private RuleConverter             ruleConverter;
	private AntlrGrinderParserWrapper lowParser;

	@Before
	public void setUp () {
		Grammar grammar = new LPIGrammar();
		// Ensure the grammar class passed in is used where necessary.
		BrewerConfiguration.setProperty(BrewerConfiguration.KEY_DEFAULT_GRAMMAR_CLASS, grammar.getClass().getName());
		
		ruleParser = new RuleParserWrapper();
		ruleConverter = new RuleConverter();
		
		lowParser = new AntlrGrinderParserWrapper();
	}
	
	// Tests for:
	// http://code.google.com/p/aic-praise/wiki/SyntaxAndMeaningOfProbabilisticRules
	// http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	//


	@Test
	public void testTranslateAtomicRules () {

		// string = "sick(X);";
		testTranslateRules(ruleParser.parse("sick(X);"), 
				lowParser.parse("if sick(X) then 1 else 0"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("sick", "X"), "1"), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("sick", "X"), "1", "0"));

		// string = "sick(X) 0.3;";
		testTranslateRules(ruleParser.parse("sick(X) 0.3;"), 
				lowParser.parse("if sick(X) then 0.3 else 0.7"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("sick", "X"), "0.3"),
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("sick", "X"), "0.3", "0.7"));

		// string = "sick(X) and happy(X);";
		testTranslateRules(ruleParser.parse("sick(X) and happy(X);"), 
				lowParser.parse("if sick(X) and happy(X) then 1 else 0"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("happy", "X")), "1"), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("happy", "X")), "1", "0"));

		// string = "sick(X) and happy(X) 0.1;";
		testTranslateRules(ruleParser.parse("sick(X) and happy(X) 0.1;"), 
				lowParser.parse("if sick(X) and happy(X) then 0.1 else 0.9"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("atomic rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("happy", "X")), "0.1"),
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("happy", "X")), "0.1", "0.9"));
	}
	
	@Test
	public void testTranslateConditionalRules() {
		// Conditional rule tests
		// string = "if circle(X) then round(X);";
		testTranslateRules(ruleParser.parse("if circle(X) then round(X);"), 
				lowParser.parse("if circle(X) then if round(X) then 1 else 0 else 0.5"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("circle", "X"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("round", "X"), "1")), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("circle", "X"), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("round", "X"), "1", "0"), "0.5"));

		// string = "if epidemic then sick(X) 0.7;";
		testTranslateRules(ruleParser.parse("if epidemic then sick(X) 0.7;"), 
				lowParser.parse("if epidemic then if sick(X) then 0.7 else 0.3 else 0.5"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("conditional rule", "epidemic", 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "X"), "0.7")), 
			new DefaultCompoundSyntaxTree("if . then . else .", "epidemic", 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("sick", "X"), "0.7", "0.3"), "0.5"));

		// string = "if epidemic then sick(X) and unhappy(X) 0.9;";
		testTranslateRules(ruleParser.parse("if epidemic then sick(X) and unhappy(X) 0.9;"), 
				lowParser.parse("if epidemic then if sick(X) and unhappy(X) then 0.9 else 0.1 else 0.5"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("conditional rule", "epidemic", 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("and", 
								new DefaultCompoundSyntaxTree("sick", "X"), 
								new DefaultCompoundSyntaxTree("unhappy", "X")), "0.9")), 
			new DefaultCompoundSyntaxTree("if . then . else .", "epidemic", 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("and", 
								new DefaultCompoundSyntaxTree("sick", "X"), 
								new DefaultCompoundSyntaxTree("unhappy", "X")), "0.9", "0.1"), "0.5"));

		// string = "if chilly(P) and live(X, P) then sick(X) 0.6;";
		testTranslateRules(ruleParser.parse("if chilly(P) and live(X, P) then sick(X) 0.6;"), 
				lowParser.parse("if chilly(P) and live(X, P) then if sick(X) then 0.6 else 0.4 else 0.5"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("chilly", "P"), 
						new DefaultCompoundSyntaxTree("live", "X", "P")), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "X"), "0.6")), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("chilly", "P"), 
						new DefaultCompoundSyntaxTree("live", "X", "P")), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("sick", "X"), "0.6", "0.4"), "0.5"));

		// string = "if colleagues(X,Y) and Y != bob then likes(X,Y) 0.8;";	
		testTranslateRules(ruleParser.parse("if colleagues(X,Y) and Y != bob then likes(X,Y) 0.8;"), 
				lowParser.parse("if colleagues(X,Y) and Y != bob then if likes(X,Y) then 0.8 else 0.2 else 0.5"));
		
		// string = "if colleagues(X,Y) then likes(X,Y) 0.8;";
		testTranslateRules(new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("colleagues", "X", "Y"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("likes", "X", "Y"), "0.8")), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("colleagues", "X", "Y"), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("likes", "X", "Y"), "0.8", "0.2"), "0.5"));

		// string = "if epidemic then if sick(X) and friends(X,Y) then sick(Y) 0.8;";
		testTranslateRules(ruleParser.parse("if epidemic then if sick(X) and friends(X,Y) then sick(Y) 0.8;"), 
				lowParser.parse("if epidemic then if sick(X) and friends(X, Y) then if sick(Y) then 0.8 else 0.2 else 0.5 else 0.5"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("conditional rule", "epidemic", 
				new DefaultCompoundSyntaxTree("conditional rule", 
						new DefaultCompoundSyntaxTree("and", 
								new DefaultCompoundSyntaxTree("sick", "X"), 
								new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
						new DefaultCompoundSyntaxTree("atomic rule", 
								new DefaultCompoundSyntaxTree("sick", "Y"), "0.8"))), 
			new DefaultCompoundSyntaxTree("if . then . else .", "epidemic", 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("and", 
								new DefaultCompoundSyntaxTree("sick", "X"), 
								new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
						new DefaultCompoundSyntaxTree("if . then . else .", 
								new DefaultCompoundSyntaxTree("sick", "Y"), "0.8", "0.2"), "0.5"), "0.5"));

		// string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y);";
		testTranslateRules(ruleParser.parse("if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y);"), 
				lowParser.parse("if sick(X) and friends(X,Y) then if sick(Y) then 0.8 else 0.2 else if sick(Y) then 1 else 0"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "0.8"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "Y"), 1)), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "0.8", "0.2"), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "1", "0")));

		// string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y) 0.3;";
		testTranslateRules(ruleParser.parse("if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y) 0.3;"), 
				lowParser.parse("if sick(X) and friends(X,Y) then if sick(Y) then 0.8 else 0.2 else if sick(Y) then 0.3 else 0.7"));

		testTranslateRules(new DefaultCompoundSyntaxTree("conditional rule", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "0.8"), 
				new DefaultCompoundSyntaxTree("atomic rule", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "0.3")), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("and", 
						new DefaultCompoundSyntaxTree("sick", "X"), 
						new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "0.8", "0.2"), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("sick", "Y"), "0.3", "0.7")));
		
			// string = "if sick(X) and friends(X,Y) then 0.5 else sick(Y);";
// TODO - this looks incorrect!!!! i.e.: if 0.5 then 1 else 0	
// http://code.google.com/p/aic-praise/issues/detail?id=19
			testTranslateRules(ruleParser.parse("if sick(X) and friends(X,Y) then 0.5 else sick(Y);"), 
					lowParser.parse("if sick(X) and friends(X,Y) then if 0.5 then 1 else 0 else if sick(Y) then 1 else 0"));
			
			testTranslateRules(new DefaultCompoundSyntaxTree("conditional rule", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("sick", "X"), 
							new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
					"0.5", 
					new DefaultCompoundSyntaxTree("atomic rule", 
							new DefaultCompoundSyntaxTree("sick", "Y"), 1)), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("sick", "X"), 
							new DefaultCompoundSyntaxTree("friends", "X", "Y")), 
					"0.5", 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("sick", "Y"), "1", "0")));		
	}
	
	@Test
	public void testTranslatePrologRules () {
// TODO - can we not just get away with a trailing period instead of '.;' for prolog rules?
// http://code.google.com/p/aic-praise/issues/detail?id=18
		
		// Prolog rule tests		
		testTranslateRules(ruleParser.parse("sick(john).;"), 
				lowParser.parse("if sick(john) then 1 else 0"));

		testTranslateRules(new DefaultCompoundSyntaxTree("prolog rule", "1", 
				new DefaultCompoundSyntaxTree("sick", "john")),
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("sick", "john"), "1", "0"));

		// string = "sick(X).";
		testTranslateRules(ruleParser.parse("sick(X).;"), 
				lowParser.parse("if sick(X) then 1 else 0"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("prolog rule", "1", 
				new DefaultCompoundSyntaxTree("sick", "X")), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("sick", "X"), "1", "0"));

		// string = "not sick(mary).";
		testTranslateRules(ruleParser.parse("not sick(mary).;"), 
				lowParser.parse("if not sick(mary) then 1 else 0"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("prolog rule", "1", 
				new DefaultCompoundSyntaxTree("not", 
						new DefaultCompoundSyntaxTree("sick", "mary"))), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("not", 
						new DefaultCompoundSyntaxTree("sick", "mary")), "1", "0"));

		// string = "0.3 sick(X).";
		testTranslateRules(ruleParser.parse("0.3 sick(X).;"), 
				lowParser.parse("if sick(X) then 0.3 else 0.7"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("prolog rule", "0.3", 
				new DefaultCompoundSyntaxTree("sick", "X")), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("sick", "X"), "0.3", "0.7"));

		// string = "round(X) :- circle(X).";
		testTranslateRules(ruleParser.parse("round(X) :- circle(X).;"), 
				lowParser.parse("if circle(X) then if round(X) then 1 else 0 else 0.5"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("prolog rule", "1", 
				new DefaultCompoundSyntaxTree("round", "X"), 
				new DefaultCompoundSyntaxTree("circle", "X")), 
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("circle", "X"), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("round", "X"), "1", "0"), "0.5"));

		// string = "0.7 sick(X) :- epidemic and not vaccinated(X).";
		testTranslateRules(ruleParser.parse("0.7 sick(X) :- epidemic and not vaccinated(X).;"), 
				lowParser.parse("if epidemic and not vaccinated(X) then if sick(X) then 0.7 else 0.3 else 0.5"));
		
		testTranslateRules(new DefaultCompoundSyntaxTree("prolog rule", "0.7", 
				new DefaultCompoundSyntaxTree("sick", "X"), 
				new DefaultCompoundSyntaxTree("and", "epidemic", 
						new DefaultCompoundSyntaxTree("not", 
								new DefaultCompoundSyntaxTree("vaccinated", "X")))),
			new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("and", "epidemic", 
						new DefaultCompoundSyntaxTree("not", 
								new DefaultCompoundSyntaxTree("vaccinated", "X"))), 
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree("sick", "X"), "0.7", "0.3"), "0.5"));
	}
	
	@Test
	public void testTranslateConjunctionOfRules () {
		List<Expression> input, result, expected;

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("sick(X) 0.8 and sick(john) 0.3;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("sick(X) 0.8; sick(john) 0.3;"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("sick(X) 0.8 and happy(X);"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("sick(X) 0.8; happy(X);"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.addAll(ruleParser.parseAll("sick(X) 0.8 and happy(X); mother(Y);"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("sick(X) 0.8; happy(X); mother(Y);"));
		assertEquals(expected, result);

		// Testing conditional rule lists.
		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("if epidemic then sick(john) 0.3 and sick(mary) 0.4;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("if epidemic then sick(john) 0.3; if epidemic then sick(mary) 0.4;"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("sick(X) 0.4 and if epidemic then sick(john) 0.3 and sick(mary) 0.4;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("sick(X) 0.4; if epidemic then sick(john) 0.3; if epidemic then sick(mary) 0.4;"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("if epidemic then sick(john) 0.3 and sick(mary) 0.4 else sick(X) 0.8;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("if epidemic then sick(john) 0.3 else sick(X) 0.8;"));
		expected.add(lowParser.parse("'conditional rule'(epidemic, 'atomic rule'(sick(mary), 0.4), 0.5)"));
		assertEquals(expected, result);

		// Should not change anything.
		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("if epidemic then sick(X) 0.7 else sick(X) 0.4;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("if epidemic then sick(X) 0.7 else sick(X) 0.4;"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("if epidemic then sick(X) 0.7 else sick(john) 0.2 and sick(mary) 0.3;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("if epidemic then sick(X) 0.7 else sick(john) 0.2;"));
		expected.add(lowParser.parse("'conditional rule'(epidemic, 0.5, 'atomic rule'(sick(mary), 0.3))"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("if epidemic then sick(X) 0.7 and sick(X) 0.8 else sick(john) 0.2 and sick(mary) 0.3;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("if epidemic then sick(X) 0.7 else sick(john) 0.2;" +
				"if epidemic then sick(X) 0.8 else sick(mary) 0.3;"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("if epidemic then sick(X) 0.7 and if panic then sick(X) 0.8 and flu(Y) 0.9 else sick(X) 0.3 and sick(X) 0.4 else sick(john) 0.2 and sick(mary) 0.3;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("if epidemic then sick(X) 0.7 else sick(john) 0.2;" +
				"if epidemic then if panic then sick(X) 0.8 else sick(X) 0.3 else sick(mary) 0.3;" +
				"if epidemic then if panic then flu(Y) 0.9 else sick(X) 0.4 else sick(mary) 0.3;"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("if epidemic then sick(X) 0.7 and if panic then sick(X) 0.8 and flu(Y) 0.9 else sick(john) 0.2 and sick(mary) 0.3;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("if epidemic then sick(X) 0.7;" +
				"if epidemic then if panic then sick(X) 0.8 else sick(john) 0.2;" +
				"if epidemic then if panic then flu(Y) 0.9 else sick(mary) 0.3;"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("if epidemic then sick(X) 0.7 and (if panic then sick(X) 0.8 and flu(Y) 0.9) else sick(john) 0.2 and sick(mary) 0.3;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("if epidemic then sick(X) 0.7 else sick(john) 0.2;" +
				"if epidemic then (if panic then sick(X) 0.8) else sick(mary) 0.3;" +
				"if epidemic then (if panic then flu(Y) 0.9) else sick(mary) 0.3;"));
		assertEquals(expected, result);

		// Testing causal rule lists.
		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("sick(X) -> fever(X) 0.6 and flu;"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("sick(X) -> fever(X) 0.6; sick(X) -> flu;"));
		assertEquals(expected, result);

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("sick(X) -> fever(X) 0.6 and flu 0.5 and coughing(Y) -> pneumonia(Y) 0.7 and strep(Y);"));
		result = ruleConverter.translateConjunctions(input);
		expected = new ArrayList<Expression>();
		expected.addAll(ruleParser.parseAll("sick(X) -> fever(X) 0.6; sick(X) -> flu 0.5;" +
				"sick(X) -> coughing(Y) -> pneumonia(Y) 0.7; sick(X) -> coughing(Y) -> strep(Y);"));
		assertEquals(expected, result);

	}
	
	@Test(expected=UnsupportedOperationException.class)
	public void testTranslateConjuntionOfRulesFail () {
		List<Expression> input;

		input = new ArrayList<Expression>();
		input.add(ruleParser.parse("if epidemic then (if panic then sick(X) 0.8 and flu(Y) 0.9) else if panic then sick(john) 0.2 and sick(mary) 0.3;"));
		ruleConverter.translateConjunctions(input);
		
	}
	
	@Test
	public void testTranslateStandardProbabilityNotationStyleRules () {
		// Standard probability rule tests.
		String string = "P(sick(X) | epidemic) = 0.8;";
		testTranslateRules(ruleParser.parse(string), 
				lowParser.parse("if epidemic then if sick(X) then 0.8 else 0.2 else 0.5"));

		string = "P(sick(X) and happy(Y) | mother(Z)) = 0.4;";
		testTranslateRules(ruleParser.parse(string), 
				lowParser.parse("if mother(Z) then if sick(X) and happy(Y) then 0.4 else 0.6 else 0.5"));
	}
	
	@Test
	public void testTranslateCausalEffectStyleRules() {
		// Causal rule tests.
		String string = "sick(X) -> fever(X) 0.6;";
		testTranslateRules(ruleParser.parse(string), 
				lowParser.parse("if sick(X) then (if fever(X) then 0.6 else 0.4) else 0.5"));

		string = "sick(X) and happy(Y) -> fever(X) 0.6;";
		testTranslateRules(ruleParser.parse(string), 
				lowParser.parse("if sick(X) and happy(Y) then (if fever(X) then 0.6 else 0.4) else 0.5"));
	}
	
	@Test
	public void testTranslatePotentialExpressionsWithFunctionsIntoPotentialExpressionsWithoutFunctions() {
		List<Expression> potentialExpressions = new ArrayList<Expression>();
		Set<Expression> randomVariableDefinitions = new LinkedHashSet<Expression>();
		List<Expression> expected = new ArrayList<Expression>();
		
		// mother(john) = mother(bob)
		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("mother(john) = mother(bob);")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if mother(john, X0) and mother(bob, X1) and X0 = X1 then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("mother(john) = mother(bob) = jane;")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if mother(john, X0) and mother(bob, X1) and X0 = X1 = jane then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));
		
		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("mother(john) = jane = mother(bob);")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if mother(john, X0) and mother(bob, X1) and X0 = jane = X1 then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));
		
		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("jane = mother(john) = mother(bob);")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if mother(john, X0) and mother(bob, X1) and jane = X0 = X1 then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("sick(mother(X));")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if mother(X, X0) and sick(X0) then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("sick(mother(X, Y, Z));")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if mother(X, Y, Z, X0) and sick(X0) then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, X1, X2, Y) then if not mother(X0, X1, X2, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, X1, X2, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("sick(mother(X), bob);")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if mother(X, X0) and sick(X0, bob) then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("sick(bob, mother(X));")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if mother(X, X0) and sick(bob, X0) then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("sick(mother(X), father(X));")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if mother(X, X0) and father(X, X1) and sick(X0, X1) then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		expected.add(lowParser.parse("if father(X0, Y) then if not father(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : father(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("sick(mother(father(X)));")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if 'and'(father(X, X1), mother(X1, X0)) and sick(X0) then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		expected.add(lowParser.parse("if father(X0, Y) then if not father(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : father(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("mother(john) = bestfriend(mother(bob));")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if and(mother(john, X0), and(mother(bob, X2), bestfriend(X2, X1)), (X0 = X1)) then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		expected.add(lowParser.parse("if bestfriend(X0, Y) then if not bestfriend(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : bestfriend(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		// Test multiple tiered functions.
		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("sick(bob, mother(bestfriend(X)));")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if and(and(bestfriend(X, X1), mother(X1, X0)), sick(bob, X0)) then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		expected.add(lowParser.parse("if bestfriend(X0, Y) then if not bestfriend(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : bestfriend(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));

		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse("sick(bob, mother(bestfriend(father(X))));")));
		randomVariableDefinitions.clear();
		expected.clear();
		expected.add(lowParser.parse("if and(and(and(father(X, X2), bestfriend(X2, X1)), mother(X1, X0)), sick(bob, X0)) then 1 else 0"));
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		expected.add(lowParser.parse("if bestfriend(X0, Y) then if not bestfriend(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : bestfriend(X0, Y) then 1 else 0"));
		expected.add(lowParser.parse("if father(X0, Y) then if not father(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : father(X0, Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));
	}
	
	@Test
	public void testUpdatingRandomVariableDeclaration () {
		Expression result, input;
		input = new DefaultCompoundSyntaxTree("randomVariable", "mother", 1, "People", "People");
		result = ruleConverter.updateRandomVariableDeclaration(input);
		assertEquals(lowParser.parse("randomVariable(mother, 2, People, People, Boolean)"), result);

		input = new DefaultCompoundSyntaxTree("randomVariable", "president", 0, "People");
		result = ruleConverter.updateRandomVariableDeclaration(input);
		assertEquals(lowParser.parse("randomVariable(president, 1, People, Boolean)"), result);

		input = new DefaultCompoundSyntaxTree("sort", "sprinters", "bolt", "johnson");
		result = ruleConverter.updateRandomVariableDeclaration(input);
		assertEquals(null, result);
	}
	
	@Test
	public void testDifferentiatingNullaryRandomFunctionsAndConstants() {
		List<Expression> potentialExpressions = new ArrayList<Expression>();
		Set<Expression> randomVariableDefinitions = new LinkedHashSet<Expression>();
		List<Expression> expected = new ArrayList<Expression>();
		
		// Test of zero arg function.
		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(ruleParser.parse("if president = obama then government(socialism) else government(freedom);")));
		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(president, 0, Boolean)"));
		expected.clear();
		expected.add(lowParser.parse("if president(X0) and X0 = obama then if government(socialism) then 1 else 0 else (if government(freedom) then 1 else 0)"));
		expected.add(lowParser.parse("if president(Y) then if not president(Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : president(Y) then 1 else 0"));
		assertEquals(expected, ruleConverter.translateFunctions(potentialExpressions, randomVariableDefinitions));	
	}

	@Test
	public void testTranslateQuantifiedPotentialExpressionsIntoQuantifierFreePotentialExpressions() {
		Set<Expression> randomVariableDefinitions = new LinkedHashSet<Expression>();
		List<Expression> potentialExpressions = new ArrayList<Expression>();
		List<Expression> expected = new ArrayList<Expression>();
		
		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(young, 1, Person, Boolean)"));
		randomVariableDefinitions.add(lowParser.parse("randomVariable(friends, 2, Person, Person, Boolean)"));
		randomVariableDefinitions.add(lowParser.parse("randomVariable(smokes, 1, Person, Boolean)"));
		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse(
				"if young(X) and (for all Y : (friends(Y,X) => smokes(Y))) then smokes(X) 0.8;")));
		expected.clear();
		expected.add(ruleConverter.translateConditionalRule(ruleParser.parse("if young(X) and 'for all Y : friends(Y, X) => smokes(Y)'(X) then smokes(X) 0.8;")));
		expected.add(ruleConverter.translateConditionalRule(ruleParser.parse("if not (friends(Y, X) => smokes(Y)) then not 'for all Y : friends(Y, X) => smokes(Y)'(X);")));
		assertEquals(expected, ruleConverter.translateQuantifiers(potentialExpressions, randomVariableDefinitions));
		assertEquals(4, randomVariableDefinitions.size());
		Assert.assertTrue(randomVariableDefinitions.contains(lowParser.parse("randomVariable('for all Y : friends(Y, X) => smokes(Y)', 1, Person, Boolean)")));

		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(president, 2, Person, Nation, Boolean)"));
		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(ruleParser.parse("there exists X : president(X, Country);")));
		expected.clear();
		expected.add(ruleConverter.translateRule(ruleParser.parse("'there exists X : president(X, Country)'(Country);")));
		expected.add(ruleConverter.translateRule(ruleParser.parse("if president(X, Country) then 'there exists X : president(X, Country)'(Country);")));
		assertEquals(expected, ruleConverter.translateQuantifiers(potentialExpressions, randomVariableDefinitions));
		assertEquals(2, randomVariableDefinitions.size());		
		Assert.assertTrue(randomVariableDefinitions.contains(lowParser.parse("randomVariable('there exists X : president(X, Country)', 1, Nation, Boolean)")));

		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(friends, 2, Person, Person, Boolean)"));
		potentialExpressions.clear();
		potentialExpressions.add(ruleConverter.translateRule(this.ruleParser.parse("friends(X,Y) and (there exists Z : friends(X,Z));")));
		expected.clear();
		expected.add(ruleConverter.translateRule(ruleParser.parse("friends(X,Y) and 'there exists Z : friends(X, Z)'(X);")));
		expected.add(ruleConverter.translateRule(ruleParser.parse("if friends(X,Z) then 'there exists Z : friends(X, Z)'(X);")));
		assertEquals(expected, ruleConverter.translateQuantifiers(potentialExpressions, randomVariableDefinitions));
		assertEquals(2, randomVariableDefinitions.size());		
		Assert.assertTrue(randomVariableDefinitions.contains(lowParser.parse("randomVariable('there exists Z : friends(X, Z)', 1, Person, Boolean)")));

		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(friends, 2, Person, Person, Boolean)"));
		randomVariableDefinitions.add(lowParser.parse("randomVariable(loves, 2, Person, Person, Boolean)"));
		potentialExpressions.clear();
		potentialExpressions.add(this.ruleConverter.translateRule(this.ruleParser.parse(
				"friends(X,Y) and (there exists Z : Z may be same as X and loves(X,Z));")));
		expected.clear();
		expected.add(ruleConverter.translateRule(ruleParser.parse("friends(X,Y) and 'there exists Z : \\\'. may be same as .\\\'(Z, X) and loves(X, Z)'(X);")));
		expected.add(ruleConverter.translateRule(ruleParser.parse("if Z may be same as X and loves(X,Z) then 'there exists Z : \\\'. may be same as .\\\'(Z, X) and loves(X, Z)'(X);")));
		assertEquals(expected, ruleConverter.translateQuantifiers(potentialExpressions, randomVariableDefinitions));
		assertEquals(3, randomVariableDefinitions.size());		
		Assert.assertTrue(randomVariableDefinitions.contains(lowParser.parse("randomVariable('there exists Z : \\\'. may be same as .\\\'(Z, X) and loves(X, Z)', 1, Person, Boolean)")));

		// Test nested quantifiers.
		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(loves, 3, Person, Amount, Object, Boolean)"));
		potentialExpressions.clear();
		potentialExpressions.add(ruleConverter.translateRule(this.ruleParser.parse("there exists Z : for all Y : loves(X, Y, Z);")));
		expected.clear();
		expected.add(ruleConverter.translateRule(ruleParser.parse("'there exists Z : for all Y : loves(X, Y, Z)'(X);")));
		expected.add(ruleConverter.translateRule(ruleParser.parse("if 'for all Y : loves(X, Y, Z)'(X, Z) then 'there exists Z : for all Y : loves(X, Y, Z)'(X);")));
		expected.add(ruleConverter.translateRule(ruleParser.parse("if not loves(X, Y, Z) then not 'for all Y : loves(X, Y, Z)'(X, Z);")));
		assertEquals(expected, ruleConverter.translateQuantifiers(potentialExpressions, randomVariableDefinitions));
		assertEquals(3, randomVariableDefinitions.size());		
		Assert.assertTrue(randomVariableDefinitions.contains(lowParser.parse("randomVariable('for all Y : loves(X, Y, Z)', 2, Person, Object, Boolean)")));
		Assert.assertTrue(randomVariableDefinitions.contains(lowParser.parse("randomVariable('there exists Z : for all Y : loves(X, Y, Z)', 1, Person, Boolean)")));

		doTreeUtilWaitUnilClosed(); 
	}
	
	@Test
	public void testDisembedConstraints () {
		List<Expression> potentialExpressions;
		List<Pair<Expression, Expression>> expected;

		potentialExpressions = new ArrayList<Expression>();
		potentialExpressions.add(lowParser.parse(
				"if friends(X,Y) and likes(X,Z) and '. may be same as .'(X, Z) then if likes(Y,Z) then 0.8 else 0.2 else 0.5"));
		expected = new ArrayList<Pair<Expression, Expression>>();
		expected.add(new Pair<Expression, Expression>(
				lowParser.parse("if friends(X, Y) and likes(X, Z) then if likes(Y, Z) then 0.8 else 0.2 else 0.5"),
				lowParser.parse("Y != Z and Y != X")));
		assertEquals(expected, ruleConverter.disembedConstraints(potentialExpressions));

		potentialExpressions = new ArrayList<Expression>();
		potentialExpressions.add(lowParser.parse(
				"if colleagues(X,Y) and Y != bob then (if likes(X,Y) then 0.8 else 0.2) else 0.5"));
		expected = new ArrayList<Pair<Expression, Expression>>();
		expected.add(new Pair<Expression, Expression>(
				lowParser.parse("if colleagues(X, Y) then if likes(X, Y) then 0.8 else 0.2 else 0.5"),
				lowParser.parse("Y != X and Y != bob")));
		assertEquals(expected, ruleConverter.disembedConstraints(potentialExpressions));
	}
	
	@Test
	public void testCreateModel () {
		Set<Expression> sorts = new LinkedHashSet<Expression>();
		sorts.add(lowParser.parse("sort(People,   Unknown, {ann, bob})"));
		sorts.add(lowParser.parse("sort(Treasure, Unknown, {gold, silver, diamonds})"));
		
		Set<Expression> randomVariables = new LinkedHashSet<Expression>();
		randomVariables.add(lowParser.parse("randomVariable(gaveTreasureTo, 3, People, Treasure, People)"));
		randomVariables.add(lowParser.parse("randomVariable(owns, 2, People, Treasure)"));
		randomVariables.add(lowParser.parse("randomVariable(rich, 1, People)"));

		Set<Expression> parfactors = new LinkedHashSet<Expression>();
		parfactors.add(lowParser.parse(
				"{{(on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X,Z,Y) then (if owns(Y,Z) then 1 else 0)  else 1] }}"));
		parfactors.add(lowParser.parse(
				"{{(on X in People, Z in Treasure) [if owns(X,Z) then if rich(X) then 1 else 0 else 1] }}"));

		ruleConverter.createModel("Gave Treasure To", 
				"An example of how hard it is to model things without aggregate factors.", 
				sorts, randomVariables, parfactors);
// TODO test the created model is as expected.		
	}
	
	// Tests For:
	// http://code.google.com/p/aic-praise/wiki/TranslatingInferenceInputAndOutput
	//

	@Test
	public void testEncodingQueries() {
		String string;
		Pair<Expression, Expression> result, expected;
		
		string = "sick(X)";
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string));
		expected = new Pair<Expression, Expression>(ruleParser.parseFormula("query(X)"), 
				ruleParser.parse("query(X) <=> sick(X);"));
		assertEquals(expected, result);
		
		string = "sick(john) and sick(mary)";
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string));
		expected = new Pair<Expression, Expression>(ruleParser.parseFormula("query"), 
				ruleParser.parse("query <=> sick(john) and sick(mary);"));
		assertEquals(expected, result);

		string = "not sick(X)";
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string));
		expected = new Pair<Expression, Expression>(ruleParser.parseFormula("query(X)"), 
				ruleParser.parse("query(X) <=> not sick(X);"));
		assertEquals(expected, result);

		string = "there exists X : friends(X,Y)";
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string));
		expected = new Pair<Expression, Expression>(ruleParser.parseFormula("query(Y)"), 
				ruleParser.parse("query(Y) <=> there exists X : friends(X,Y);"));
		assertEquals(expected, result);

		string = "conspiracy(C) and leader(C) = X and member(C,Y) and member(C,Z)";
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string));
		expected = new Pair<Expression, Expression>(ruleParser.parseFormula("query(C, Y, X, Z)"), 
				ruleParser.parse("query(C, Y, X, Z) <=> conspiracy(C) and leader(C) = X and member(C,Y) and member(C,Z);"));
		assertEquals(expected, result);

		string = "mother(X) = lucy";
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string));
		expected = new Pair<Expression, Expression>(ruleParser.parseFormula("query(X)"), 
				ruleParser.parse("query(X) <=> mother(X) = lucy;"));
		assertEquals(expected, result);
	}
	
	@Test
	public void testTransformingQueryOutputsIntoHighLevelSyntax() {
		Expression input, result, expected;

		input = lowParser.parse("if sick(bob) then 0.3 else 0.7");
		result = ruleConverter.potentialExpressionToRule(input);
		expected = ruleParser.parse("sick(bob) 0.3;");
		assertEquals(expected, result);

		input = lowParser.parse("if X = bob then if sick(bob) then 0.7 else 0.3 else if sick(X) then 0.2 else 0.8");
		result = ruleConverter.potentialExpressionToRule(input);
		expected = ruleParser.parse("if X = bob then sick(bob) 0.7 else sick(X) 0.2;");
		assertEquals(expected, result);
	}
	
	//
	// Tests for additional issues that have been encountered
	//
	@Test
	public void testIssue6() throws ReservedWordException {
		// Test for issue #6
		// http://code.google.com/p/aic-praise/issues/detail?id=6
		//
		// entityOf(m1) = obama;
		// ->
		// {{ ( on ) ([ if entityOf(m1, obama) then 1 else 0 ]) | X0 = obama }} 
		// incorrectly.
		List<Expression> inputRules = new ArrayList<Expression>();
		inputRules.add(ruleParser.parse("entityOf(m1) = obama;"));
		RuleConverter.LowLevelSyntax lowLevelSyntax = ruleConverter.translateToLowLevelSyntax(inputRules);
		
		Assert.assertTrue(lowLevelSyntax.getParfactors().contains(lowParser.parse("{{ (on) [if entityOf(m1, obama) then 1 else 0] | true }}")));
		
		// Additional case with explicit assignment for just 1 of the terms
		inputRules.clear();
		inputRules.add(ruleParser.parse("entityOf(X0) = obama;"));
		lowLevelSyntax = ruleConverter.translateToLowLevelSyntax(inputRules);
		Assert.assertTrue(lowLevelSyntax.getParfactors().contains(lowParser.parse("{{ (on X0) [if entityOf(X0, obama) then 1 else 0] | X0 != obama }}")));

		// Additional case with explicit assignment for just 1 of the terms
		inputRules.clear();
		inputRules.add(ruleParser.parse("if entityOf(X, Y) and X may be same as Y and Y = obama then 1;"));
		lowLevelSyntax = ruleConverter.translateToLowLevelSyntax(inputRules);
		Assert.assertTrue(lowLevelSyntax.getParfactors().contains(lowParser.parse("{{ ( on X ) ([ if entityOf(X, obama) then if 1 then true else 0 else 0.5 ]) | true }}")));
	}
	
	//
	// Test Supporting Routines
	//	
	@Test
	public void testCreateTransformedFunctionConstraints () {
		List<Expression> expected;

		List<Expression> parfactors = new ArrayList<Expression>();
		ruleConverter.createTransformedFunctionConstraints("president", 0, parfactors);
		expected = new ArrayList<Expression>();
		expected.add(lowParser.parse("if president(Y) then if not president(Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : president(Y) then 1 else 0"));
		assertEquals(expected, parfactors);

		parfactors = new ArrayList<Expression>();
		ruleConverter.createTransformedFunctionConstraints("mother", 1, parfactors);
		expected = new ArrayList<Expression>();
		expected.add(lowParser.parse("if mother(X0, Y) then if not mother(X0, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : mother(X0, Y) then 1 else 0"));
		assertEquals(expected, parfactors);

		parfactors = new ArrayList<Expression>();
		ruleConverter.createTransformedFunctionConstraints("foo", 3, parfactors);
		expected = new ArrayList<Expression>();
		expected.add(lowParser.parse("if foo(X0, X1, X2, Y) then if not foo(X0, X1, X2, Z) then 1 else 0 else 0.5"));
		expected.add(lowParser.parse("if there exists Y : foo(X0, X1, X2, Y) then 1 else 0"));
		assertEquals(expected, parfactors);
	}
		
	@Test
	public void testIsRandomFunctionApplication () {
		Expression input;
		input = new DefaultCompoundSyntaxTree ("if . then . else .", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = DefaultSymbol.createSymbol("foo");
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree ("and", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree ("or", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree ("not", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree ("<=>", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree ("=>", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree ("there exists . : .", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree ("for all . : .", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree (". may be same as .", "A", "B");
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree("mother", 1, 2, 3, 4);
		Assert.assertEquals(true, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree("+", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));
		
		input = new DefaultCompoundSyntaxTree("-", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));
		
		input = new DefaultCompoundSyntaxTree("*", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));
		
		input = new DefaultCompoundSyntaxTree("/", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));
		
		input = new DefaultCompoundSyntaxTree("^", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = new DefaultCompoundSyntaxTree("=", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

	}

	@Test
	public void testCreateQueryDeclaration () {
		Expression queryAtom, query, result, expected;
		Set<Expression> randomVariables = new LinkedHashSet<Expression>();

		query = ruleParser.parseFormula("sick(john) and sick(mary)");
		queryAtom = lowParser.parse("query");
		randomVariables.clear();
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables);
		expected = ruleParser.parse("random query: -> Boolean;");
		assertEquals(expected, result);

		query = ruleParser.parseFormula("not sick(X)");
		queryAtom = lowParser.parse("query(X)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random sick: People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables);
		expected = ruleParser.parse("random query: People -> Boolean;");
		assertEquals(expected, result);

		query = ruleParser.parseFormula("there exists X : friends(X,Y)");
		queryAtom = lowParser.parse("query(Y)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random friends: People x People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables);
		expected = ruleParser.parse("random query: People -> Boolean;");
		assertEquals(expected, result);

		query = ruleParser.parseFormula("conspiracy(C) and leader(C) = X and member(C,Y) and member(C,Z)");
		queryAtom = lowParser.parse("query(C, Y, X, Z)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random conspiracy: Concept -> Boolean;" +
				"random leader: Concept x People -> Boolean;" +
				"random member: Concept x People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables);
		expected = ruleParser.parse("random query: Concept x People x People x People -> Boolean;");
		assertEquals(expected, result);

		query = ruleParser.parseFormula("conspiracy(C) and leader = X and member(C,Y) and member(C,Z)");
		queryAtom = lowParser.parse("query(C, Y, X, Z)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random conspiracy: Concept -> Boolean;" +
				"random leader: People -> Boolean;" +
				"random member: Concept x People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables);
		expected = ruleParser.parse("random query: Concept x People x People x People -> Boolean;");
		assertEquals(expected, result);

		query = ruleParser.parseFormula("mother(X) = lucy");
		queryAtom = lowParser.parse("query(X)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random mother: People x People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables);
		expected = ruleParser.parse("random query: People -> Boolean;");
		assertEquals(expected, result);
		
	}
	
	@Test
	public void testQueryResultToRule () {
		Expression input, queryAtom, query, result, expected;

		input = lowParser.parse("if query then 0.3 else 0.7");
		queryAtom = lowParser.parse("query");
		query = lowParser.parse("sick(bob)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query);
		expected = ruleParser.parse("sick(bob) 0.3;");
		assertEquals(expected, result);

		input = lowParser.parse("if query(bob) then 0.3 else 0.7");
		queryAtom = lowParser.parse("query(X)");
		query = lowParser.parse("sick(X)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query);
		expected = ruleParser.parse("sick(bob) 0.3;");
		assertEquals(expected, result);

		input = lowParser.parse("if query(bob, mary) then 0.3 else 0.7");
		queryAtom = lowParser.parse("query(X, Y)");
		query = lowParser.parse("sick(X) = cold(Y)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query);
		expected = ruleParser.parse("sick(bob) = cold(mary) 0.3;");
		assertEquals(expected, result);

		input = lowParser.parse("if query(X = bob, Z) then 0.3 else 0.7");
		queryAtom = lowParser.parse("query(X, Y)");
		query = lowParser.parse("sick(X) = cold(Y)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query);
		expected = ruleParser.parse("sick(X = bob) = cold(Z) 0.3;");
		assertEquals(expected, result);

		// Test a case where there is overlap between the query args in the input
		// and the query atom's args.
		input = lowParser.parse("if query(Y, X) then 0.3 else 0.7");
		queryAtom = lowParser.parse("query(X, Y)");
		query = lowParser.parse("sick(X) = cold(Y)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query);
		expected = ruleParser.parse("sick(Y) = cold(X) 0.3;");
		assertEquals(expected, result);

	}

	@Test
	public void testToRuleString () {
		String string, result, expected;
		Expression inputExpression, outputExpression;

		// Testing atomic rules.
		string = "sick(X);";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X);";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 1.0;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X);";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 1.000000;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X);";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3 + 0.1;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 + 0.1;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3+0.1;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 + 0.1;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3 * 0.1;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 * 0.1;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3*0.1;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 * 0.1;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3 - 0.1;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 - 0.1;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3-0.1;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 - 0.1;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3 minus 0.1;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 minus 0.1;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3 / 2;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 / 2;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3/2;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 / 2;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3 ^ 2;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 ^ 2;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) 0.3^2;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) 0.3 ^ 2;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) and happy(X);";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) and happy(X);";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) and happy(X) 0.1;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) and happy(X) 0.1;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(john) = sick(bob);";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(john) = sick(bob);";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(john) != sick(bob);";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(john) != sick(bob);";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		// Testing conditional rules.
		string = "if circle(X) then round(X);";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if circle(X) then round(X);";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "if epidemic then sick(X) 0.7;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if epidemic then sick(X) 0.7;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "if epidemic then sick(X) and unhappy(X) 0.9;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if epidemic then sick(X) and unhappy(X) 0.9;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "if chilly(P) and live(X, P) then sick(X) 0.6;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if chilly(P) and live(X, P) then sick(X) 0.6;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "if colleagues(X,Y) then likes(X,Y) 0.8;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if colleagues(X, Y) then likes(X, Y) 0.8;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "if epidemic then if sick(X) and friends(X,Y) then sick(Y) 0.8;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if epidemic then if sick(X) and friends(X, Y) then sick(Y) 0.8;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "if epidemic then if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Z) else sick(A);";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if epidemic then if sick(X) and friends(X, Y) then sick(Y) 0.8 else sick(Z) else sick(A);";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y);";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if sick(X) and friends(X, Y) then sick(Y) 0.8 else sick(Y);";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y) 0.3;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if sick(X) and friends(X, Y) then sick(Y) 0.8 else sick(Y) 0.3;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "if epidemic then 0.7 sick(X) :- not vaccinated(X). else 0.7 sick(X).;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "if epidemic then 0.7 sick(X) :- not vaccinated(X). else 0.7 sick(X).;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		// Testing prolog rules.
		string = "sick(john). ;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(john). ;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "1 sick(john). ;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(john). ;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "1.0 sick(john). ;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(john). ;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X). ;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X). ;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "not sick(mary). ;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "not sick(mary). ;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "0.3 sick(X). ;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "0.3 sick(X). ;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "round(X) :- circle(X). ;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "round(X) :- circle(X). ;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "0.7 sick(X) :- epidemic and not vaccinated(X). ;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "0.7 sick(X) :- epidemic and not vaccinated(X). ;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);
		
		// Standard probability rules.
		string = "P(sick(X) | epidemic) = 0.8;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "P(sick(X) | epidemic) = 0.8;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "P(sick(X) and happy(Y) | mother(Z)) = 0.4;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "P(sick(X) and happy(Y) | mother(Z)) = 0.4;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		// Causal rules.
		string = "sick(X) -> fever(X) 0.6;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) -> fever(X) 0.6;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X) and happy(Y) -> fever(X) 0.6;";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X) and happy(Y) -> fever(X) 0.6;";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);
	}
	
	@Test
	public void testConvert() {
		String modelString, queryString;
// TODO - test result is what is expected.		
		@SuppressWarnings("unused")
		Pair<Expression, Model> result;

		modelString = "if mother(X) = Y then X != Y;" +
				"if trait(mother(X)) then trait(X) 0.8 else trait(X) 0.3;" +
				"there exists X : trait(X);" +
				"mother(bob)=mary;" +
				"mother(ann)=mary;" +
				"mother(john)=ann;" +
				"trait(john);";
		queryString = "trait(mary)";
		try {
			result = ruleConverter.translateQuery("Test Model", "Description", modelString, queryString);
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (ModelException e) {
			e.printStackTrace();
			Assert.fail("Errors in model string " + modelString + ": " + Util.join(e.getErrors()));
		}

		// Missing sort declaration for People.
		modelString = "random president: -> People;" +
				"random firstLady: -> People;" +
				"president = barrackObama <=> firstLady = michelleObama;" +
				"president = billClinton <=> firstLady = hillaryClinton;" +
				"firstLady = michelleObama 0.9;";
		queryString = "president";
		try {
			result = ruleConverter.translateQuery("Test Model", "Description", modelString, queryString);
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (ModelException e) {
			e.printStackTrace();
			Assert.fail("Errors in model string " + modelString + ": " + Util.join(e.getErrors()));
		}

		modelString = "there exists X : X = bestFriend(X) 0.9;";
		queryString = "bestFriend(john)";
		try {
			result = ruleConverter.translateQuery("Test Model", "Description", modelString, queryString);
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (ModelException e) {
			e.printStackTrace();
			Assert.fail("Errors in model string " + modelString + ": " + Util.join(e.getErrors()));
		}
		
		modelString = "/**\n"+
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
				"fever(X) 0.001;\n";
		queryString = "sick(X)";
		try {
			result = ruleConverter.translateQuery("Test Model", "Description", modelString, queryString);
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (ModelException e) {
			e.printStackTrace();
			Assert.fail("Errors in model string " + modelString + ": " + Util.join(e.getErrors()));
		}

		modelString = "// RANDOM VARIABLE DECLARATIONS:" +
				"random epidemic: -> Boolean;\n" +
				"random sick: People -> Boolean;\n" +
				"random fever: People -> Boolean;\n" +
				"random happy: People -> Boolean;\n" +
				"random mother: People -> People;\n" +
				"\n"+
				"//\n" +
				"// RULES\n" +
				"// By default, how likely is an epidemic?\n" +
				"epidemic 0.01 and \n" +
				"\n"+
				"(if epidemic then sick(X) 0.6 else not sick(X)) and \n" +
				"if sick(mother(X)) then sick(X) 0.5 else sick(X) 0.1;\n";
		queryString = "sick(mother(X)) and happy(Y)";
		try {
			result = ruleConverter.translateQuery("Test Model", "Description", modelString, queryString);
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (ModelException e) {
			e.printStackTrace();
			Assert.fail("Errors in model string " + modelString + ": " + Util.join(e.getErrors()));
		}
	}
	


	/*===================================================================================
	 * PRIVATE METHODS
	 *=================================================================================*/		
	private void testTranslateRules(Expression input, Expression expectedResult) {
		testTranslateRules(null, input, true, true, expectedResult);
	}

	private void testTranslateRules (String inputString, Expression inputExpr, boolean expectSucceed, boolean checkResult, Expression expectedResult) {
		Expression result;
		if (inputExpr == null) {
			result = ruleConverter.translateRule(ruleParser.parse(inputString));
		} 
		else {
			result = ruleConverter.translateRule(inputExpr);
		}
		if (expectSucceed) {
			if (checkResult) {
				assertEquals(expectedResult.toString(), result.toString());
			}
			else {
				Assert.assertNotNull(result);
			}
		}
		else {
			Assert.assertNull(result);
		}
	}
	
	// Note: Pass the VM argument
    // -Dgrinder.wait.until.ui.closed.enabled=true
    // in the Eclipse run configuration for this test, in order for the tree to stay up when this is called.
	private void doTreeUtilWaitUnilClosed() {
        if (GrinderConfiguration.isWaitUntilUIClosedEnabled()) {
            TreeUtil.waitUntilUIClosed();
        }
    } 
}
