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
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityOfType;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.grinder.ui.TreeUtil;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.Model.ModelException;
import com.sri.ai.praise.rules.QueryContainsUnknownRandomVariablesException;
import com.sri.ai.praise.rules.ReservedWordException;
import com.sri.ai.praise.rules.RuleConverter;
import com.sri.ai.praise.rules.antlr.RuleParserWrapper;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;

public class RuleConverterTest {
	
	private final static int DEFAULT_DOMAIN_SIZE = 1000;
	
	private RuleParserWrapper         ruleParser;
	private RuleConverter             ruleConverter;
	private AntlrGrinderParserWrapper lowParser;

	@Before
	public void setUp () {
		
		ruleParser = new RuleParserWrapper();
		ruleConverter = new RuleConverter();
		
		lowParser = new AntlrGrinderParserWrapper();
	}
	
	// Tests for:
	// http://code.google.com/p/aic-praise/wiki/SyntaxAndMeaningOfProbabilisticRules
	// http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	//
	
    // NOTE: Currently expected as we have disabled support for rules with functions.
	@Test(expected=UnsupportedOperationException.class)
	public void testTranslateFunctions() {
		List<Expression> rules                     = new ArrayList<Expression>();
		Set<Expression>  randomVariableDefinitions = new LinkedHashSet<Expression>();
		Set<Expression>  expectedRules             = new LinkedHashSet<Expression>();
		Set<Expression>  exprectedRandomVariables  = new LinkedHashSet<Expression>();
		
		Pair<List<Expression>, Set<Expression>> translateFunctionsResult;
		
		// mother(john) = mother(bob)
		rules.clear();
		rules.add(this.ruleParser.parse("mother(john) = mother(bob);"));
		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(ruleParser.parse("random mother: Person -> Person;"));
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(bob, X0) then mother(john, X0);"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		exprectedRandomVariables.clear();
		exprectedRandomVariables.add(ruleParser.parse("random mother: Person x Person -> Boolean;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		assertEquals(exprectedRandomVariables, translateFunctionsResult.second);
		// Now see if can just get the rules
		randomVariableDefinitions.clear();
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		exprectedRandomVariables.clear();
		assertEquals(exprectedRandomVariables, translateFunctionsResult.second);

		rules.clear();
		rules.add(this.ruleParser.parse("mother(john) = mother(bob) = jane;"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(bob, X0) then mother(john, X0) and mother(bob, jane);"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("mother(john) = jane = mother(bob);"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("mother(john, jane) and mother(bob, jane);"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("jane = mother(john) = mother(bob);"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("mother(john, jane) and mother(bob, jane);"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("sick(mother(X));"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(X, X0) then sick(X0) and X0 may be same as X;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("sick(mother(X)) 0.8;"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(X, X0) then sick(X0) and X0 may be same as X 0.8;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("sick(mother(X, Y));"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(X, Y, X0) then sick(X0) and X0 may be same as X and X0 may be same as Y;"));
		expectedRules.add(ruleParser.parse("if mother(X0, X1, Y) then not mother(X0, X1, Z) and Y may be same as X0 and Z may be same as X0 and Y may be same as X1 and Z may be same as X1;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, X1, Y) and Y may be same as X0 and Y may be same as X1;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("sick(mother(X), bob);"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(X, X0) then sick(X0, bob) and X0 may be same as X;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("sick(bob, mother(X));"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(X, X0) then sick(bob, X0) and X0 may be same as X;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("sick(mother(X), father(X));"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(X, X0) and father(X, X1) then ((sick(X0, X1) and X1 may be same as X) and X0 may be same as X and X0 may be same as X1);"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		expectedRules.add(ruleParser.parse("if father(X0, Y) then not father(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : father(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("sick(mother(father(X)));"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if father(X, X1) then if mother(X1, X0) and X1 may be same as X then sick(X0) and X0 may be same as X and X0 may be same as X1;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		expectedRules.add(ruleParser.parse("if father(X0, Y) then not father(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : father(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("mother(john) = bestfriend(mother(bob));"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(bob, X1) and X0 may be same as X1 then if bestfriend(X1, X0) then mother(john, X0);"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		expectedRules.add(ruleParser.parse("if bestfriend(X0, Y) then not bestfriend(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : bestfriend(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		// Test multiple tiered functions.
		rules.clear();
		rules.add(this.ruleParser.parse("sick(bob, mother(bestfriend(X)));"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if bestfriend(X, X1) then if mother(X1, X0) and X1 may be same as X then sick(bob, X0) and X0 may be same as X and X0 may be same as X1;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		expectedRules.add(ruleParser.parse("if bestfriend(X0, Y) then not bestfriend(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : bestfriend(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("sick(bob, mother(bestfriend(father(X))));"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if father(X, X2) then if bestfriend(X2, X1) and X2 may be same as X then if mother(X1, X0) and X1 may be same as X and X1 may be same as X2 then sick(bob, X0) and X0 may be same as X and X0 may be same as X2 and X0 may be same as X1;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		expectedRules.add(ruleParser.parse("if bestfriend(X0, Y) then not bestfriend(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : bestfriend(X0, Y) and Y may be same as X0;"));
		expectedRules.add(ruleParser.parse("if father(X0, Y) then not father(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : father(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("if gender(Possessive) = gender(Entity) then referenceOf(Possessive, Entity) 0.7;"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if gender(Entity, X0) and X0 may be same as Entity and X0 may be same as Possessive then if gender(Possessive, X0) then referenceOf(Possessive, Entity) 0.7;"));
		expectedRules.add(ruleParser.parse("if gender(X0, Y) then not gender(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : gender(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("if gender(Possessive) = gender(Entity) then referenceOf(Possessive, Entity) 0.7 else referenceOf(Possessive, Entity) 0.4;"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if gender(Entity, X0) and X0 may be same as Entity and X0 may be same as Possessive then if gender(Possessive, X0) then referenceOf(Possessive, Entity) 0.7 else referenceOf(Possessive, Entity) 0.4;"));
		expectedRules.add(ruleParser.parse("if gender(X0, Y) then not gender(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : gender(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));

		rules.clear(); // From PRAiSEDemoApp Example #4.
		rules.add(this.ruleParser.parse("if possessive(Possessive) and noun(AnotherWord) and gender(Possessive) = gender(entityOfWord(AnotherWord)) then referenceOf(Possessive) = entityOfWord(AnotherWord) 0.7;"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if entityOfWord(AnotherWord, X1) then if gender(X1, X0) and X0 may be same as AnotherWord and X0 may be same as X1 and X0 may be same as Possessive then if possessive(Possessive) and noun(AnotherWord) and gender(Possessive, X0) then if entityOfWord(AnotherWord, X1) then referenceOf(Possessive, X1) and X1 may be same as AnotherWord and X1 may be same as Possessive 0.7;"));
		expectedRules.add(ruleParser.parse("if entityOfWord(X0, Y) then not entityOfWord(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : entityOfWord(X0, Y) and Y may be same as X0;"));
		expectedRules.add(ruleParser.parse("if gender(X0, Y) then not gender(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : gender(X0, Y) and Y may be same as X0;"));
		expectedRules.add(ruleParser.parse("if referenceOf(X0, Y) then not referenceOf(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : referenceOf(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("0.8 sick(mother(X))."));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if mother(X, X0) then sick(X0) and X0 may be same as X 0.8;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("0.8 sick(mother(X)) :- epidemic(Y)."));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if epidemic(Y) then if mother(X, X0) then sick(X0) and X0 may be same as Y and X0 may be same as X 0.8;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("P(sick(mother(X)) | epidemic(Y)) = 0.8;"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if epidemic(Y) then if mother(X, X0) then sick(X0) and X0 may be same as Y and X0 may be same as X 0.8;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		
		rules.clear();
		rules.add(this.ruleParser.parse("epidemic(Y) -> sick(mother(X)) 0.8;"));
		randomVariableDefinitions.clear();
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if epidemic(Y) then if mother(X, X0) then sick(X0) and X0 may be same as Y and X0 may be same as X 0.8;"));
		expectedRules.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expectedRules.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
	}
	
	// NOTE: Currently expected as we have disabled support for rules with quantifiers.
	@Test(expected=UnsupportedOperationException.class)
	public void testTranslateQuantifiers() {
		List<Expression> rules                     = new ArrayList<Expression>();
		Set<Expression>  randomVariableDefinitions = new LinkedHashSet<Expression>();
		Set<Expression>  expectedRules             = new LinkedHashSet<Expression>();
		
		Pair<List<Expression>, Set<Expression>> translateFunctionsResult;
		
		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(young, 1, Person, Boolean)"));
		randomVariableDefinitions.add(lowParser.parse("randomVariable(friends, 2, Person, Person, Boolean)"));
		randomVariableDefinitions.add(lowParser.parse("randomVariable(smokes, 1, Person, Boolean)"));
		rules.clear();
		rules.add(this.ruleParser.parse("if young(X) and (for all Y : (friends(Y,X) => smokes(Y))) then smokes(X) 0.8;"));
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if young(X) and 'for all Y : friends(Y, X) => smokes(Y)'(X) then smokes(X) 0.8;"));
		expectedRules.add(ruleParser.parse("if not (friends(Y, X) => smokes(Y)) then not 'for all Y : friends(Y, X) => smokes(Y)'(X) else 'for all Y : friends(Y, X) => smokes(Y)'(X) 0.999999999;"));
		translateFunctionsResult = ruleConverter.translateQuantifiers(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		assertEquals(4, translateFunctionsResult.second.size());
		Assert.assertTrue(translateFunctionsResult.second.contains(lowParser.parse("randomVariable('for all Y : friends(Y, X) => smokes(Y)', 1, Person, Boolean)")));

		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(president, 2, Person, Nation, Boolean)"));
		rules.clear();
		rules.add(ruleParser.parse("there exists X : president(X, Country);"));
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("'there exists X : president(X, Country)'(Country);"));
		expectedRules.add(ruleParser.parse("if president(X, Country) then 'there exists X : president(X, Country)'(Country) else 'there exists X : president(X, Country)'(Country) 0.000000001;"));
		translateFunctionsResult = ruleConverter.translateQuantifiers(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		assertEquals(2, translateFunctionsResult.second.size());
		Assert.assertTrue(translateFunctionsResult.second.contains(lowParser.parse("randomVariable('there exists X : president(X, Country)', 1, Nation, Boolean)")));

		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(friends, 2, Person, Person, Boolean)"));
		rules.clear();
		rules.add(this.ruleParser.parse("friends(X,Y) and (there exists Z : friends(X,Z));"));
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("friends(X,Y) and 'there exists Z : friends(X, Z)'(X);"));
		expectedRules.add(ruleParser.parse("if friends(X,Z) then 'there exists Z : friends(X, Z)'(X) else 'there exists Z : friends(X, Z)'(X) 0.000000001;"));
		translateFunctionsResult = ruleConverter.translateQuantifiers(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		assertEquals(2, translateFunctionsResult.second.size());
		Assert.assertTrue(translateFunctionsResult.second.contains(lowParser.parse("randomVariable('there exists Z : friends(X, Z)', 1, Person, Boolean)")));


		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(friends, 2, Person, Person, Boolean)"));
		randomVariableDefinitions.add(lowParser.parse("randomVariable(loves, 2, Person, Person, Boolean)"));
		rules.clear();
		rules.add(this.ruleParser.parse("friends(X,Y) and (there exists Z : Z may be same as X and loves(X,Z));"));
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("friends(X,Y) and 'there exists Z : \\\'. may be same as .\\\'(Z, X) and loves(X, Z)'(X);"));
		expectedRules.add(ruleParser.parse("if Z may be same as X and loves(X,Z) then 'there exists Z : \\\'. may be same as .\\\'(Z, X) and loves(X, Z)'(X) else 'there exists Z : \\\'. may be same as .\\\'(Z, X) and loves(X, Z)'(X) 0.000000001;"));
		translateFunctionsResult = ruleConverter.translateQuantifiers(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		assertEquals(3, translateFunctionsResult.second.size());
		Assert.assertTrue(translateFunctionsResult.second.contains(lowParser.parse("randomVariable('there exists Z : \\\'. may be same as .\\\'(Z, X) and loves(X, Z)', 1, Person, Boolean)")));


		// Test nested quantifiers.
		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(lowParser.parse("randomVariable(loves, 3, Person, Amount, Object, Boolean)"));
		rules.clear();
		rules.add(this.ruleParser.parse("there exists Z : for all Y : loves(X, Y, Z);"));
		expectedRules.clear();

		// TODO: the results of this test flip between the two following alternatives (the difference is in the parentheses around the for all Y), so parentheses generation is not deterministic and this needs to be fixed.

//		expectedRules.add(ruleParser.parse("'there exists Z : (for all Y : loves(X, Y, Z))'(X);"));
//		expectedRules.add(ruleParser.parse("if 'for all Y : loves(X, Y, Z)'(X, Z) then 'there exists Z : (for all Y : loves(X, Y, Z))'(X) else 'there exists Z : (for all Y : loves(X, Y, Z))'(X) 0.000000001;"));
//		expectedRules.add(ruleParser.parse("if not loves(X, Y, Z) then not 'for all Y : loves(X, Y, Z)'(X, Z) else 'for all Y : loves(X, Y, Z)'(X, Z) 0.999999999;"));

		expectedRules.add(ruleParser.parse("'there exists Z : for all Y : loves(X, Y, Z)'(X);"));
		expectedRules.add(ruleParser.parse("if 'for all Y : loves(X, Y, Z)'(X, Z) then 'there exists Z : for all Y : loves(X, Y, Z)'(X) else 'there exists Z : for all Y : loves(X, Y, Z)'(X) 0.000000001;"));
		expectedRules.add(ruleParser.parse("if not loves(X, Y, Z) then not 'for all Y : loves(X, Y, Z)'(X, Z) else 'for all Y : loves(X, Y, Z)'(X, Z) 0.999999999;"));

		translateFunctionsResult = ruleConverter.translateQuantifiers(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
		assertEquals(3, translateFunctionsResult.second.size());
		Assert.assertTrue(translateFunctionsResult.second.contains(lowParser.parse("randomVariable('for all Y : loves(X, Y, Z)', 2, Person, Object, Boolean)")));
		Assert.assertTrue(translateFunctionsResult.second.contains(lowParser.parse("randomVariable('there exists Z : for all Y : loves(X, Y, Z)', 1, Person, Boolean)")));

		doTreeUtilWaitUnilClosed(); 
	}

	@Test
	public void testTranslateAtomicRules () {

		// string = "sick(X);";
		testRule2PotentialExpression(ruleParser.parse("sick(X);"), 
				lowParser.parse("if sick(X) then 1 else 0"));
		
		testRule2PotentialExpression(
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1", "0"));

		// string = "sick(X) 0.3;";
		testRule2PotentialExpression(ruleParser.parse("sick(X) 0.3;"), 
				lowParser.parse("if sick(X) then 0.3 else 0.7"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.3"),
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.3", "0.7"));

		// string = "sick(X) and happy(X);";
		testRule2PotentialExpression(ruleParser.parse("sick(X) and happy(X);"), 
				lowParser.parse("if sick(X) and happy(X) then 1 else 0"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("happy", "X")), "1"), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("happy", "X")), "1", "0"));

		// string = "sick(X) and happy(X) 0.1;";
		testRule2PotentialExpression(ruleParser.parse("sick(X) and happy(X) 0.1;"), 
				lowParser.parse("if sick(X) and happy(X) then 0.1 else 0.9"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("happy", "X")), "0.1"),
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("happy", "X")), "0.1", "0.9"));
	}
	
	@Test
	public void testTranslateConditionalRules() {
		// Conditional rule tests
		// string = "if circle(X) then round(X);";
		testRule2PotentialExpression(ruleParser.parse("if circle(X) then round(X);"), 
				lowParser.parse("if circle(X) then if round(X) then 1 else 0 else 0.5"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("circle", "X"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("round", "X"), "1")), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("circle", "X"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("round", "X"), "1", "0"), "0.5"));

		// string = "if epidemic then sick(X) 0.7;";
		testRule2PotentialExpression(ruleParser.parse("if epidemic then sick(X) 0.7;"), 
				lowParser.parse("if epidemic then if sick(X) then 0.7 else 0.3 else 0.5"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.7")), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "epidemic", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.7", "0.3"), "0.5"));

		// string = "if epidemic then sick(X) and unhappy(X) 0.9;";
		testRule2PotentialExpression(ruleParser.parse("if epidemic then sick(X) and unhappy(X) 0.9;"), 
				lowParser.parse("if epidemic then if sick(X) and unhappy(X) then 0.9 else 0.1 else 0.5"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("unhappy", "X")), "0.9")), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "epidemic", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("unhappy", "X")), "0.9", "0.1"), "0.5"));

		// string = "if chilly(P) and live(X, P) then sick(X) 0.6;";
		testRule2PotentialExpression(ruleParser.parse("if chilly(P) and live(X, P) then sick(X) 0.6;"), 
				lowParser.parse("if chilly(P) and live(X, P) then if sick(X) then 0.6 else 0.4 else 0.5"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("chilly", "P"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("live", "X", "P")), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.6")), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("chilly", "P"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("live", "X", "P")), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.6", "0.4"), "0.5"));

		// string = "if colleagues(X,Y) and Y != bob then likes(X,Y) 0.8;";	
		testRule2PotentialExpression(ruleParser.parse("if colleagues(X,Y) and Y != bob then likes(X,Y) 0.8;"), 
				lowParser.parse("if colleagues(X,Y) and Y != bob then if likes(X,Y) then 0.8 else 0.2 else 0.5"));
		
		// string = "if colleagues(X,Y) then likes(X,Y) 0.8;";
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("colleagues", "X", "Y"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("likes", "X", "Y"), "0.8")), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("colleagues", "X", "Y"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("likes", "X", "Y"), "0.8", "0.2"), "0.5"));

		// string = "if epidemic then if sick(X) and friends(X,Y) then sick(Y) 0.8;";
		testRule2PotentialExpression(ruleParser.parse("if epidemic then if sick(X) and friends(X,Y) then sick(Y) 0.8;"), 
				lowParser.parse("if epidemic then if sick(X) and friends(X, Y) then if sick(Y) then 0.8 else 0.2 else 0.5 else 0.5"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", "epidemic", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.8"))), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "epidemic", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.8", "0.2"), "0.5"), "0.5"));

		// string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y);";
		testRule2PotentialExpression(ruleParser.parse("if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y);"), 
				lowParser.parse("if sick(X) and friends(X,Y) then if sick(Y) then 0.8 else 0.2 else if sick(Y) then 1 else 0"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.8"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), 1)), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.8", "0.2"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "1", "0")));

		// string = "if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y) 0.3;";
		testRule2PotentialExpression(ruleParser.parse("if sick(X) and friends(X,Y) then sick(Y) 0.8 else sick(Y) 0.3;"), 
				lowParser.parse("if sick(X) and friends(X,Y) then if sick(Y) then 0.8 else 0.2 else if sick(Y) then 0.3 else 0.7"));

		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.8"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.3")), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.8", "0.2"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "0.3", "0.7")));
			
			testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("conditional rule", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
					"0.5", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("atomic rule", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), 1)), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("friends", "X", "Y")), 
					"0.5", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "Y"), "1", "0")));	
			
			//
			// Conditional Rules with potential expressions (i.e. arithmetic expressions) on the branches are now allowed
			testRule2PotentialExpression(ruleParser.parse("if a then 0.3;"), 
					lowParser.parse("if a then 0.3 else 0.5"));
			
			testRule2PotentialExpression(ruleParser.parse("if a then 0.4 else 0;"), 
					lowParser.parse("if a then 0.4 else 0"));
			
			testRule2PotentialExpression(ruleParser.parse("if a then 0.2 else if b then 0.3 else if c then 0.5 else 0;"), 
					lowParser.parse("if a then 0.2 else if b then 0.3 else if c then 0.5 else 0"));
	}
	
	@Test
	public void testTranslatePrologRules () {
// TODO - can we not just get away with a trailing period instead of '.;' for prolog rules?
// http://code.google.com/p/aic-praise/issues/detail?id=18
		
		// Prolog rule tests		
		testRule2PotentialExpression(ruleParser.parse("sick(john)."), 
				lowParser.parse("if sick(john) then 1 else 0"));

		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "1", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "john")),
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "john"), "1", "0"));

		// string = "sick(X).";
		testRule2PotentialExpression(ruleParser.parse("sick(X)."), 
				lowParser.parse("if sick(X) then 1 else 0"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "1", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X")), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "1", "0"));

		// string = "not sick(mary).";
		testRule2PotentialExpression(ruleParser.parse("not sick(mary)."), 
				lowParser.parse("if not sick(mary) then 1 else 0"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "1", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "mary"))), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "mary")), "1", "0"));

		// string = "0.3 sick(X).";
		testRule2PotentialExpression(ruleParser.parse("0.3 sick(X)."), 
				lowParser.parse("if sick(X) then 0.3 else 0.7"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "0.3", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X")), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.3", "0.7"));

		// string = "round(X) :- circle(X).";
		testRule2PotentialExpression(ruleParser.parse("round(X) :- circle(X)."), 
				lowParser.parse("if circle(X) then if round(X) then 1 else 0 else 0.5"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "1", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("round", "X"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("circle", "X")), 
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("circle", "X"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("round", "X"), "1", "0"), "0.5"));

		// string = "0.7 sick(X) :- epidemic and not vaccinated(X).";
		testRule2PotentialExpression(ruleParser.parse("0.7 sick(X) :- epidemic and not vaccinated(X)."), 
				lowParser.parse("if epidemic and not vaccinated(X) then if sick(X) then 0.7 else 0.3 else 0.5"));
		
		testRule2PotentialExpression(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("prolog rule", "0.7", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "epidemic", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("vaccinated", "X")))),
			Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "epidemic", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("vaccinated", "X"))), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sick", "X"), "0.7", "0.3"), "0.5"));
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
		testRule2PotentialExpression(ruleParser.parse(string), 
				lowParser.parse("if epidemic then if sick(X) then 0.8 else 0.2 else 0.5"));

		string = "P(sick(X) and happy(Y) | mother(Z)) = 0.4;";
		testRule2PotentialExpression(ruleParser.parse(string), 
				lowParser.parse("if mother(Z) then if sick(X) and happy(Y) then 0.4 else 0.6 else 0.5"));
	}
	
	@Test
	public void testTranslateCausalEffectStyleRules() {
		// Causal rule tests.
		String string = "sick(X) -> fever(X) 0.6;";
		testRule2PotentialExpression(ruleParser.parse(string), 
				lowParser.parse("if sick(X) then (if fever(X) then 0.6 else 0.4) else 0.5"));

		string = "sick(X) and happy(Y) -> fever(X) 0.6;";
		testRule2PotentialExpression(ruleParser.parse(string), 
				lowParser.parse("if sick(X) and happy(Y) then (if fever(X) then 0.6 else 0.4) else 0.5"));
	}
		
	@Test
	public void testUpdatingRandomVariableDeclaration () {
		Expression result, input;
		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "mother", 1, "People", "People");
		result = ruleConverter.updateRandomVariableDeclaration(input);
		assertEquals(lowParser.parse("randomVariable(mother, 2, People, People, Boolean)"), result);

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "president", 0, "People");
		result = ruleConverter.updateRandomVariableDeclaration(input);
		assertEquals(lowParser.parse("randomVariable(president, 1, People, Boolean)"), result);

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sort", "sprinters", "bolt", "johnson");
		result = ruleConverter.updateRandomVariableDeclaration(input);
		assertEquals(null, result);
		
		// Note: ensure non-Boolean return_type is not used as a marker in the logic (had been).
		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("randomVariable", "gate", 1, "Boolean", "Boolean");
		result = ruleConverter.updateRandomVariableDeclaration(input);
		assertEquals(lowParser.parse("randomVariable(gate, 2, Boolean, Boolean, Boolean)"), result);
	}
	
	// NOTE: Currently expected as we have disabled support for rules with functions.
	@Test(expected=UnsupportedOperationException.class)
	public void testDifferentiatingNullaryRandomFunctionsAndConstants() {
		List<Expression> rules                     = new ArrayList<Expression>();
		Set<Expression>  randomVariableDefinitions = new LinkedHashSet<Expression>();
		Set<Expression>  expectedRules             = new LinkedHashSet<Expression>();
		
		Pair<List<Expression>, Set<Expression>> translateFunctionsResult;
		
		// Test of zero arg function.
		rules.clear();
		rules.add(ruleParser.parse("if president = obama then government(socialism) else government(freedom);"));
		randomVariableDefinitions.clear();
		randomVariableDefinitions.add(ruleParser.parse("random president: Boolean;"));
		expectedRules.clear();
		expectedRules.add(ruleParser.parse("if president(obama) then government(socialism) else government(freedom);"));
		expectedRules.add(ruleParser.parse("if president(Y) then not president(Z);"));
		expectedRules.add(ruleParser.parse("there exists Y : president(Y);"));
		translateFunctionsResult = ruleConverter.translateFunctions(rules, randomVariableDefinitions, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		assertEquals(expectedRules, new LinkedHashSet<Expression>(translateFunctionsResult.first));
	}
	
	@Test
	public void testDisembedConstraints () {
		List<Expression> potentialExpressions;
		List<Pair<Expression, Expression>> expected;
		
		// Required in advance of callling disembedConstraints.
		Set<Expression> randomVariableDeclarations = new LinkedHashSet<Expression>();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(friends, 2, Universe, Universe, Boolean)"));
		randomVariableDeclarations.add(lowParser.parse("randomVariable(likes, 2, Universe, Universe, Boolean)"));
		randomVariableDeclarations.add(lowParser.parse("randomVariable(colleagues, 2, Universe, Universe, Boolean)"));
 
		potentialExpressions = new ArrayList<Expression>();
		potentialExpressions.add(lowParser.parse(
				"if friends(X,Y) and likes(X,Z) and '. may be same as .'(X, Z) then if likes(Y,Z) then 0.8 else 0.2 else 0.5"));
		expected = new ArrayList<Pair<Expression, Expression>>();
		expected.add(new Pair<Expression, Expression>(
				lowParser.parse("if friends(X, Y) and likes(X, Z) then if likes(Y, Z) then 0.8 else 0.2 else 0.5"),
				lowParser.parse("X != Y and Y != Z")));
		assertEquals(expected, ruleConverter.disembedConstraints(potentialExpressions, getNewRewritingProcessWithDefaultTypeSizeAndRandomVariableDeclarations(DEFAULT_DOMAIN_SIZE, randomVariableDeclarations)));

		potentialExpressions = new ArrayList<Expression>();
		potentialExpressions.add(lowParser.parse(
				"if colleagues(X,Y) and Y != bob then (if likes(X,Y) then 0.8 else 0.2) else 0.5"));
		expected = new ArrayList<Pair<Expression, Expression>>();
		expected.add(new Pair<Expression, Expression>(
				lowParser.parse("if colleagues(X, Y) then if likes(X, Y) then 0.8 else 0.2 else 0.5"),
				lowParser.parse("Y != X and Y != bob")));
		assertEquals(expected, ruleConverter.disembedConstraints(potentialExpressions, getNewRewritingProcessWithDefaultTypeSizeAndRandomVariableDeclarations(DEFAULT_DOMAIN_SIZE, randomVariableDeclarations)));
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
	}
	
	// Tests For:
	// http://code.google.com/p/aic-praise/wiki/TranslatingInferenceInputAndOutput
	//

	@Test
	public void testEncodingQueries() throws QueryContainsUnknownRandomVariablesException {
		String string;
		Set<Expression> randomVariableDeclarations = new LinkedHashSet<Expression>(); 
		Pair<Expression, Expression> result, expected;
		
		string = "sick(X)";
		randomVariableDeclarations.clear();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(sick, 1, Boolean, Boolean)"));
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		Assert.assertNull(result);
		
		string = "sick(john) and sick(mary)";
		randomVariableDeclarations.clear();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(sick, 1, Boolean, Boolean)"));
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = new Pair<Expression, Expression>(ruleParser.parse("query <=> sick(john) and sick(mary);"),
				                                    ruleParser.parseFormula("query"));
		assertEquals(expected, result);

		string = "not sick(X)";
		randomVariableDeclarations.clear();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(sick, 1, Boolean, Boolean)"));
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = new Pair<Expression, Expression>(ruleParser.parse("query(X) <=> not sick(X);"),
				                                    ruleParser.parseFormula("query(X)"));
		assertEquals(expected, result);

		string = "there exists X : friends(X,Y)";
		randomVariableDeclarations.clear();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(friends, 2, Boolean, Boolean, Boolean)"));
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = new Pair<Expression, Expression>(ruleParser.parse("query(Y) <=> there exists X : friends(X,Y);"),
				                                    ruleParser.parseFormula("query(Y)"));
		assertEquals(expected, result);

		string = "conspiracy(C) and leader(C) = X and member(C,Y) and member(C,Z)";
		randomVariableDeclarations.clear();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(conspiracy, 1, Boolean, Boolean)"));
		randomVariableDeclarations.add(lowParser.parse("randomVariable(leader, 1, Boolean, Boolean)"));
		randomVariableDeclarations.add(lowParser.parse("randomVariable(member, 2, Boolean, Boolean, Boolean)"));
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = new Pair<Expression, Expression>(ruleParser.parse("query(C, X, Y, Z) <=> conspiracy(C) and leader(C) = X and member(C,Y) and member(C,Z);"),
				                                    ruleParser.parseFormula("query(C, X, Y, Z)"));
		assertEquals(expected, result);

		string = "mother(X) = lucy";
		randomVariableDeclarations.clear();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(mother, 1, Boolean, Boolean)"));
		randomVariableDeclarations.add(lowParser.parse("randomVariable(lucy, 0, Boolean)"));
		result = ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = new Pair<Expression, Expression>(ruleParser.parse("query(X) <=> (mother(X) = lucy);"),
				                                    ruleParser.parseFormula("query(X)"));
		assertEquals(expected, result);
	}
	
	@Test
	public void testQueryContainsUnknownRandomVariablesException() {
		String string;
		Set<Expression> randomVariableDeclarations = new LinkedHashSet<Expression>(); 
		
		string = "epidemic";
		randomVariableDeclarations.clear();
		try {
			ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
			Assert.fail("QueryContainsUnknownRandomVariablesException should have been thrown");
		}
		catch (QueryContainsUnknownRandomVariablesException e) {
			// Expected
		}
		
		string = "sicko(X)";
		randomVariableDeclarations.clear();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(sick, 1, Boolean, Boolean)"));
		try {
			ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
			Assert.fail("QueryContainsUnknownRandomVariablesException should have been thrown");
		}
		catch (QueryContainsUnknownRandomVariablesException e) {
			// Expected
		}
		
		string = "sick(X) and epidemic";
		randomVariableDeclarations.clear();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(sick, 1, Boolean, Boolean)"));
		try {
			ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
			Assert.fail("QueryContainsUnknownRandomVariablesException should have been thrown");
		}
		catch (QueryContainsUnknownRandomVariablesException e) {
			// Expected
		}
		
		string = "sick(john) and john";
		randomVariableDeclarations.clear();
		randomVariableDeclarations.add(lowParser.parse("randomVariable(sick, 1, Boolean, Boolean)"));
		try {
			ruleConverter.queryRuleAndAtom(ruleParser.parseFormula(string), randomVariableDeclarations, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
			Assert.fail("QueryContainsUnknownRandomVariablesException should have been thrown");
		}
		catch (QueryContainsUnknownRandomVariablesException e) {
			// Expected
		}
	}
	
	@Test
	public void testTransformingQueryOutputsIntoHighLevelSyntax() {
		Expression input, result, expected;

		input = lowParser.parse("if sick(bob) then 0.3 else 0.7");
		result = ruleConverter.queryAnswerPotentialExpression2Rule(
				input, lowParser.parse("sick(bob)"),
				getNewRewritingProcessWithDefaultTypeSizeAndContextualSymbolsEqualToFreeVariablesInGivenExpression(DEFAULT_DOMAIN_SIZE, input));
		expected = ruleParser.parse("sick(bob) 0.3;");
		assertEquals(expected, result);

		input = lowParser.parse("if X = bob then if sick(bob) then 0.7 else 0.3 else if sick(X) then 0.2 else 0.8");
		result = ruleConverter.queryAnswerPotentialExpression2Rule(
				input, lowParser.parse("sick(bob)"),
				getNewRewritingProcessWithDefaultTypeSizeAndContextualSymbolsEqualToFreeVariablesInGivenExpression(DEFAULT_DOMAIN_SIZE, input));
		expected = ruleParser.parse("if X = bob then sick(bob) 0.7 else sick(X) 0.2;");
		assertEquals(expected, result);

		input = lowParser.parse("0.8");
		result = ruleConverter.queryAnswerPotentialExpression2Rule(
				input, lowParser.parse("sick(bob)"), 
				getNewRewritingProcessWithDefaultTypeSizeAndContextualSymbolsEqualToFreeVariablesInGivenExpression(DEFAULT_DOMAIN_SIZE, input));
		expected = ruleParser.parse("sick(bob) 0.5;");
		assertEquals(expected, result);
	}
	
	//
	// Tests for additional issues that have been encountered
	//
	// NOTE: Currently expected as we have disabled support for rules with functions.
	@Test(expected=UnsupportedOperationException.class)
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
		RuleConverter.LowLevelModelParts lowLevelSyntax = ruleConverter.translate(inputRules, LBPFactory.newLBPProcess(Expressions.TRUE));	
		Assert.assertTrue(lowLevelSyntax.getParfactors().contains(lowParser.parse("{{ (on) [if entityOf(m1, obama) then 1 else 0] }}")));
		
		// Additional case with explicit assignment for just 1 of the terms
		inputRules.clear();
		inputRules.add(ruleParser.parse("entityOf(X0) = obama;"));
		lowLevelSyntax = ruleConverter.translate(inputRules, LBPFactory.newLBPProcess(Expressions.TRUE));
		Assert.assertTrue(lowLevelSyntax.getParfactors().contains(lowParser.parse("{{ (on X0) [if entityOf(X0, obama) then 1 else 0] }}")));
		
		inputRules.clear();
		inputRules.add(ruleParser.parse("entityOf(X) = Y;"));
		lowLevelSyntax = ruleConverter.translate(inputRules, LBPFactory.newLBPProcess(Expressions.TRUE));
		Assert.assertTrue(lowLevelSyntax.getParfactors().contains(lowParser.parse("{{ ( on X, Y ) ([ if entityOf(X, Y) then 1 else 0 ]) | X != Y }}")));
		
		inputRules.clear();
		inputRules.add(ruleParser.parse("entityOf(X) = Y and X may be same as Y;"));
		lowLevelSyntax = ruleConverter.translate(inputRules, LBPFactory.newLBPProcess(Expressions.TRUE));
		Assert.assertTrue(lowLevelSyntax.getParfactors().contains(lowParser.parse("{{ ( on X, Y ) ([ if entityOf(X, Y) then 1 else 0 ]) }}")));
	}
	
	//
	// Test Supporting Routines
	//	
	@Test
	public void testCreateTransformedFunctionConstraints () {
		List<Expression> expected;

		List<Expression> rules;
		rules = ruleConverter.createTransformedFunctionConstraints("president", 0);
		expected = new ArrayList<Expression>();
		expected.add(ruleParser.parse("if president(Y) then not president(Z);"));
		expected.add(ruleParser.parse("there exists Y : president(Y);"));
		assertEquals(expected, rules);

		rules = ruleConverter.createTransformedFunctionConstraints("mother", 1);
		expected = new ArrayList<Expression>();
		expected.add(ruleParser.parse("if mother(X0, Y) then not mother(X0, Z) and Y may be same as X0 and Z may be same as X0;"));
		expected.add(ruleParser.parse("there exists Y : mother(X0, Y) and Y may be same as X0;"));
		assertEquals(expected, rules);

		rules = ruleConverter.createTransformedFunctionConstraints("foo", 2);
		expected = new ArrayList<Expression>();
		expected.add(ruleParser.parse("if foo(X0, X1, Y) then not foo(X0, X1, Z) and Y may be same as X0 and Z may be same as X0 and Y may be same as X1 and Z may be same as X1;"));
		expected.add(ruleParser.parse("there exists Y : foo(X0, X1, Y) and Y may be same as X0 and Y may be same as X1;"));
		assertEquals(expected, rules);
	}
		
	@Test
	public void testIsRandomFunctionApplication () {
		Expression input;
		input = IfThenElse.make(Expressions.ONE, Expressions.TWO, Expressions.THREE);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeSymbol("foo");
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees ("and", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees ("or", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees ("not", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees ("<=>", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees ("=>", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees ("there exists . : .", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees ("for all . : .", 1, 2, 3);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees (". may be same as .", "A", "B");
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("mother", 1, 2, 3, 4);
		Assert.assertEquals(true, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));
		
		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));
		
		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));
		
		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));
		
		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

		input = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 1, 2, 3, 4);
		Assert.assertEquals(false, ruleConverter.isRandomFunctionApplication(input));

	}

	@Test
	public void testCreateQueryDeclaration () {
		Expression queryAtom, query, result, expected;
		Set<Expression> randomVariables = new LinkedHashSet<Expression>();

		query = ruleParser.parseFormula("sick(john) and sick(mary)");
		queryAtom = lowParser.parse("query");
		randomVariables.clear();
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("random query: Boolean;");
		assertEquals(expected, result);
		
		query = ruleParser.parseFormula("not sick(X)");
		queryAtom = lowParser.parse("query(X)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random sick: People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("random query: People -> Boolean;");
		assertEquals(expected, result);

		query = ruleParser.parseFormula("there exists X : friends(X,Y)");
		queryAtom = lowParser.parse("query(Y)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random friends: People x People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("random query: People -> Boolean;");
		assertEquals(expected, result);

		query = ruleParser.parseFormula("conspiracy(C) and leader(C) = X and member(C,Y) and member(C,Z)");
		queryAtom = lowParser.parse("query(C, Y, X, Z)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random conspiracy: Concept -> Boolean;" +
				"random leader: Concept x People -> Boolean;" +
				"random member: Concept x People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("random query: Concept x People x People x People -> Boolean;");
		assertEquals(expected, result);

		query = ruleParser.parseFormula("conspiracy(C) and leader = X and member(C,Y) and member(C,Z)");
		queryAtom = lowParser.parse("query(C, Y, X, Z)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random conspiracy: Concept -> Boolean;" +
				"random leader: People -> Boolean;" +
				"random member: Concept x People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("random query: Concept x People x People x People -> Boolean;");
		assertEquals(expected, result);

		query = ruleParser.parseFormula("mother(X) = lucy");
		queryAtom = lowParser.parse("query(X)");
		randomVariables.clear();
		randomVariables.addAll(ruleParser.parseAll("random mother: People x People -> Boolean;"));
		result = ruleConverter.createQueryDeclaration(queryAtom, query, randomVariables, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("random query: People -> Boolean;");
		assertEquals(expected, result);
		
	}
	
	@Test
	public void testQueryResultToRule () {
		Expression input, queryAtom, query, result, expected;

		input = lowParser.parse("if query then 0.3 else 0.7");
		queryAtom = lowParser.parse("query");
		query = lowParser.parse("sick(bob)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("sick(bob) 0.3;");
		assertEquals(expected, result);

		input = lowParser.parse("if query(bob) then 0.3 else 0.7");
		queryAtom = lowParser.parse("query(X)");
		query = lowParser.parse("sick(X)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("sick(bob) 0.3;");
		assertEquals(expected, result);

		input = lowParser.parse("if query(bob, mary) then 0.3 else 0.7");
		queryAtom = lowParser.parse("query(X, Y)");
		query = lowParser.parse("sick(X) = cold(Y)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("sick(bob) = cold(mary) 0.3;");
		assertEquals(expected, result);

		input = lowParser.parse("if query(X = bob, Z) then 0.3 else 0.7");
		queryAtom = lowParser.parse("query(X, Y)");
		query = lowParser.parse("sick(X) = cold(Y)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
		expected = ruleParser.parse("sick(X = bob) = cold(Z) 0.3;");
		assertEquals(expected, result);

		// Test a case where there is overlap between the query args in the input
		// and the query atom's args.
		input = lowParser.parse("if query(Y, X) then 0.3 else 0.7");
		queryAtom = lowParser.parse("query(X, Y)");
		query = lowParser.parse("sick(X) = cold(Y)");
		result = ruleConverter.queryResultToRule(input, queryAtom, query, getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE));
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
		string = "sick(john).";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(john).";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "1 sick(john).";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(john).";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "1.0 sick(john).";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(john).";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "sick(X).";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "sick(X).";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "not sick(mary).";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "not sick(mary).";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "0.3 sick(X).";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "0.3 sick(X).";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "round(X) :- circle(X).";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "round(X) :- circle(X).";
		assertEquals(expected, result);
		assertEquals(inputExpression, outputExpression);

		string = "0.7 sick(X) :- epidemic and not vaccinated(X).";
		inputExpression = ruleParser.parse(string);
		result = ruleConverter.toRuleString(inputExpression);
		outputExpression = ruleParser.parse(result);
		expected = "0.7 sick(X) :- epidemic and not vaccinated(X).";
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
	public void testQuery() {
		String modelString, queryString;
		
		@SuppressWarnings("unused")
		Triple<Model, Expression, Model> result;

		modelString = "if mother(X) = Y then X != Y;" +
				"if trait(mother(X)) then trait(X) 0.8 else trait(X) 0.3;" +
				"there exists X : trait(X);" +
				"mother(bob)=mary;" +
				"mother(ann)=mary;" +
				"mother(john)=ann;" +
				"trait(john);";
		queryString = "trait(mary)";
		try {
			result = ruleConverter.query(queryString, modelString, "Test Model", "Description", LBPFactory.newLBPProcess(Expressions.TRUE));
		}
		catch (UnsupportedOperationException e) {
			// Currently expected as we have disabled support for rules with functions.
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (QueryContainsUnknownRandomVariablesException e) {
			e.printStackTrace();
			Assert.fail("Unexpected query contains unknown random variables exception thrown");
		}
		catch (ModelException e) {
			e.printStackTrace();
			Assert.fail("Errors in model string " + modelString + ": " + Util.join(e.getErrors()));
		}

		// Missing sort declaration for People.
		modelString =
				"sort People;" +
				"random president: People;" +
				"random firstLady: People;" +
				"president = barrackObama <=> firstLady = michelleObama;" +
				"president = billClinton <=> firstLady = hillaryClinton;" +
				"firstLady = michelleObama 0.9;";
		queryString = "president = X";
		try {
			result = ruleConverter.query(queryString, modelString, "Test Model", "Description", LBPFactory.newLBPProcess(Expressions.TRUE));
		}
		catch (UnsupportedOperationException e) {
			// Currently expected as we have disabled support for rules with functions.
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (QueryContainsUnknownRandomVariablesException e) {
			e.printStackTrace();
			Assert.fail("Unexpected query contains unknown random variables exception thrown");
		}
		catch (ModelException e) {
			e.printStackTrace();
			Assert.fail("Errors in model string " + modelString + ": " + Util.join(e.getErrors()));
		}

		modelString = "there exists X : X = bestFriend(X) 0.9;";
		queryString = "bestFriend(john)";
		try {
			result = ruleConverter.query(queryString, modelString, "Test Model", "Description", LBPFactory.newLBPProcess(Expressions.TRUE));		
		}
		catch (UnsupportedOperationException e) {
			// Currently expected as we have disabled support for rules with functions.
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (QueryContainsUnknownRandomVariablesException e) {
			e.printStackTrace();
			Assert.fail("Unexpected query contains unknown random variables exception thrown");
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
				"random epidemic: Boolean;\n"+
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
			result = ruleConverter.query(queryString, modelString, "Test Model", "Description", LBPFactory.newLBPProcess(Expressions.TRUE));
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (QueryContainsUnknownRandomVariablesException e) {
			e.printStackTrace();
			Assert.fail("Unexpected query contains unknown random variables exception thrown");
		}
		catch (ModelException e) {
			e.printStackTrace();
			Assert.fail("Errors in model string " + modelString + ": " + Util.join(e.getErrors()));
		}

		modelString = "// RANDOM VARIABLE DECLARATIONS:" +
				"random epidemic: Boolean;\n" +
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
			result = ruleConverter.query(queryString, modelString, "Test Model", "Description", LBPFactory.newLBPProcess(Expressions.TRUE));
		}
		catch (UnsupportedOperationException e) {
			// Currently expected as we have disabled support for rules with functions.
		}
		catch (ReservedWordException e) {
			e.printStackTrace();
			Assert.fail("Unexpected reserved word exception thrown");
		}
		catch (QueryContainsUnknownRandomVariablesException e) {
			e.printStackTrace();
			Assert.fail("Unexpected query contains unknown random variables exception thrown");
		}
		catch (ModelException e) {
			e.printStackTrace();
			Assert.fail("Errors in model string " + modelString + ": " + Util.join(e.getErrors()));
		}
	}
	
	// NOTE: Currently expected as we have disabled support for rules with functions.
	@Test(expected=UnsupportedOperationException.class)
	public void testRVequalRVTranslateException() {
		String modelString = "// RANDOM VARIABLE DECLARATIONS:\n" +
				"random earthquake: Boolean;\n" +
				"random burglary: Boolean;\n" +
				"earthquake 0.5;\n" +
				"burglary 0.5;\n" +
				"if earthquake = burglary then 1 else 0;";
		
		RuleConverter.makeModel("Test Model", "Description", modelString);
	}
	
	/*===================================================================================
	 * PRIVATE METHODS
	 *=================================================================================*/		
	private void testRule2PotentialExpression(Expression input, Expression expectedResult) {
		testRule2PotentialExpression(null, input, true, true, expectedResult);
	}

	private void testRule2PotentialExpression(String inputString, Expression inputExpr, boolean expectSucceed, boolean checkResult, Expression expectedResult) {
		Expression result;
		if (inputExpr == null) {
			result = ruleConverter.rule2PotentialExpression(ruleParser.parse(inputString));
		} 
		else {
			result = ruleConverter.rule2PotentialExpression(inputExpr);
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
	
	private static RewritingProcess getNewRewritingProcessWithDefaultTypeSize(final int n) {
		RewritingProcess result = LBPFactory.newLBPProcess(Expressions.TRUE);
		CardinalityOfType.registerTypeSizeOfSymbolOrTypeWithProcess(new CardinalityOfType.TypeSizeOfSymbolOrType() {
			@Override
			public Integer getSize(Expression logicalVariable, RewritingProcess process) {
				return n;
			}
		}, result);
		return result;
	}
	
	private static RewritingProcess getNewRewritingProcessWithDefaultTypeSizeAndRandomVariableDeclarations(final int n, Set<Expression> randomVariableDeclarations) {
		RewritingProcess process = getNewRewritingProcessWithDefaultTypeSize(DEFAULT_DOMAIN_SIZE);
		process = Model.setKnownSortsAndRandomVariables(Collections.<Expression>emptySet(), randomVariableDeclarations, process);
		return process;
	}
	
	/** Prepares rewriting process with type cardinalities contextual symbols, assuming contextual constraint equal to "true" */
	private static RewritingProcess getNewRewritingProcessWithDefaultTypeSizeAndContextualSymbolsEqualToFreeVariablesInGivenExpression(final int n, Expression expression) {
		RewritingProcess result = LBPFactory.newLBPProcess(Expressions.TRUE);
		CardinalityOfType.registerTypeSizeOfSymbolOrTypeWithProcess(new CardinalityOfType.TypeSizeOfSymbolOrType() {
			@Override
			public Integer getSize(Expression logicalVariable, RewritingProcess process) {
				return n;
			}
		}, result);
		result = GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(expression, result);
		// the above would have to include the contextual constraint (say, in a Tuple with expression), if it were not known to be "true".
		return result;
	}
}
