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
package com.sri.ai.test.praise.lbp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Marker;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.PickSingleElement;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElseExternalizationHierarchical;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.BreakConditionsContainingBothLogicalAndRandomVariablesHierarchical;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.MoveAllRandomVariableValueExpressionConditionsDownHierarchical;
import com.sri.ai.praise.PRAiSEConfiguration;
import com.sri.ai.praise.Solver;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPQueryEngine;
import com.sri.ai.praise.lbp.LBPQueryEngine.QueryStep;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.lbp.LiftedBeliefPropagationSolver;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.example.EmptyPQ;
import com.sri.ai.praise.model.example.IntensionalFanIn;
import com.sri.ai.praise.model.example.TrivialEpidemicAndSickNotbob;
import com.sri.ai.praise.model.example.TrivialEpidemicSickEveryone;
import com.sri.ai.praise.model.example.TrivialEpidemicSickEveryoneNotbobAmaryAjohn;
import com.sri.ai.praise.model.example.TrivialEpidemicSickbob;
import com.sri.ai.praise.model.example.TrivialGaveTreasureToOwnsRich;
import com.sri.ai.praise.model.example.TrivialLoopyFriendsAnnBobAndSmokerBobExample;
import com.sri.ai.praise.model.example.TrivialLoopyMisconceptionExample;
import com.sri.ai.praise.model.example.TrivialLoopyPQ;
import com.sri.ai.praise.model.example.TrivialLoopyPQWithPriors;
import com.sri.ai.praise.model.example.TrivialLoopyPQandb;
import com.sri.ai.praise.model.example.TrivialLoopyParfactorsExample;
import com.sri.ai.praise.model.example.TrivialPQ;
import com.sri.ai.praise.model.example.TrivialPQPeoplea1Anda2;
import com.sri.ai.praise.model.example.TrivialPQR;
import com.sri.ai.praise.model.example.TrivialPQRWithPriors;
import com.sri.ai.praise.model.example.TrivialPQWithPArity2AndQArity1;
import com.sri.ai.praise.model.example.TrivialPQWithPriors;
import com.sri.ai.praise.model.example.TrivialPRWithNonDeterministicFactor;
import com.sri.ai.praise.model.example.TrivialPeopleAmericanTallIntelligentUnintelligent;
import com.sri.ai.praise.model.example.TrivialSickSmokerbob;
import com.sri.ai.praise.model.example.TrivialSickbob;
import com.sri.ai.praise.model.example.TrivialSunnyAvailableCanPlayWith;
import com.sri.ai.praise.model.example.TrivialThereExistsPQWithPriors;
import com.sri.ai.praise.model.example.WeightedPQWithPriors;
import com.sri.ai.praise.model.example.WeightedThereExistsPQWithPriors;
import com.sri.ai.test.praise.AbstractLPITest;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * Consolidated TestCase for all of the rewriters involved in Lifted Belief Propagation (LBP).
 * 
 * <br>
 * Sources of Models for testing purposes:<br>
 * <br>
 * Bayesian Network Repository - http://www.cs.huji.ac.il/~galel/Repository/<br>
 * HUGIN Knowledge Base samples - http://www.hugin.com/developer/samples<br>
 * Norsys Net Library - http://www.norsys.com/net_library.htm<br>
 * GeNIe & SMILE - http://genie.sis.pitt.edu/networks.html<br>
 * 
 */
@SuppressWarnings("unused")
//@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class LBPTest extends AbstractLPITest {
	
	@Test
	public void testNewLBPProcess() {
		Expression testExpression = parse("1 + 1");
		RewritingProcess process = LBPFactory.newLBPProcess(testExpression);
		Expression result = process.rewrite(LBPRewriter.R_basic, testExpression);
		System.out.println(result);
	}
	
	@Test
	public void testBreakConditionsContainingBothLogicalAndRandomVariablesHierarchical() {
		String expressionString;
		String expectedString;
		Expression actual;
		Rewriter rewriter = new BreakConditionsContainingBothLogicalAndRandomVariablesHierarchical();
		RewritingProcess process =
				LBPFactory.newLBPProcessWithHighLevelModel(
						"sort Person : 10, ann, beth, carol, tom, john;" +
						"random smart : Person -> Boolean;");
		
		expressionString = "if X = john and smart(X) then 1 else 0";
		expectedString = "if X = john then if smart(X) then 1 else 0 else 0";
		actual = rewriter.rewrite(parse(expressionString), process);
		assertEquals(parse(expectedString), actual);
		
		expressionString = "if smart(X) and X = john and smart(X) then 1 else 0";
		expectedString = "if X = john then if smart(X) then 1 else 0 else 0";
		actual = rewriter.rewrite(parse(expressionString), process);
		assertEquals(parse(expectedString), actual);

		expressionString = "if X = john and smart(ann) then if X = john and smart(beth) then 1 else 0 else if X = john and smart(carol) and Y != tom then 1 else 0";
		expectedString = "if X = john then if smart(ann) then if X = john then if smart(beth) then 1 else 0 else 0 else (if X = john and Y != tom then if smart(carol) then 1 else 0 else 0) else (if X = john and Y != tom then if smart(carol) then 1 else 0 else 0)";
		actual = rewriter.rewrite(parse(expressionString), process);
		assertEquals(parse(expectedString), actual);
	}
	
	@Test
	public void testIfThenElseExternalizationHierarchical() {
		String expressionString;
		String expectedString;
		Expression actual;
		Rewriter rewriter = new IfThenElseExternalizationHierarchical();
		RewritingProcess process =
				LBPFactory.newLBPProcessWithHighLevelModel(
						"sort Person : 10, ann, beth, carol, tom, john;" +
						"random smart : Person -> Boolean;");
		
		// simple case
		expressionString = "(if X = tom then 1 else 2) + (if Y = beth then 3 else 4)";
		expectedString = "if X = tom then if Y = beth then 1 + 3 else 1 + 4 else (if Y = beth then 2 + 3 else 2 + 4)";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);
		
		// more-than-one-level case
		expressionString = "(if X = tom then 1 else 2) + (if Y = beth then if Z = carol then 3 else 4 else 5)";
		expectedString = "if X = tom then if Y = beth then if Z = carol then 1 + 3 else 1 + 4 else 1 + 5 else (if Y = beth then if Z = carol then 2 + 3 else 2 + 4 else 2 + 5)";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);
		
		// more-than-one-level case, with conditionals at the bottom only
		expressionString = "f(g((if X = tom then 1 else 2) + (if Y = beth then 3 else 4)))";
		expectedString = "if X = tom then if Y = beth then f(g(1 + 3)) else f(g(1 + 4)) else (if Y = beth then f(g(2 + 3)) else f(g(2 + 4)))";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);
		
		// conditional condition case
		expressionString = "(if X = tom then 1 else 2) + (if (if Y = beth then W = tom else W = john) then if Z = carol then 3 else 4 else 5)";
		expectedString = "if X = tom then if Y = beth then if W = tom then if Z = carol then 1 + 3 else 1 + 4 else 1 + 5 else (if W = john then if Z = carol then 1 + 3 else 1 + 4 else 1 + 5) else (if Y = beth then if W = tom then if Z = carol then 2 + 3 else 2 + 4 else 2 + 5 else (if W = john then if Z = carol then 2 + 3 else 2 + 4 else 2 + 5))";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);
		
		// double conditional condition case, with simplifications
		expressionString = "if (if X = tom then (if X = ann then X = john else X != john) else if X = beth then X = mark else X != mark) then 4 else 5";
		expectedString = "if X = tom or X != beth and X != mark then 4 else 5";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);
	}

	@Test
	public void testMoveAllRandomVariableValueExpressionConditionsDownHierarchical() {
		String expressionString;
		String expectedString;
		Expression actual;
		Rewriter rewriter = new MoveAllRandomVariableValueExpressionConditionsDownHierarchical();
		RewritingProcess process =
				LBPFactory.newLBPProcessWithHighLevelModel(
						"sort Person : 10, ann, beth, carol, tom, john;" +
						"random smart : Person -> Boolean;");

		// non-case
		expressionString = "1 + 2";
		expectedString = "1 + 2";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);

		// non-RV case
		expressionString = "if X = ann then 1 + 2 else 3";
		expectedString = "if X = ann then 1 + 2 else 3";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);

		// Already normalized case with nested LV conditions
		expressionString = "if X = ann then if X = bob then 1 else 2 else if X = bob then 3 else 4";
		expectedString   = "if X = ann then if X = bob then 1 else 2 else if X = bob then 3 else 4";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);

		// simple case
		expressionString = "if smart(X) then if X = tom then 1 else 2 else 3";
		expectedString = "if X = tom then if smart(X) then 1 else 3 else if smart(X) then 2 else 3";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);

		// simple case, symmetric case around then/else
		expressionString = "if smart(X) then 3 else if X = tom then 1 else 2";
		expectedString = "if X = tom then if smart(X) then 3 else 1 else if smart(X) then 3 else 2";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);

		// RVs on top and bottom
		expressionString = "if smart(X) then if X = tom then if smart(ann) then 1 else 2 else 3 else 4";
		expectedString = "if X = tom then if smart(X) then if smart(ann) then 1 else 2 else 4 else if smart(X) then 3 else 4";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);

		// RVs on top, LV at bottom
		expressionString = "if smart(X) then if X = tom then if Y = ann then 1 else 2 else 3 else 4";
		expectedString = "if X = tom then if Y = ann then if smart(X) then 1 else 4 else (if smart(X) then 2 else 4) else (if smart(X) then 3 else 4)";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);

		// RVs on top, LV at bottom, symmetric case around then/else
		expressionString = "if smart(X) then 4 else if X = tom then 3 else if Y = ann then 1 else 2";
		expectedString = "if X = tom then if smart(X) then 4 else 3 else (if Y = ann then if smart(X) then 4 else 1 else (if smart(X) then 4 else 2))";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);

		// case with unregistered RV. Should still be fine because it detects LV conditions based on formula definition, which is equality logic.
		expressionString = "if predicate(X) then if X = a then 1 else 2 else 3";
		expectedString = "if X = a then if predicate(X) then 1 else 3 else if predicate(X) then 2 else 3";
		runTestAfterExtendingContextualSymbols(expressionString, expectedString, rewriter, process);
	}
	
	/** Runs test assuming contextual constraint equal to "true" */
	private void runTestAfterExtendingContextualSymbols(String expressionString, String expectedString, Rewriter rewriter, RewritingProcess process) {
		Expression expression;
		Expression actual;
		RewritingProcess testProcess;
		expression = parse(expressionString);
		testProcess = GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(expression, process);
		// the above would have to include the contextual constraint (say, in a Tuple with expression), if it were not known to be "true".
		actual = rewriter.rewrite(expression, testProcess);
		assertEquals(parse(expectedString), actual);
	}

	@SuppressWarnings({ "unchecked" })
	@Test
	public void testFindRandomVariableValueExpressionsThatAreNotAGivenOne() {
		Expression expression;
		Expression randomVariableValue;
		RewritingProcess process;
		List<Pair<Expression,Expression>> expected;
		List<Pair<Expression,Expression>> otherRandomVariableValuesAndContexts;
		Model model;
		
        model = new Model(
                "union({{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}})",
                "epidemic/0", "sick/1"
        );
        expression = parse("if sick(X) and sick(Y) then if Z != X then (if epidemic and sick(Z) then 1 else 0) else (if sick(Z) and sick(W) and sick(X) then 1 else 0) else 0");
		randomVariableValue = parse("sick(X)");
		expected = Util.list(
				Util.pair(parse("sick(Y)"), parse("true")),
				Util.pair(parse("epidemic"), parse("Z != X")),
				Util.pair(parse("sick(Z)"), parse("Z != X")),
				Util.pair(parse("sick(W)"), parse("not (Z != X)"))
				);
        process = LBPFactory.newLBPProcess(expression);
        process = Model.setRewritingProcessesModel(parse(model.getModelDeclaration()), model.getKnownRandomVariableNameAndArities(), process);
        process = GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(Tuple.make(expression, randomVariableValue), process);
        // the above would have to include the contextual constraint (say, in a Tuple with expression), if it were not known to be "true".
        otherRandomVariableValuesAndContexts = LPIUtil.findRandomVariableValueExpressionsThatAreNotNecessarilyTheSameAsAGivenOne(expression, randomVariableValue, process);
        assertEquals(expected, otherRandomVariableValuesAndContexts);	
    }
	
	@Test
	public void testDifferenceOfExtensionalAndIntensionalSet() {
		
		class DifferenceOfExtensionalAndIntensionalSetTestData extends TestData {
			private String extA, intB; 
			private Expression exprA, exprB;
			
			public DifferenceOfExtensionalAndIntensionalSetTestData(String contextualConstraint, Model model, String extA, String intB,  boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.extA = extA;
				this.intB = intB;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprA = parse(extA);
				this.exprB = parse(intB);
				
				return Expressions.apply("-", exprA, exprB);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_DifferenceOfExtensionalAndIntensionalSet,
						LPIUtil.argForDifferenceOfExtensionalAndIntensionalSetRewriteCall(exprA, exprB, 0));
			}
		};

		TestData[] tests = new TestData[] {
				//
				// Basic: Empty Difference
				//
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{}", "{(on X) X | X = a or X = b}", 
						false, 
						"{ }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{}", "{{(on X) X | X = a or X = b}}", 
						false, 
						"{ }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{}}", "{(on X) X | X = a or X = b}", 
						false, 
						"{ }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{}}", "{{(on X) X | X = a or X = b}}", 
						false, 
						"{ }"),
				//
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a}", "{(on X) X | X = a }", 
						false, 
						"{ }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a}", "{{(on X) X | X = a }}", 
						false, 
						"{ }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a}}", "{(on X) X | X = a }", 
						false, 
						"{ }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a}}", "{{(on X) X | X = a }}", 
						false, 
						"{ }"),
				//
				// Basic: No Difference
				//
				//
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a}", "{(on X) X | X != a }", 
						false, 
						"{a}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a}", "{{(on X) X | X != a }}", 
						false, 
						"{a}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a}}", "{(on X) X | X != a }", 
						false, 
						"{{a}}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a}}", "{{(on X) X | X != a }}", 
						false, 
						"{{a}}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a}}", "{(on X) X | X != a }", 
						false, 
						"{{a, a}}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a}}", "{{(on X) X | X != a }}", 
						false, 
						"{{a, a}}"),
				//
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b}", "{(on X) X | X != a and X != b}", 
						false, 
						"{a, b}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b}", "{{(on X) X | X != a and X != b}}", 
						false, 
						"{a, b}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b}}", "{(on X) X | X != a and X != b}", 
						false, 
						"{{a, b}}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b}}", "{{(on X) X | X != a and X != b}}", 
						false, 
						"{{a, b}}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, b}}", "{(on X) X | X != a and X != b}", 
						false,
						"{{a, b, b}}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, b}}", "{{(on X) X | X != a and X != b}}", 
						false,
						"{{a, b, b}}"),
				//
				// Basic: Conditional
				//
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{Y, c}", "{(on X) X | X = a or X = b}", 
						false,
						"if Y = a or Y = b then {c} else { Y, c }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{Y, c}", "{{(on X) X | X = a or X = b}}", 
						false,
						"if Y = a or Y = b then {c} else { Y, c }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{Y, c}}", "{(on X) X | X = a or X = b}", 
						false,
						"if Y = a or Y = b then {{c}} else {{Y,c}}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{Y, c}}", "{{(on X) X | X = a or X = b}}", 
						false,
						"if Y = a or Y = b then {{c}} else {{Y,c}}"),
				//
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{Y, Y}", "{(on X) X | X = a or X = b}", 
						false,
						"if Y = a or Y = b then { } else { Y, Y }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{Y, Y}", "{{(on X) X | X = a or X = b}}", 
						false,
						"if Y = a or Y = b then { } else { Y, Y }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{Y, Y}}", "{(on X) X | X = a or X = b}", 
						false,
						"if Y = a or Y = b then { } else {{ Y, Y }}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{Y, Y}}", "{{(on X) X | X = a or X = b}}", 
						false,
						"if Y = a or Y = b then { } else {{ Y, Y }}"),
				//
				// Basic: Conditional - known true then known false
				//
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{b, c}", "{(on X) X | X = b or X != c}", 
						false,
						"{c}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{b, c}", "{{(on X) X | X = b or X != c}}", 
						false,
						"{c}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{b, c}}", "{(on X) X | X = b or X != c}", 
						false,
						"{{c}}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{b, c}}", "{{(on X) X | X = b or X != c}}", 
						false,
						"{{c}}"),
				//
				// Basic: Standardize Apart
				//
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{X, c}", "{(on X) X | X = a or X = b}", 
						false,
						"if X = a or X = b then {c} else { X, c }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{X, c}", "{{(on X) X | X = a or X = b}}", 
						false,
						"if X = a or X = b then {c} else { X, c }"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{X, c}}", "{(on X) X | X = a or X = b}", 
						false,
						"if X = a or X = b then {{c}} else {{X,c}}"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{X, c}}", "{{(on X) X | X = a or X = b}}", 
						false,
						"if X = a or X = b then {{c}} else {{X,c}}"),
				//
				// Basic: Illegal Argument Exceptions
				//
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X = a or X = b}}", "{{(on Y) Y | Y = c or Y = d}}", 
						true, 
						"N/A"),
				new DifferenceOfExtensionalAndIntensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b}", "{a}", 
						true, 
						"N/A"),
		};
		
		//perform(new TestData[] {
		//		new DifferenceOfExtensionalAndIntensionalSetTestData("{Y, c}", "{(on X) X | X = a or X = b}", "if Y = a or Y = b then {c} else if Y = c then { c } else { Y, c }"),
		//});
		
		perform(tests);
	}

	@Test
	public void testDifferenceOfExtensionalAndExtensionalSet() {
		class DifferenceOfExtensionalAndExtensionalSetTestData extends TestData {
			private String extA, extB; 
			private Expression exprA, exprB;
			
			public DifferenceOfExtensionalAndExtensionalSetTestData(String contextualConstraint, Model model, String extA, String extB,  boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model,  illegalArgumentTest, expected);
				this.extA = extA;
				this.extB = extB;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprA = parse(extA);
				this.exprB= parse(extB);
				
				return Expressions.apply("-", exprA, exprB);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_DifferenceOfExtensionalAndExtensionalSet,
							LPIUtil.argForDifferenceOfExtensionalAndExtensionalSetRewriteCall(exprA, exprB, 0, 0));
			}
		};

		TestData[] tests = new TestData[] {
				//
				// Empty difference
				//
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{}", "{}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a}", "{}", 
						false,
						"{a}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b}", "{}", 
						false,
						"{a, b}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{A}", "{}", 
						false,
						"{A}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{A, B}", "{}", 
						false,
						"{ A, B }"),
				//
				// Defined same order difference
				//
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a}", "{b}", 
						false,
						"{a}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b}", "{b}", 
						false,
						"{a}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{b}", 
						false,
						"{a, c}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{b, c}", 
						false,
						"{a}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b}", "{a, b}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{a, b, c}", 
						false,
						"{}"),
				//
				// Defined different orderings that should give {}
				//
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b}", "{b, a}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{a, c, b}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{b, a, c}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{b, c, a}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{c, a, b}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{c, b, a}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d}", "{a, b, d, c}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d}", "{a, c, b, d}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d}", "{d, c, b, a}", 
						false,
						"{}"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d}", "{c, d, a, b}", 
						false,
						"{}"),
				//
				// Multi-Set Differences that should give {{}}
				//
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b}}", "{{a, b}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b}}", "{{b, a}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c}}", "{{a, b, c}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c}}", "{{a, c, b}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c}}", "{{b, a, c}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c}}", "{{b, c, a}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c}}", "{{c, a, b}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c}}", "{{c, b, a}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c, d}}", "{{a, b, d, c}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c, d}}", "{{a, c, b, d}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c, d}}", "{{d, c, b, a}}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c, d}}", "{{c, d, a, b}}", 
						false,
						"{ }"), 
				//
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a}}", "{a}", 
						false,
						"{ }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b}}", "{a, b}", 
						false,
						"{ }"),
				//
				// Multi-Set Differences that should give remaining elements
				//
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a}}", "{{ a }}", 
						false,
						"{{ a }}"), 
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a, b, b, c, c, d, d}}", "{{c, d, a, b}}", 
						false,
						"{{ a, b, c, d }}"), 
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a, b, b, c, c, d, d}}", "{{b, c, a, d, c, a, b, c}}", 
						false,
						"{{ d }}"), 
				//
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a}}", "{ a }", 
						false,
						"{{ a }}"), 
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a, b, b, c, c, d, d}}", "{c, d, a, b}", 
						false,
						"{{ a, b, c, d }}"), 
				//
				//
				// Basic conditional expected
				//
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a}", "{B}", 
						false,
						"if B = a then { } else { a }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{A}", "{B}", 
						false,
						"if A = B then { } else { A }"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{A, B}", "{C}", 
						false,
						// Note: old R_basic result - has unreachable branch:
						// "if A = C then if A = B then { } else { B } else if B = C then { A } else if A = B then { A } else { A, B }"
						"if A = C then if B = C then { } else { B } else if B = C then { A } else { A, B }"),	
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{A, B, C}", "{D, C}", 
						false,
						// Note: old R_basic result - has unreachable branch:
						// "if A = D then if A = B then { } else if B = C then { } else { B } else if A = C then if B = D then { } else if A = B then { } else { B } else if B = D then { A } else if B = C then { A } else if A = B then { A } else { A, B }"
						"if A = D then if B = D then { } else if B = C then { } else { B } else if A = C then if B = D then { } else if B = C then { } else { B } else if B = D then { A } else if B = C then { A } else { A, B }"),
				//
				// Complex conditional expected
				//
			    new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
			    		"{A, B, if A = B then C else D}", 
			    		"{C}", 
			    		false,
			    		// Note: old R_basic result - has unreachable branches:
			    		// "if A = C then if A = B then { } else if A = D then { B } else if B = D then { B } else { B, D } else if B = C then if A = B then { A } else if B = D then { A } else if A = D then { A } else { A, D } else if A = B then { A } else if C = D then { A, B } else if A = D then { A, B } else if B = D then { A, B } else { A, B, D }"
			    		// Note: new R_normalize result also has unreachable branches (to be expected as known to be incomplete).
			    		// Note: introduction of FromConditionalFormulaToFormula shortens the expression further
			    		//       Used to be: if A = C then if B = C then if A = B then { } else if D = C then { } else { D } else if A = B then { B } else if D = C then { B } else { B, D } else if B = C then if A = B then { A } else if D = C then { A } else { A, D } else if A = B then { A, B } else if D = C then { A, B } else { A, B, D }
			    		"if A = C then if B = C then { } else (if A = B or D = C then { B } else { B, D }) else (if B = C then if A = B or D = C then { A } else { A, D } else (if A = B or D = C then { A, B } else { A, B, D }))"),
				//
				// Basic: Illegal Argument Exceptions
				//
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a}", "a", 
						true, 
						"N/A"),
				new DifferenceOfExtensionalAndExtensionalSetTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{a}", 
						true, 
						"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testIn() {
		class InTestData extends TestData {
			private String Alpha, Set; 
			private Expression exprAlpha, exprSet;
			
			public InTestData(String contextualConstraint, Model model, String Alpha, String Set, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.Alpha = Alpha;
				this.Set = Set;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprAlpha = parse(Alpha);
				this.exprSet = parse(Set);
				
				return Expressions.apply("In", exprAlpha, exprSet);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_in, LPIUtil.argForInRewriteCall(exprAlpha, exprSet));
			}
		};
		TestData[] tests = new TestData[] {
				//
				// Basic: Extensional Uniset In - false
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{}", 
						false, 
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{b}", 
						false, 
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{b, c}", 
						false, 
						"false"),	
				//
				// Basic: Extensional Multiset In - false
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{}}", 
						false, 
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{b}}", 
						false, 
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{b, c}}", 
						false, 
						"false"),
				//
				// Basic: Extensional Uniset In - true
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{a}", 
						false, 
						"true"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{a, b}", 
						false, 
						"true"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{b, a, c}", 
						false, 
						"true"),	
				//
				// Basic: Extensional Multiset In - true
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{a}}", 
						false, 
						"true"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{a, b}}", 
						false, 
						"true"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{b, a, c}}", 
						false, 
						"true"),
				//
				// Basic: Extensional Uniset In - condition
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{a}", 
						false, 
						"A = a"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{a, b}", 
						false, 
						"A = a or A = b"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{b, a, c}", 
						false, 
						"A = b or A = a or A = c"),
				//
				// Basic: Extensional Multiset In - condition
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{{a}}", 
						false, 
						"A = a"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{{a, b}}", 
						false, 
						"A = a or A = b"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{{b, a, c}}", 
						false, 
						"A = b or A = a or A = c"),
				// Ensure duplicates simplified out
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{{b, a, c, b, a, c}}", 
						false, 
						"A = b or A = a or A = c"),
				//
				// Basic: Intensional Uniset In - false
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{(on X) X | X != a }", 
						false, 
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{(on X) X | X = b }", 
						false, 
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{(on X) X | X = b or X = c }", 
						false, 
						"false"),
				//
				// Basic: Intensional Uniset In - true
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{(on X) X | X = a }", 
						false, 
						"true"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{(on X) X | X = b or X = a }", 
						false, 
						"true"),
				//
				// Basic: Intensional Multiset In - false
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{(on X) X | X != a }}", 
						false, 
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{(on X) X | X = b }}", 
						false, 
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{(on X) X | X = b or X = c }}", 
						false, 
						"false"),
				//
				// Basic: Intensional Multiiset In - true
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{(on X) X | X = a }}", 
						false, 
						"true"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "{{(on X) X | X = b or X = a }}", 
						false, 
						"true"),
				//
				// Basic: Intensional Uniset In - condition
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{(on X) X}", 
						false, 
						"true"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{(on X) X | X != b }", 
						false, 
						"A != b"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{(on X) X | X != b and X != c }", 
						false, 
						"A != c and A != b"),
				// Check Standardize Apart
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"X", "{(on X) X | X != b and X != c }", 
						false, 
						"X != c and X != b"),
				//
				// Basic: Intensional Multiset In - condition
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{{(on X) X}}", 
						false, 
						"true"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{{(on X) X | X != b }}", 
						false, 
						"A != b"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"A", "{{(on X) X | X != b and X != c }}", 
						false, 
						"A != c and A != b"),
				// Check Standardize Apart
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"X", "{{(on X) X | X != b and X != c }}", 
						false, 
						"X != c and X != b"),
				//
				// Basic: Externalize conditional - Alpha argument
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"if X = a then a else b", "{a}", 
						false,
						// Note: instead of 'if X = a then true else false' this
						// will be further simplified to:
						"X = a"),	
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"if X = a then b else a", "{a}", 
						false,
						"X != a"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"if X = a then if Y = b then a else b else if Y = c then b else c", "{a}", 
						false,
						"X = a and Y = b"),	
				//
				// Basic: Externalize conditional - Set argument
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "if X = a then {a} else {b}", 
						false,
						// Note: instead of 'if X = a then true else false' this
						// will be further simplified to:
						"X = a"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "if X = a then {b} else {a}", 
						false,
						"X != a"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "if X = a then if Y = b then {a} else {b} else if Y = c then {b} else {c}", 
						false,
						"X = a and Y = b"),
				//
				// Basic: Externalize conditional - Set contains conditional argument
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "union(if X = a then {a} else {b}, {c})", 
						false,
						"X = a"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "union(if X = a then {a} else {b}, {a})", 
						false,
						"true"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "union(if X = a then {a} else {b}, if Y = a then {a} else {b})", 
						false,
						"X = a or Y = a"),
				//
				// Basic: Set is a Union
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "union()", 
						false,
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "union({a})", 
						false,
						"true"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "union({b})", 
						false,
						"false"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "union({b}, {a})", 
						false,
						"true"),
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "union({b}, {c})", 
						false,
						"false"),
				//
				//
				// Basic: Illegal Argument Exceptions
				//
				new InTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"a", "1+1", 
						true, 
						"N/A"),	
		};
		
		perform(tests);	
	}
	
	@Test
	public void testSetDifference() {
		class SetDifferenceTestData extends TestData {
			private String S1, S2; 
			private Expression exprS1, exprS2;
			
			public SetDifferenceTestData(String contextualConstraint, Model model, String S1, String S2, boolean illegalArgumentTest,  String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.S1 = S1;
				this.S2 = S2;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprS1 = parse(S1);
				this.exprS2 = parse(S2);
				
				return Expressions.apply("-", exprS1, exprS2);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_set_diff,
								LPIUtil.argForSetDifferenceRewriteCall(exprS1, exprS2));
			}
		};
		
		TestData[] tests = new TestData[] {
				//
				// Basic: is S1 is the empty set
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{}", "{a, b}", 
						false,
						"{}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{}", "{{a, b}}",
						false,
						"{}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{}", "{ (on X) X | X != a }", 
						false,
						"{}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{}", "{{ (on X) X | X != a }}", 
						false,
						"{}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union()", "{a, b}", 
						false,
						"{}"),
				//
				// Basic: if S2 is the empty set  
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b}", "{}", 
						false,
						"{a, b}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b}}", "{}", 
						false,
						"{{a, b}}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{ (on X) X | X != a }", "{}", 
						false,
						"{ (on X) X | X != a }"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{ (on X) X | X != a }}", "{}", 
						false,
						"{{ (on X) X | X != a }}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b}", "union()", 
						false,
						"{a, b}"),
				//
				// Basic: if S1 is S11 union S1rest, where S1i and S2 are unisets
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b} union {c, d}", "{b, d}", 
						false,
						"{a} union {c}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union({c, d})", "{b, d}", 
						false,
						"{c} union {}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union()", "{b, d}", 
						false,
						"{}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b} union {c, d} union {e, f} ", "{b, c, e}", 
						false,
						"union({a}, {d} union {f})"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b} union {c, d} union {e, f} union {g, h} ", "{b, c, e, h}", 
						false,
						"union({a}, union({d}, {f} union {g}))"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b} union {c, d} union {e, f} union {g, h} union {i, j} ", "{b, c, e, h, i}", 
						false,
						"union({a}, union({d}, union({f}, {g} union {j})))"),
				//
				// Basic: if S2 is S21 union S2rest, where S1 and S2i are unisets
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d}", "{b} union {d}", 
						false,
						"{a, c}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d}", "union({d})", 
						false,
						"{a, b, c}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d}", "union()", 
						false,
						"{a, b, c, d}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d}", "{b} union {d} union {a}", 
						false,
						"{c}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d}", "{b} union {d} union {a} union {c}", 
						false,
						"{}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d, e}", "{b} union {d} union {a} union {c}", 
						false,
						"{e}"),
				//
				// Basic: if S1 is S11 union S1rest, where S1i are multisets guaranteed to have unique elements, or a singleton, and S2 is a singleton { b }
				//
				// TODO - waiting for ALBP-72 to determine if this is really legal (i.e. treats the R_in as true currently).
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }} union {{(on X) X | X != c }}", "{b}",
						false,
						"{{ ( on X ) X | X != a and X != b }} union {{ ( on X ) X | X != c }}"),
				// R_in is known to be false for the first union argument
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }} union {{(on X) X | X != c }}", "{a}",
						false,
						"{{ ( on X ) X | X != a }} union {{ ( on X ) X | X != c and X != a }}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }} union {{(on Y) [ if p(Y) then 1 else 0 ] | Y != b }}", 
						"{ [ if q(Z) then 1 else 0 ] }", 
						false, 
						"{{ ( on X ) ([ if p(X) then 1 else 0 ]) | X != a }} union {{ ( on Y ) ([ if p(Y) then 1 else 0 ]) | Y != b }}"),
				// A union of a multiset and singleton
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }} union { [ if q(Z) then 1 else 0 ] }", 
						"{ [ if q(Z) then 1 else 0 ] }", 
						false, 
						"{{ ( on X ) ([ if p(X) then 1 else 0 ]) | X != a }}"),
				// A singleton multiset
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }} union {{(on X) X | X != c }}", "{{a}}",
						false,
						"{{ ( on X ) X | X != a }} union {{ ( on X ) X | X != c and X != a }}"),
				//
				// Basic: if S1 is {{a1,...,an}} and S2 is { b }
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{}}", "{a}", 
						false,
						"{ }"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c}}", "{b}", 
						false,
						"{{a, c}}"), 
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{A, B}}", "{C}", 
						false,
						"if A = C then if B = C then { } else {{ B }} else if B = C then {{ A }} else {{ A, B }}"),	
				// A singleton multiset
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, b, c}}", "{{b}}", 
						false,
						"{{a, c}}"), 
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						" {{ [ if p(X) then 1 else 0 ] }}", "{ [ if q(X) then 1 else 0 ] }", 
						false, 
						"{{ ([ if p(X) then 1 else 0 ]) }}"),
				//
				// Basic: if S1 is {{ Alpha | C }}_I and S2 is { b }
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }}", "{b}", 
						false,
						"{{(on X) X | X != a and X != b}}"),
				// Note: In truth the result should be a multisets of all 'a' elements the size of the type of X - 1.
				// However, not currently considered a problem as the multisets 
				// passed to this rewriter are suppose to guarantee to have unique elements. 
				// Keeping this test to highlight the limitation.
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) a | X != a }}", "{a}", 
						false,
						"{ }"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X = a }}", "{a}", 
						false,
						"{ }"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) a | X != a }}", "{b}", 
						false,
						"{{(on X) a | X != a}}"),
				// A singleton multiset
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }}", "{{b}}", 
						false,
						"{{(on X) X | X != a and X != b}}"),
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }}", "{ b }", 
						false, 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a and [ if p(X) then 1 else 0 ] != b }}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }}", "{ [ if p(Y) then 1 else 0 ] }", 
						false, 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a and X != Y }}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }}", "{ [ if q(Y) then 1 else 0 ] }", 
						false, 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }}"),
				// Ensure standardize apart works with factors
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }}", "{ [ if p(X) then 1 else 0 ] }", 
						false, 
						"{{(on X') [ if p(X') then 1 else 0 ] | X' != a and X' != X }}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }}", "{ [ if q(X) then 1 else 0 ] }", 
						false, 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }}"),
			    //
			    // Basic: if S1 is a multiset and S2 is {b1,..., bm}
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{ a, b, a }}", "{ b, a }", 
						false, 
						"{{ a }}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a, b, b, c, c}}", "{a, b}", 
						false, 
						"{{a, b, c, c}}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a, b, b, c, c, d, d}}", "{c, d, a, b}", 
						false, 
						"{{a, b, c, d}}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }}", "{b, c}", 
						false, 
						"{{ ( on X ) X | X != a and X != b and X != c }}"),
				//
				// Basic: if S1 is a multiset and S2 is S21 union … union S2m
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{ a, b, a }}", "{ b } union { a }", 
						false, 
						"{{ a }}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a, b, b, c, c}}", "{ a } union { b }", 
						false, 
						"{{a, b, c, c}}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a, b, b, c, c, d, d}}", "{c, d} union {a, b}", 
						false, 
						"{{a, b, c, d}}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }}", "{ b } union { c }", 
						false, 
						"{{ ( on X ) X | X != a and X != b and X != c }}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }}", "{ b } union {c, d}", 
						false, 
						"{{ ( on X ) X | X != a and X != b and X != c and X != d }}"),
				//
				// Basic: if S1 is {a1,...,an} and S2 is {b1,...,bm}
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{}", "{}", 
						false,
						"{}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{b, c}", 
						false,
						"{a}"), 
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{A, B}", "{C}", 
						false,
						// Note: old R_basic result:
						// "if A = C then if A = B then { } else { B } else if B = C then { A } else if A = B then { A } else { A, B }"
						"if A = C then if B = C then { } else { B } else if B = C then { A } else { A, B }"),	
				//
				// Basic: if S1 is { Alpha | C }_I and S2 is {b1,...,bm}
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) X | X != a }", "{a, b}", 
						false,
						"{(on X) X | not (X = a or X = b)}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) a | X != a }", "{a, b}", 
						false,
						"{ }"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) X | X = a or X = b}", "{a, b}", 
						false,
						"{ }"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) a | X != a }", "{b, c}", 
						false,
					    "{(on ) a | true}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) X | X != a }", "{ }", 
						false,
						"{(on X) X | X != a }"),
				// Ensure standardize apart works
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) X | X != a }", "{X}", 
						false,
						"{(on X') X' | X' != a and X' != X}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) a | X != a }", "{X}", 
						false,
					    "{(on ) a | X != a}"),
				//
				// Basic: if S1 is { Alpha | C }_I and S2 is { Alpha' | C' }_I'
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) X | X != a }", "{(on Y) Y | Y != b }", 
						false,
						// Note: old R_formula_simplification result, which in hindsight looks to be wrong.
						//"{ ( on X ) X | X != a and | type(Y) - { b } | = 0 }"
						// Note: old R_basic result:
						// "{ b }"
						"{ (on ) b | true }"),
				// Ensure standardize apart works
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) X | X != a }", "{(on X) X | X != b }", 
						false,
						// Note: old R_formula_simplification result, which in hindsight looks to be wrong.
						// "{ ( on X ) X | X != a and | type(X) - { b } | = 0 }"	
						// Note: old R_basic result:
						// "{ b }"
						"{ (on ) b | true }"),
				//
				// Basic: if S1 is {a1,...,an} and S2 is { Alpha | C }_I
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{(on X) X | X = b }", 
						false,
						"{a, c}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{(on X) X | X != b }", 
						false,
						"{ b }"),
				//
				// Basic: if S1 is 'if C then Alpha else Beta'
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"if  X = a then {a,b,c} else {d,e,f} ", "{b, e}", 
						false,
						"if X = a then {a, c} else {d, f}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"if  X = a then if Y = a then {a,b} else {c, d} else if Z = a then {e, f} else {g, h}", "{b, d, f, h}", 
						false,
						"if X = a then if Y = a then {a} else {c} else if Z = a then {e} else {g}"),
				//
				// Basic: if S2 is 'if C then Alpha else Beta'
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d, e, f}", "if Y = g then {c} else {f}", 
						false,
						"if Y = g then {a, b, d, e, f} else {a, b, c, d, e}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c, d, e, f}", "if Y = g then if Z = g then {b} else {c} else if Z = g then {e} else {f}", 
						false,
						"if Y = g then if Z = g then {a, c, d, e, f} else  {a, b, d, e, f} else if Z = g then {a, b, c, d, f} else {a, b, c, d, e}"),
				//
				// Basic: S1 and S2 are conditionals
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"if  X = a then {a,b,c} else {d,e,f}", "if  Y = g then {c} else {f}", 
						false,
						"if X = a then if Y = g then {a, b} else {a, b, c} else if Y = g then {d, e, f} else {d, e}"),
				//
				// Complex: if S1 is S11 union S1rest
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b} union {c, d}", "{b} union {d}", 
						false,
						"{a} union {c}"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{(on X) X | X = a or X = b } union {(on X) X | X = c or X =d}", "{b, d}", 
						false,
						// Note: old R_basic result:
						// "{ a } union { c }"
// TODO - can we do better than this with the new R_normalize logic?
// Doing better now, but conversion from { (on ) alpha | C } to if C then { alpha } else {} breaking BP tests at the moment, so it's disabled 						
						"{ (on ) a | true } union { (on ) c | true }"),
				//
				// Fix for failing test: use to remove the A index making it free
				//
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialLoopyMisconceptionExample(),
						"{ ( on A, A', B ) ( ([ m(A) ]), ([ if gA(A') and gB(B) then if m(A') then if m(B) then 10 else 1 else (if m(B) then 5 else 30) else 1 ]) ) | A != X and A' != B and (A = A' or A = B) and (A' != A or B != X) }",
						"{ ( on B, A, B' ) ( ([ m(B) ]), ([ if gA(A) and gB(B') then if m(A) then if m(B') then 10 else 1 else (if m(B') then 5 else 30) else 1 ]) ) | X != B and A != B' and (B = A or B = B') and (A != X or B' != B) }",
						false,
						GrinderUtil.usePlain ? 
								//"{ ( on A, A', B ) ([ m(A) ], [ if gA(A') and gB(B) then if m(A') then if m(B) then 10 else 1 else if m(B) then 5 else 30 else 1 ]) | if A' = A then (A != X) and (A' != B) and (B != X) and (B = A) else (A != X) and (A' != B) and (A = B) and (A' = X) }" // plain cardinality
								"{ ( on A, A', B ) ([ m(A) ], [ if gA(A') and gB(B) then if m(A') then if m(B) then 10 else 1 else if m(B) then 5 else 30 else 1 ]) | if A' = A then (A != X) and (A' != B) and (B != X) and (A = B) else (A != X) and (A' != B) and (A = B) and (A' = X) }"
								: "{ ( on A, A', B ) (  [ m(A) ],   [ if gA(A') and gB(B) then if m(A') then if m(B) then 10 else 1 else if m(B) then 5 else 30 else 1 ]) | (A != X) and (A' != B) and ((A' != A) or (B != X)) and not ((A' != X) or (B != A)) }"), // direct cardinality
//					"{ ( on A, A', B ) ( ([ m(A) ]), ([ if gA(A') and gB(B) then if m(A') then if m(B) then 10 else 1 else (if m(B) then 5 else 30) else 1 ]) ) | A != X and A' != B and (A = A' or A = B) and (A' != A or B != X) and not (B = A and A' != A and A' != X or A' = A and B != A) }"),
//					"{ ( on A, A', B ) ( ([ m(A) ]), ([ if gA(A') and gB(B) then if m(A') then if m(B) then 10 else 1 else (if m(B) then 5 else 30) else 1 ]) ) | A != X and A' != B and (A = A' or A = B) and (A' != A or B != X) and (B != A or A' = A or A' = X) and (A' != A or B = A) }"),
//					"{ ( on A) ( ([ m(A) ]), ([ if gA(X) and gB(A) then if m(X) then if m(A) then 10 else 1 else (if m(A) then 5 else 30) else 1 ]) ) | X != A }"),
				//
				// Basic: Illegal Argument Exceptions
				//
				// Multisets are only every meant to be the first operand S1 (i.e. a multiset of factors).
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{{a}}", 
						true, 
						"N/A"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a, b, c}", "{{a, a}}", 
						true, 
						"N/A"),
				// S1 is a multiset but s2 is also a multiset
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{a, a, b, b, c, c, d, d}}", "{{c, d, a, b}}", 
						true, 
						"N/A"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }}", "{{b, c}}", 
						true, 
						"N/A"),
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{{(on X) X | X != a }} union {{(on X) X | X != c }}", "{{b, d}}", 
						true, 
						"N/A"),
				// S1 is a union of unique multisets with a non singleton
				new SetDifferenceTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"{{(on X) [ if p(X) then 1 else 0 ] | X != a }} union { [ if q(Z) then 1 else 0 ],  [ if p(Z) then 1 else 0 ]}", 
						"{ [ if q(Z) then 1 else 0 ] }", 
						true, 
						"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testIntersection() {
		class IntersectionTestData extends TestData {
			private String S1, S2; 
			private Expression exprS1, exprS2;
			
			public IntersectionTestData(String contextualConstraint, Model model, String S1, String S2, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.S1 = S1;
				this.S2 = S2;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprS1 = parse(S1);
				this.exprS2 = parse(S2);
				
				return Expressions.apply("intersection", exprS1, exprS2);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return  process.rewrite(LBPRewriter.R_intersection, LPIUtil.argForIntersectionRewriteCall(exprS1, exprS2));
			}
		};
		
		TestData[] tests = new TestData[] {
			//
			// Basic: Intersection with an empty set
			new IntersectionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
					"{{ (on X) [ p(X) ] | X = a}}", "{{ (on Y) [ p(Y) ] | Y = b }}", 
					false, 
					"{}"),
			//
			// Basic: Intersection with an intensional set
			new IntersectionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
					"{{ (on X) [ p(X) ] | X != a}}", "{{ (on Y) [ p(Y) ] | Y != a }}", 
					false, 
					"{{ (on Y) [ p(Y) ] | Y != a}}"),
				    // "{{ (on X, Y) [ p(X) ] | X = Y and X != a and Y != a}}"),
			//
			// Basic: standardize apart necessary
			new IntersectionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
					"{{ (on X) [ p(X) ] | X != a}}", "{{ (on X) [ p(X) ] | X != a }}", 
					false, 
					"{{ ( on X ) ([ p(X) ]) | X != a }}"),
					// "{{ (on X', X) [ p(X') ] | X' = X and X' != a and X != a}}"),
			//
			// Basic: Extensional Set
			new IntersectionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
					"{a, b, c}", "{a}", 
					false, 
					"{a}"),
			new IntersectionTestData("Y != X and X = ann", new TrivialLoopyFriendsAnnBobAndSmokerBobExample(),
					"{ ([ friends(ann, Y) ]), ([ smoker(Y) ]) }", "{ ([ friends(X, Y) ]), ([ smoker(X) ]), ([ smoker(Y) ]) }", 
					false, 
					"{ ([ friends(ann, Y) ]), ([ smoker(Y) ]) }"),
// TODO 
			//
			// Basic: Conditional S1
// TODO
			//
			// Basic: Conditional S2
// TODO
			//
			// Basic: Illegal Argument Exceptions
			//
			new IntersectionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
					"{{ (on X) X | X != a}}", "{ (on Y) Y | Y != a }", 
					true, 
					"N/A")
		};
		
		perform(tests);
	}
	
	@Test
	public void testSum() {
		class SumTestData extends TestData {
			private String N, E, Pi, T, beingComputed; 
			private Expression exprN, exprE, exprPi, exprT, exprBeingComputed;
			private int expectedNumberOfComputedMessages = -1;
			private LBPConfiguration configuration = LBPFactory.newLBPConfiguration();
			
			public SumTestData(String contextualConstraint, Model model, String N, String E, String Pi, String T, String beingComputed, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.N             = N;
				this.E             = E;
				this.Pi            = Pi;
				this.T             = T;
				this.beingComputed = beingComputed;
			};
			
			public SumTestData(String contextualConstraint, Model model, String N, String E, String Pi, String T, String beingComputed, boolean illegalArgumentTest, String expected, int expectedNumberOfComputedMessages) {
				this(contextualConstraint, model, N, E, Pi, T, beingComputed, illegalArgumentTest, expected);
				this.expectedNumberOfComputedMessages = expectedNumberOfComputedMessages;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprN             = parse(N);
				this.exprE             = parse(E);
				this.exprPi            = parse(Pi);
				this.exprT             = parse(T);
				this.exprBeingComputed = parse(beingComputed);
				
				Expression ETimesPi = Times.make(Arrays.asList(exprE, exprPi));
				Expression sumMultiset = new DefaultIntensionalMultiSet(new ExtensionalIndexExpressionsSet(ExtensionalSet.getElements(exprN)), ETimesPi, Expressions.TRUE);
				return Expressions.apply(FunctorConstants.SUM, sumMultiset);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				configuration.setSumRewriterTestMessageCounter(0);
				RewritingProcess lbpProcess = LBPFactory.newBoundLBPProcess(process.getRootExpression(), configuration, process);
				return lbpProcess.rewrite(LBPRewriter.R_sum, LPIUtil.argForSumRewriteCall(exprN, exprE, exprPi, exprT, exprBeingComputed));
			}
			
			@Override
			public String perform(int i) {
				String errorMessage = super.perform(i);
				if (errorMessage != null) {
					return errorMessage;
				}
				if (expectedNumberOfComputedMessages != -1) {
					if (expectedNumberOfComputedMessages != configuration.getSumRewriterTestMessageCounter()) {
						return "Expected " + expectedNumberOfComputedMessages + " messages to be computed but " + configuration.getSumRewriterTestMessageCounter() + " were actually computed.";
					}
				}
				return null;
			}
		};
		TestData[] tests = new TestData[] {
				//
				// Basic: Straight forward cases
				//
				//
				// Use a logical variable name in the model
				new SumTestData(Expressions.TRUE.toString(), new TrivialPQWithPriors(), 
						"{ [ q(X) ] }",
						"if p(X) and q(X) then 1 else 0",
						"product({{ ( on V' in { ([ q(X) ]) } ) message to [ if p(X) and q(X) then 1 else 0 ] from V' }})",
						"[ p(X) ]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false, 
						"if p(X) then 1 else 0",
						0 /* given determinism, there is no need to compute the incoming message */),	
				// Use a logical variable name not in the model
				new SumTestData(Expressions.TRUE.toString(), new TrivialPQWithPriors(), 
						"{ [ q(W) ] }",
						"if p(W) and q(W) then 1 else 0",
						"product({{ ( on V' in { ([ q(W) ]) } ) message to [ if p(W) and q(W) then 1 else 0 ] from V' }})",
						"[ p(W) ]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false, 
						"if p(W) then 1 else 0",
						0),
				//
				new SumTestData(Expressions.TRUE.toString(), new WeightedPQWithPriors(), 
						"{ [ q(X) ] }",
						"if p(X) and q(X) then 0.6 else 0.4",
						"product({{ ( on V' in { ([ q(X) ]) } ) message to [ if p(X) and q(X) then 0.6 else 0.4 ] from V' }})",
						"[ p(X) ]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false, 
						"if p(X) then 0.46 else 0.4",
						1 /* lack of determinism makes computing the message necessary */),
				//
				new SumTestData(Expressions.TRUE.toString(), new TrivialPQRWithPriors(), 
						"{ [ q(X) ], [ r(X) ] }",
						"if p(X) and q(X) and r(X) then 0.6 else 0.4",
						"product({{ ( on V' in { [ q(X) ], [ r(X) ] } ) message to [ if p(X) and q(X) and r(X) then 0.6 else 0.4 ] from V' }})",
						"[ p(X) ]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false, 
						"0.40",
						1 /* determinism on q makes it unnecessary to compute message on r*/),
				//
				// Basic: Loopy Models
				//
				/*TODO - Loopy model, currently not supported.
				new SumTestData(new TrivialLoopyPQWithPriors(), 
						"{ [ q(Y) ] }",
						"if p(X) and q(Y) then 1 else 0",
						"product({{ ( on V' in { ([ q(Y) ]) } ) 'message to . from .'([ if p(X) and q(Y) then 2 else 1 ], V') }})", 
						false, 
						"if p(X) then 0.3 else 0"),	
				*/
				//
				// Basic: Illegal Argument Exceptions
				//
		};
		
		perform(tests);	
	}
	
	@Test
	public void testMessageToFactorFromVariable() {
		class MsgToFFromVTestData extends TestData {
			private String msgToF_V, beingComputed; 
			private Expression exprMsgToF_V, exprBeingComputed;
			
			public MsgToFFromVTestData(String contextualConstraint, Model model, String msgToF_V, String beingComputed, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.msgToF_V      = msgToF_V;
				this.beingComputed = beingComputed;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprMsgToF_V      = parse(msgToF_V);
				this.exprBeingComputed = parse(beingComputed);
				return this.exprMsgToF_V;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_m_to_f_from_v,
							LPIUtil.argForMessageToFactorFromVariableRewriteCall(this.exprMsgToF_V, this.exprBeingComputed));
			}
		};
		TestData[] tests = new TestData[] {
				//
				// Basic: Straight forward cases
				//
				new MsgToFFromVTestData(Expressions.TRUE.toString(), 
						new Model(
								"union( {{ (on X) [if p(X) then 0.2 else 0.3] }}, {{ [if p(a) then 1 else 0] }} )",
								"p/1"
						),
						"message to [if p(a) then 1 else 0] from [p(a)]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if p(a) then 0.2 else 0.3"
				),
				new MsgToFFromVTestData(Expressions.TRUE.toString(), 
						new Model(
								"union( {{ [if p(X) then 0.2 else 0.3] }}, {{ [if p(a) then 1 else 0] }} )",
								"p/1"
						),
						"message to [if p(a) then 1 else 0] from [p(a)]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						// Note: old R_basic result
						// "if X = a then if p(a) then 0.2 else 0.3 else 1"
						// Note: no constraint applier used in R_normalize so p(X) instead of p(a).
						"if X = a then if p(a) then 0.20 else 0.30 else 1"
						// Note that the above model uses a free variable inside a parfactor, an unusual feature
						// This ended up helping to find a bug that causes free variables in parfactors not to extend the context.
				),
				// From ALBPTest.testMessageToFactorFromVariable()
				new MsgToFFromVTestData(Expressions.TRUE.toString(), 
						new TrivialPQWithPriors(),
						"message to [if p(X) and q(X) then 1 else 0] from [p(X)]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if p(X) then 0.2 else 0.8"),
				// Basic: Illegal Argument Exceptions
				//
				new MsgToFFromVTestData(Expressions.TRUE.toString(), 
						new TrivialLoopyPQWithPriors(), 
						"message to [if p(X) and q(Y) then 2 else 1] from [if p(X) and q(Y) then 1 else 0]", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						true, 
						"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testNeighboursRandomVariable() {
		class NRVTestData extends TestData {
			private String neighV; 
			private Expression exprNeighV;
			
			public NRVTestData(String contextualConstraint, Model model, String neighV, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.neighV = neighV;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprNeighV = parse(neighV);
				return this.exprNeighV;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_neigh_v, exprNeighV);
			}
		};
		
		TestData[] tests = new TestData[] {
				//
				// Basic: Simple tests
				// 
				new NRVTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"Neigh([p(z)])", 
						false, 
						"{{ ( on X, Y ) ([ if p(X) or p(Y) then 1 else 0 ]) | X = z or Y = z }}"),
				new NRVTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"Neigh([p(b)])", 
						false, 
						"{{ ( on X, Y ) ([if p(b) and q(X, Y) and r then 1 else 0]) | X != a }} union {{ ( on X, Y ) ([if p(X) or p(Y) then 1 else 0]) | X = b or Y = b }}"),
				new NRVTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"Neigh([p(Z)])", 
						false, 
						"{{ ( on X, Y ) ([if p(b) and q(X, Y) and r then 1 else 0]) | X != a and Z = b }} union {{ ( on X, Y ) ([if p(X) or p(Y) then 1 else 0]) | Z = X or Z = Y }}"),
				//
				new NRVTestData(Expressions.TRUE.toString(), new TrivialLoopyPQWithPriors(), 
						"Neigh([p(Z)])", 
						false, 
						"{{ ( on Y ) ([ if p(Z) and q(Y) then 2 else 1 ]) }} union {{ ([ if p(Z) then 0.2 else 0.8 ]) }}"),
				new NRVTestData(Expressions.TRUE.toString(), new TrivialLoopyPQWithPriors(), 
						"Neigh([q(Z)])", 
						false, 
						"{{ ( on X ) ([ if p(X) and q(Z) then 2 else 1 ]) }} union {{ ([ if q(Z) then 0.3 else 0.7 ]) }}"),
				//
				new NRVTestData(Expressions.TRUE.toString(), new IntensionalFanIn(), 
						"Neigh([p])", 
						false, 
						"{{ ( on X in People ) ([if q(X) then if p then 1 else 0 else if p then 0 else 1]) }}"),
				new NRVTestData(Expressions.TRUE.toString(), new IntensionalFanIn(), 
						"Neigh([p])", 
						false, 
						"{{ ( on X in People ) ([if q(X) then if p then 1 else 0 else if p then 0 else 1]) }}"),
				new NRVTestData(Expressions.TRUE.toString(), new IntensionalFanIn(), 
						"Neigh([q(a)])", 
						false, 
						"{{ ([ if q(a) then if p then 1 else 0 else if p then 0 else 1 ]) }}"),
				// 
				new NRVTestData(Expressions.TRUE.toString(), new IntensionalFanIn(), 
						"Neigh([q(a1)])", 
						false, 
						"{{ ([ if q(a1) then if p then 1 else 0 else if p then 0 else 1 ]), ([ if q(a1) then 1 else 0 ]) }}"),
				new NRVTestData(Expressions.TRUE.toString(), new IntensionalFanIn(), 
						"Neigh([q(Z)])", 
						false, 
						// Note: old R_basic result:
						// "if Z = a1 then {{ ([ if q(a1) then if p then 1 else 0 else if p then 0 else 1 ]), ([ if q(a1) then 1 else 0 ]) }} else if Z = a2 then {{ ([ if q(a2) then if p then 1 else 0 else if p then 0 else 1 ]), ([ if q(a2) then 1 else 0 ]) }} else {{ ([ if q(Z) then if p then 1 else 0 else if p then 0 else 1 ]) }}"
						// Note: no constraint applier used anymore so q(Z) instead of q(a1)
						"if Z = a1 then {{ ([ if q(a1) then if p then 1 else 0 else if p then 0 else 1 ]), ([ if q(a1) then 1 else 0 ]) }} else if Z = a2 then {{ ([ if q(a2) then if p then 1 else 0 else if p then 0 else 1 ]), ([ if q(a2) then 1 else 0 ]) }} else {{ ([ if q(Z) then if p then 1 else 0 else if p then 0 else 1 ]) }}"),
				//
				// Basic: Illegal Argument Exceptions
				//
				// Not a Random Variable in the model
				new NRVTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"Neigh([m(a)])", 
						true, 
						"N/A"),	
		};
		
		perform(tests);
	}
	
	@Test
	public void testNeighboursOfRandomVariableInParfactor() {
		class NRVIPFTestData extends TestData {
			private String Ev, PF; 
			private Expression exprEv, exprPF;
			
			public NRVIPFTestData(String contextualConstraint, Model model, String Ev, String PF, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.Ev = Ev;
				this.PF = PF;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprEv = parse(Ev);
				this.exprPF = parse(PF);
				return Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", exprEv, exprPF);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_neigh_v_parf,
							LPIUtil.argForNeighborsOfRandomVariableInParfactorRewriteCall(exprEv, exprPF));
			}
		};
		
		TestData[] tests = new TestData[] {
				//
				// Basic: Trivial argument cases
				// 
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(X)]", "{ }", 
						false, 
						"{ }"),	
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(X)]", "{{ }}", 
						false, 
						"{ }"),	
				//
				// Basic: Extensional Parfactor cases
				//
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a1)]", "{ [if p(a1) then 1 else 0] }", 
						false, 
						"{ [if p(a1) then 1 else 0] }"),	
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a1)]", "{{ [if p(a1) then 1 else 0] }}", 
						false, 
						"{{ [if p(a1) then 1 else 0] }}"),
				//Note: Currently when normailized will create a comparison of the parfactors in a conditional, which is currently
				// not supported. This is currently determined an edge case that should not occur as the model is usually intensional.
				//new NRVIPFTestData(new TrivialPQR(), "[p(a1)]", "union({ [if p(a1) then 1 else 0]}, {[if p(a1) and q(a1) then 1 else 0] })", false, "union([if p(a1) then 1 else 0], [if p(a1) and q(a1) then 1 else 0])"),
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a1)]", "{{ [if p(a1) then 1 else 0], [if p(a1) and q(a1) then 1 else 0] }}", 
						false, 
						"{{ ([ if p(a1) then 1 else 0 ]), ([ if p(a1) and q(a1) then 1 else 0 ]) }}"),
				// 
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(X)]", "{ [if p(a1) then 1 else 0] }", 
						false, 
						"if X = a1 then {[if p(a1) then 1 else 0]} else { }"),	
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(X)]", "{{ [if p(a1) then 1 else 0] }}", 
						false, 
						"if X = a1 then {{[if p(a1) then 1 else 0]}} else { }"),					
				//
				// Basic: Intensional Parfactor cases
				//
				// No change expected
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{ (on X in {a,b,c}) [if p(a) then 1 else 0] | X != d }", 
						false, 
						"{ (on X in {a,b,c}) [if p(a) then 1 else 0] | X != d }"),
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{{ (on X in {a,b,c}) [if p(a) then 1 else 0] | X != d }}", 
						false, 
						"{{ (on X in {a,b,c}) [if p(a) then 1 else 0] | X != d }}"),
				// and addition expected
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{ (on X in {a,b,c}) [if p(X) then 1 else 0] | X != d }", 
						false, 
						"{ ([if p(a) then 1 else 0]) }"),
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{{ (on X in {a,b,c}) [if p(X) then 1 else 0] | X != d }}", 
						false, 
						"{{ ([if p(a) then 1 else 0]) }}"),
				// and addition or expected
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{ (on X in {a,b,c}, Y in {d, e, f}) [if p(X) and p(Y) then 1 else 0] | X != d }", 
						false, 
						// Note: old R_formula_simplification result
						// "{ (on X in {a,b,c}, Y in {d,e,f}) [if p(X) and p(Y) then 1 else 0] | X = a or (X != d and Y = a) }"
						"{ (on X in {a,b,c}, Y in {d,e,f}) [if p(X) and p(Y) then 1 else 0] |X != d and (X = a or Y = a) }"),
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{{ (on X in {a,b,c}, Y in {d, e, f}) [if p(X) and p(Y) then 1 else 0] | X != d }}", 
						false,
						// Note: old R_formula_simplification result
						// "{{ (on X in {a,b,c}, Y in {d,e,f}) [if p(X) and p(Y) then 1 else 0] | X = a or (X != d and Y = a) }}"
						"{{ (on X in {a,b,c}, Y in {d,e,f}) [if p(X) and p(Y) then 1 else 0] | X != d and (X = a or Y = a) }}"),
				// false C
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{ (on X in {a,b,c}) [if p(a) then 1 else 0] | false }", 
						false, 
						"{ }"),
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(),
						"[p(a)]", "{{ (on X in {a,b,c}) [if p(a) then 1 else 0] | false }}", 
						false, 
						"{ }"),
				// I is empty
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{ [if p(a) then 1 else 0] | true }", 
						false, 
						"{ [if p(a) then 1 else 0] }"),
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{{ [if p(a) then 1 else 0] | true }}", 
						false, 
						"{{ [if p(a) then 1 else 0] }}"),
				// if C is (C' and i = Beta) for i an index in I
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{ (on X) [if p(X) then 1 else 0] | Z = X }", 
						false,
						// Note: old R_formula_simplification result
						// "{ ( on X ) ([ if p(X) then 1 else 0 ]) | X = Z = a }"
						"if Z = a then { ([ if p(a) then 1 else 0 ]) } else { }"),
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{{ (on X) [if p(X) then 1 else 0] | Z = X }}", 
						false, 
						// Note: old R_formula_simplification result
						// "{{ ( on X ) ([ if p(X) then 1 else 0 ]) | X = Z = a }}"
						"if Z = a then {{ ([ if p(a) then 1 else 0 ]) }} else { }"),
				// Standardize Apart
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(X)]", "{ (on X in {a,b,c}) [if p(X) then 1 else 0] | X != d }", 
						false, 
						"if X != d then { ([ if p(X) then 1 else 0 ]) } else { }"),
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(X)]", "{{ (on X in {a,b,c}) [if p(X) then 1 else 0] | X != d }}", 
						false, 
						"if X != d then {{ ([ if p(X) then 1 else 0 ]) }} else { }"),
				//
				// Basic: Illegal Argument Exceptions
				// 
				// Not a Random Variable in the model
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[X]", "{ }", 
						true, 
						"N/A"),	
				// Not a Parfactor
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(X)]", "+(1, 2)", 
						true, 
						"N/A"),	
				// Head clause of Intensional set is not a factor 
				new NRVIPFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(a)]", "{{ (on X) if p(X) then 1 else 0 | Z = X }}", 
						true, 
						"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testUnion() {
		class UnionTestData extends TestData {
			private String U; 
			private Expression exprU;
			
			public UnionTestData(String contextualConstraint, Model model, String U, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.U = U;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprU = parse(U);
				return exprU;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_union, exprU);
			}
		};
		
		TestData[] tests = new TestData[] {
				//
				// Basic: Simple argument cases
				// 
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(), 
						"union()", 
						false, 
						"{}"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union({})", 
						false, 
						"{}"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union({a})", 
						false, 
						"{a}"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union({a,b,c})", 
						false, 
						"{a,b,c}"),
				//
				// Basic: Remove empty set cases
				//
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{} union {a}", 
						false, 
						"{a}"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a} union {}", 
						false, 
						"{a}"),
				//
				// Basic: Externalize Conditionals
				//
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"if X = Y then {a} union {} else {} union {b}", 
						false, 
						"if X = Y then {a} else {b}"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union(if X = Y then {a} union {} else {} union {b})", 
						false, 
						"if X = Y then {a} else {b}"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union(if X = Y then {a} union {} else {} union {b}, {c})", 
						false, 
						"if X = Y then { a, c } else { b, c }"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union({c}, if X = Y then {a} union {} else {} union {b})", 
						false, 
						"if X = Y then { c, a } else { c, b }"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union(if X = Y then {a} union {} else {} union {b}, if R = Z then {c} union {} else {} union {d})", 
						false, 
						"if X = Y then if R = Z then { a, c } else { a, d } else if R = Z then { b, c } else { b, d }"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union(if X = Y then {a} union (if R = Z then {d} else {e}) else (if M = N then {f} else {g}) union {b}, {c})", 
						false, 
						"if X = Y then if R = Z then { a, c, d } else { a, c, e } else if M = N then { f, b, c } else { g, b, c }"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union({c}, if X = Y then {a} union (if R = Z then {d} else {e}) else (if M = N then {f} else {g}) union {b})", 
						false, 
						"if X = Y then if R = Z then { c, a, d } else { c, a, e } else if M = N then { c, f, b } else { c, g, b }"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"(if X = Y then { a } union (if R = Z then { d } else { e }) else (if M = N then { f } else { g }) union { b }) union (if K = L then {c} union (if J = P then {j} else {k}) else (if G = H then {m} else {n}) union {h})", 
						false, 
						"if X = Y then if R = Z then if K = L then if J = P then { a, d, c, j } else { a, d, c, k } else if G = H then { a, d, m, h } else { a, d, n, h } else if K = L then if J = P then { a, e, c, j } else { a, e, c, k } else if G = H then { a, e, m, h } else { a, e, n, h } else if M = N then if K = L then if J = P then { f, b, c, j } else { f, b, c, k } else if G = H then { f, b, m, h } else { f, b, n, h } else if K = L then if J = P then { g, b, c, j } else { g, b, c, k } else if G = H then { g, b, m, h } else { g, b, n, h }"),
				//
				// Basic: Test unflattened unions, that the code can handle
				//
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union(union({a}, {b}), {c})", 
						false, 
						"{a, b, c}"),	
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"union({a}, union(union({b}, {c})))", 
						false, 
						"{a, b, c}"),
				//
				// Basic: Test R_basic on Random Variables call.
				//
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"{{ ( on X, Y ) ([ if p(b) and q(X, Y) and r then 1 else 0 ]) | X != a }} union {{ ( on X, Y ) ([ if p(X) or p(Y) then 1 else 0 ]) | [ p(b) ] = [ p(X) ] or [ p(b) ] = [ p(Y) ] }}", 
						false, 
						"{{ ( on X, Y ) ([ if p(b) and q(X, Y) and r then 1 else 0 ]) | X != a }} union {{ ( on X, Y ) ([ if p(X) or p(Y) then 1 else 0 ]) | X = b or Y = b }}"),
				//
				// Basic: Illegal Argument Exceptions
				//
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"1", 
						true, 
						"N/A"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a} union 1", 
						true, 
						"N/A"),
				new UnionTestData(Expressions.TRUE.toString(), new TrivialPQ(),
						"{a} union {b} union 1", 
						true, 
						"N/A"),
		};

		perform(tests);
	}

	@Test
	public void testProductFactor() {
		class PFTestData extends TestData {
			private String Pi, beingComputed; 
			private Expression exprPi, exprBeingComputed;
			private Map<Object, Object> globalObjects;
			
			public PFTestData(String contextualConstraint, Model model, String Pi, String beingComputed, boolean illegalArgumentTest, String expected) {
				this(contextualConstraint, model, Pi, beingComputed, null, illegalArgumentTest, expected);
			};
			
			public PFTestData(String contextualConstraint, Model model, String Pi, String beingComputed, Map<Object, Object> globalObjects, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.Pi            = Pi;
				this.beingComputed = beingComputed;
				this.globalObjects = globalObjects;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprPi            = parse(Pi);
				this.exprBeingComputed = parse(beingComputed);
				
				return this.exprPi;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				if (globalObjects != null) {
					process.getGlobalObjects().putAll(globalObjects);
				}
				return process.rewrite(LBPRewriter.R_prod_factor, 
							LPIUtil.argForProductFactorRewriteCall(exprPi, exprBeingComputed));
			}
		};
		TestData[] tests = new TestData[] {
				//
				// Basic: Straight forward cases
				//
				new PFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on F in { })  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
				        "1"),
				new PFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on F in { [if r and (p(X) or not p(X)) then 0.2 else 0.3] } )  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if r then 0.4 else 0.6"), // this is ((if r then 0.2 else 0.3) + (if r then 0.2 else 0.3))
				// the summation comes from summing p(X) out (a boolean variable).
				new PFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on F in {{ (on X) [if r and (p(X) or not p(X)) then 0.2 else 0.3] | X = a or X = b }} )  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if r then 0.16 else 0.36"), // this is ((if r then 0.2 else 0.3) + (if r then 0.2 else 0.3)) ^ 2
				        // the summation comes from summing p(X) out (a boolean variable)
				        // and the exponentiation comes from the two instances of the factor, one for X = a and the other for X = b.
				        // The point of the irrelevant p(X) in the factor above is so that the factor is really on r alone, so the message is easily predictable.
				        // p(X) is included so that we have more than one instance of the factor (two, to be exact)
				        // so we can test the exponentiation of the potentials.
				new PFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on F in {{ [if r and (p(a) or not p(a)) then 0.2 else 0.3] }} union {{ [if r and (p(b) or not p(b)) then 0.2 else 0.3] }} )  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if r then 0.16 else 0.36"),
				new PFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on F in {{ [if r and (p(X) or not p(X)) then 0.2 else 0.3] }} union {{ [if r and (p(Y) or not p(Y)) then 0.2 else 0.3] }} )  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if r then 0.16 else 0.36"),
				// Ensure union with a singleton uniset allowed.
				new PFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on F in {{ [if r and (p(X) or not p(X)) then 0.2 else 0.3] }} union { [if r and (p(Y) or not p(Y)) then 0.2 else 0.3] } )  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if r then 0.16 else 0.36"),
				// Ensure union with a singleton uniset and emptyset allowed.
				new PFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on F in {{ [if r and (p(X) or not p(X)) then 0.2 else 0.3] }} union { [if r and (p(Y) or not p(Y)) then 0.2 else 0.3] } union {} )  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if r then 0.16 else 0.36"),
				new PFTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"product({{ (on F in { [if p(b) and q(a,b) and r then 1 else 0] })  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if r then 1 else 0"),	
				new PFTestData(Expressions.TRUE.toString(), 
						new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on F in (if X = Y then { [if r and (p(X) or not p(X)) then 0.2 else 0.3] } else { }) )  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if X = Y then if r then 0.4 else 0.6 else 1"),
				new PFTestData(Expressions.TRUE.toString(), 
						new Model(
								"union(" +
								"{{ (on X,Y) [if p(X) and q(X,Y) then 2 else 3] }}," +
								"{{ (on Y)   [if q(a,Y) then 10 else 20] }}" +
								")",
								"p/1", "q/2"
						), 
						"product({{ (on F in {{ (on Y) [if p(Z) and q(Z,Y) then 2 else 3] }} )  (message to [p(Z)] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						// Note: Universe is the default sort name associated with a model that doesn't specify any.
						Util.map(parse("| Universe |"), parse("2")),
						false,
						// Note: old R_basic result:
						// "if Z = a then if p(a) then 6400 else 8100 else if p(Z) then 25 else 36"
						// Note: no constraint applier used in R_normalize so p(Z) instead of p(a).
						"if Z = a then if p(a) then 6400 else 8100 else if p(Z) then 25 else 36"
						// This tests conditionals inside the message that do not depend on the indices of the product.
						// for Z = a, message on q(a,Y) is if q(a,y) then 10 else 20.
						// Then we sum over q and get 2*10 + 3*20 for true p and 3*10 + 3*20 for false p
						// That gives us 80 and 90, which are then squared to 6400 and 8100.
						// For Z != a, we get message 1 on q.
						// Then we sum over q and get 2*1 + 3*1 for true p and 3*1 + 3*1 for false p
						// That gives us 5 and 6, which are then squared to 25 and 36.
				),
				new PFTestData(Expressions.TRUE.toString(), 
						new Model(
								"union(" +
								"{{ (on X) [if p and q(X) then 2 else 3] }}," +
								"{{ [if q(a) then 10 else 20] }}" +
								")",
								"p/0", "q/1"
						), 
						"product({{ (on F in {{ (on X) [if p and q(X) then 2 else 3] }} )  (message to [p] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						// Note: here we override based on 'type(X)' directly instead of using 'Universe'
						Util.map(parse("|type(X)|"), parse("3")),
						false,
						"if p then 2000 else 3240"
						// This tests conditionals inside the message that *do* depend on the indices of the product.
						// Message from [if p and q(X) then 2 else 3] to [p] will be:
						// if X = a then if p then 80 else 90 else if p then 5 else 6
						// (this comes from 10*2 + 20*3 and 10*3 + 20*3 and 2 + 3 and 3 + 3)
						// Then product will be split into
						// (if p then 80 else 90)^|X=a| * (if p then 5 else 6)^|X != a|
						// (if p then 80 else 90) * (if p then 5 else 6)^2
						// (if p then 80 else 90) * (if p then 25 else 36)
						// if p then 2000 else 3240
				),
				//
				// Basic: Illegal Argument Exceptions
				new PFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on F in { [if r and (p(X) or not p(X)) then 0.2 else 0.3] } union { [if r and (p(Y) or not p(Y)) then 0.2 else 0.3], [if r and (p(Y) or not p(Y)) then 0.2 else 0.3] } )  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						true, // not defined on union of non empty or unisets. 
						"N/A"),
		};
		
		perform(tests);	
	}
	 
	@Test
	public void testMessageToVariableFromFactor() {
		class MsgToVFromFTestData extends TestData {
			private String msgToV_F, beingComputed; 
			private Expression exprMsgToV_F, exprBeingComputed;
			
			public MsgToVFromFTestData(String contextualConstraint, Model model, String msgToV_F, String beingComputed, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.msgToV_F      = msgToV_F;
				this.beingComputed = beingComputed;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprMsgToV_F      = parse(msgToV_F);
				this.exprBeingComputed = parse(beingComputed);
				
				return this.exprMsgToV_F;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_m_to_v_from_f,
							LPIUtil.argForMessageToVariableFromFactorRewriteCall(this.exprMsgToV_F, this.exprBeingComputed));
			}
		};
		TestData[] tests = new TestData[] {
				//
				// Basic: Straight forward cases
				//
				// From ALBPTest.testMessageToVariableFromFactorSingleStep()
				new MsgToVFromFTestData(Expressions.TRUE.toString(), 
						new Model(
								"union(" +
								"{{(on X) [if p(X) and q(X) then 0.2 else 0.3]}}" + ")",
								"p/1", "q/1"
						),
						"message to [p(X)] from [if p(X) and q(X) then 0.2 else 0.3]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false, 
						"if p(X) then 0.5 else 0.6"),
				// Test for issue #4: http://code.google.com/p/aic-praise/issues/detail?id=4
				new MsgToVFromFTestData(Expressions.TRUE.toString(), 
						new EmptyPQ(),
						"message to [p(X)] from [if p(X) and q(X) then 0.2 else 0.3]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false, 
						"if p(X) then 0.5 else 0.6"),
				// From ALBPTest.testBelief()
				new MsgToVFromFTestData(Expressions.TRUE.toString(), 
						new TrivialPeopleAmericanTallIntelligentUnintelligent(),
						"message to [intelligent(X)] from [if intelligent(X) <=> not unintelligent(X) then 1 else 0]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"1"),
				new MsgToVFromFTestData(Expressions.TRUE.toString(), 
						new TrivialPeopleAmericanTallIntelligentUnintelligent(),
						"message to [tall(X)] from [if tall(X) then 2 else 8]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if tall(X) then 2 else 8"),
				new MsgToVFromFTestData(Expressions.TRUE.toString(), 
						new TrivialPeopleAmericanTallIntelligentUnintelligent(),
						"message to [tall(X)] from [if tall(X) and american(X) then 7 else 1]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						false,
						"if tall(X) then 8 else 2"),
				//
				// Basic: Illegal Argument Exceptions
				//
				new MsgToVFromFTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"message to [if p(X) then 0.6 else 0.4] from [if p(X) then 0.6 else 0.4]",
						LPIUtil.createNewBeingComputedExpression().toString(),
						true, 
						"N/A"
				),
		};
		
		perform(tests);
	}
	
	@Test
	public void testRandomVariableIsReferencedByExpression() {
		class RVReferencedTestData extends TestData {
			private String V, E; 
			private Expression exprV, exprE, topE;
			
			public RVReferencedTestData(String contextualConstraint, Model model, String V, String E, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.V = V;
				this.E = E;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprV = parse(V);
				this.exprE = parse(E);
			
				topE = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_NEIGHBORS_OF_FROM, exprV, exprE);
				
				return topE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return LPIUtil.randomVariableIsReferencedByExpression(exprV, exprE, process);
			}
		};

		TestData[] tests = new TestData[] {
				//
				// Basic: Known true
				//
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(fred)]", "if p(fred) and q(tom, jerry) and r then 1 else 0", 
						false, 
						"true"),
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(Z)]", "if p(Z) and q(X,Y) and r then 1 else 0", 
						false, 
						"true"),
				//
				// Basic: Known false
				//
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(fred)]", "if p(tom) and q(tom, jerry) and r then 1 else 0", 
						false, 
						"false"),
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(Z)]", "if A then B else C", 
						false, 
						"false"),
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(Z)]", "if [A] then [B] else [C]", 
						false, 
						"false"),
				// Are false because subexpressions of a bracketed expression are just the Variables
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(fred)]", "[if p(fred) and q(tom, jerry) and r then 1 else 0]", 
						false, 
						"false"),
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(Z)]", "[if p(Z) and q(X,Y) and r then 1 else 0]", 
						false, 
						"false"),
				//
				// Basic: Undetermined.
				//
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(W)]", "if p(Z) and q(X,Y) and r then 1 else 0", 
						false, 
						"W = Z"),
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[p(W)]", "if p(Z) and p(X) and r then 1 else 0", 
						false, 
						"W = Z or W = X"),
				//
				// Basic: 0 arity random variable.
				//
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[r]", "if A then 1 else 0", 
						false, 
						"false"),
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[r]", "if p(Z) and q(X,Y) and r then 1 else 0", 
						false, 
						"true"),
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[r]", "if p(Z) and q(X,Y) then 1 else 0", 
						false, 
						"false"),
				//
				// Basic: Illegal Argument Exceptions
				//
				// s is not a random variable in this model.
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[s(fred)]", "if s(fred) and q(tom, jerry) and r then 1 else 0", 
						true, 
						"N/A"),
				// Not bracketed, i.e. is the value expression for the random variable.
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"p(fred)", "if p(fred) and q(tom, jerry) and r then 1 else 0", 
						true, 
						"N/A"),
				// the Variable A is not a Random Variable.
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"A", "if p(fred) and q(tom, jerry) and r then 1 else 0", 
						true, 
						"N/A"),
				// Not allowed to use a Variable in place  of the random variable's value expression
				new RVReferencedTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"[ A ]", "if p(fred) and q(tom, jerry) and r then 1 else 0", 
						true, 
						"N/A"),				
		};
		
		perform(tests);
	}
	
	@Test
	public void testIntensionalSimplification() {
		class IntensionalSimplificationTestData extends TestData {
			private String S; 
			private Expression exprS;
			
			public IntensionalSimplificationTestData(String contextualConstraint, Model model, String S, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.S = S;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprS = parse(S);
				return exprS;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_intensional_simplification, exprS);
			}
		};
		
		TestData[] tests = new TestData[] {
				//
				// Basic: No changes expected for intensional unisets
				//
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X) f(X) | X != a}", 
						false, 
						"{(on X) f(X) | X != a}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | X != a}", 
						false, 
						"{(on X,Y) f(X, Y) | X != a}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | (M = N) and (a = X = R)}", 
						false, 
						"{(on X,Y) f(X, Y) | M = N and a = X = R}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | (a = X = R) and (M = N)}", 
						false, 
						"{(on X,Y) f(X, Y) | a = X = R and M = N}"),
				//
				// Basic: No changes expected for intensional multisets
				//
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X) f(X) | X != a}}", 
						false, 
						"{{(on X) f(X) | X != a}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | X != a}}", 
						false, 
						"{{(on X,Y) f(X, Y) | X != a}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | (M = N) and (a = X = R)}}", 
						false, 
						"{{(on X,Y) f(X, Y) | M = N and a = X = R}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | (a = X = R) and (M = N)}}", 
						false, 
						"{{(on X,Y) f(X, Y) | a = X = R and M = N}}"),
				//
				// Basic: Simple argument cases for intensional unisets.
				// 
				// if C is false"
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X) f(X) | false}", 
						false, 
						"{}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X) f(X) | a = b}", 
						false, 
						"{}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{f(X) | false}", 
						false, 
						"{}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{f(X) | a = b}", 
						false, 
						"{}"),
				// if I is empty
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{f(X) | a = a}", 
						false, 
						"{f(X)}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{f(X) | Y = Z}", 
						false, 
						"if Y = Z then {f(X)} else {}"),
				// if C is (C' and i = Beta) for i an index in I
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X) f(X) | Z = X}", 
						false, 
						"{f(Z)}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X) | Z = X}", 
						false, 
						"{(on Y) f(Z)}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | Z = X}", 
						false, 
						"{(on Y) f(Z, Y)}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | a = X}", 
						false, 
						"{(on Y) f(a, Y)}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | (M = N) and (a = X)}", 
						false, 
						"{(on Y) f(a, Y) | M = N}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | (a = X) and (M = N)}", 
						false, 
						"{(on Y) f(a, Y) | M = N}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | and(a = X, M = N, K = L)}", 
						false, 
						"{(on Y) f(a, Y) | M = N and K = L}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | and(M = N, a = X, K = L)}", 
						false, 
						"{(on Y) f(a, Y) | M = N and K = L}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{(on X,Y) f(X, Y) | and(M = N, K = L, a = X)}", 
						false, 
						"{(on Y) f(a, Y) | M = N and K = L}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"{(on X in {a,b,c}) [if p(X) then 1 else 0] | X = a }", 
						false, 
						"{ ([ if p(a) then 1 else 0 ]) }"),
				//
				// Basic: Simple argument cases for intensional multisets.
				// 
				// if C is false"
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X) f(X) | false}}", 
						false, 
						"{}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X) f(X) | a = b}}", 
						false, 
						"{}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{f(X) | false}}", 
						false, 
						"{}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{f(X) | a = b}}", 
						false, 
						"{}"),
				// if I is empty
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{f(X) | a = a}}", 
						false, 
						"{{f(X)}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{f(X) | Y = Z}}", 
						false, 
						"if Y = Z then {{f(X)}} else {}"),
				// if C is (C' and i = Beta) for i an index in I
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X) f(X) | Z = X}}", 
						false, 
						"{{f(Z)}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X) | Z = X}}", 
						false, 
						"{{(on Y) f(Z)}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | Z = X}}", 
						false, 
						"{{(on Y) f(Z, Y)}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | a = X}}", 
						false, 
						"{{(on Y) f(a, Y)}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | (M = N) and (a = X)}}", 
						false, 
						"{{(on Y) f(a, Y) | M = N}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | (a = X) and (M = N)}}", 
						false, 
						"{{(on Y) f(a, Y) | M = N}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | and(a = X, M = N, K = L)}}", 
						false, 
						"{{(on Y) f(a, Y) | M = N and K = L}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | and(M = N, a = X, K = L)}}", 
						false, 
						"{{(on Y) f(a, Y) | M = N and K = L}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{(on X,Y) f(X, Y) | and(M = N, K = L, a = X)}}", 
						false, 
						"{{(on Y) f(a, Y) | M = N and K = L}}"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"{{(on X in {a,b,c}) [if p(X) then 1 else 0] | X = a }}", 
						false, 
						"{{ ([ if p(a) then 1 else 0 ]) }}"),
				//
				// Basic: Illegal Argument Exceptions
				//
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"1", 
						true, 
						"N/A"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{a}", 
						true, 
						"N/A"),
				new IntensionalSimplificationTestData(Expressions.TRUE.toString(), null,
						"{{a, a, b}}", 
						true, 
						"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testProductMessageAndProductFactor() {
		class PMAPFTestData extends TestData {
			private String m, Pi, beingComputed; 
			private Expression exprM, exprPi, exprBeingComputed;
			
			public PMAPFTestData(String contextualConstraint, Model model, String m, String Pi, String beingComputed, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.m             = m;
				this.Pi            = Pi;
				this.beingComputed = beingComputed;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprM             = parse(m);
				this.exprPi            = parse(Pi);
				this.exprBeingComputed = parse(beingComputed);
				return Times.make(Arrays.asList(new Expression[] {exprM, exprPi}));
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_prod_m_and_prod_factor,
							LPIUtil.argForProductMessageAndProductFactorRewriteCall(exprM, exprPi, exprBeingComputed));
			}
		};
		TestData[] tests = new TestData[] {
				//
				// Basic: Straight forward cases
				//
				new PMAPFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"if r then 1 else 0", 
						"product({{ (on F in { [if r and (p(X) or not p(X)) then 0.2 else 0.3] })  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false, 
						"if r then 1 else 0"
						),	
				new PMAPFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"if r then 0.7 else 0.3", 
						"product({{ (on F in { [if r and (p(X) or not p(X)) then 0.2 else 0.3] })  (message to [r] from F) }} )", 
						LPIUtil.createNewBeingComputedExpression().toString(),
						false, 
						"if r then 0.28 else 0.18" // this is (if r then 0.7 else 0.3)*(if r then 0.4 else 0.6)
				),	
				//
				// Basic: Illegal Argument Exceptions
				new PMAPFTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"if r then 0.7 else 0.3", 
						"if r then 0.7 else 0.3", // invalid argument 
						LPIUtil.createNewBeingComputedExpression().toString(),
						true, 
						"N/A"
				),	
		};
		
		perform(tests);
	}
	
	@Test
	public void testNeighboursFactor() {
		class NeighboursFactorTestData extends TestData {
			private String neighF; 
			private Expression exprNeighF;
			
			public NeighboursFactorTestData(String contextualConstraint, Model model, String neighF, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.neighF = neighF;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprNeighF = parse(neighF);
				return this.exprNeighF;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_neigh_f, exprNeighF);
			}
		};
		
		TestData[] tests = new TestData[] {
				//
				// Basic: Tests brought over from ALBPTest.testNeighborsOfFactor()
				//
				new NeighboursFactorTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"Neigh([if p(X) and q(X,Y) then 1 else 0])", 
						false, 
						"{ ([ p(X) ]), ([ q(X, Y) ]) }"),	
				new NeighboursFactorTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"Neigh([if p(X) and p(Y) and r then 1 else 0])", 
						false, 
						"if X = Y then { ([ p(X) ]), ([ r ]) } else { ([ p(X) ]), ([ p(Y) ]), ([ r ]) }"),	
				new NeighboursFactorTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"Neigh([if a(X) and b(X,Y) then 1 else 0])", 
						false, 
						"{ }"),	
				new NeighboursFactorTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
						"Neigh([if q(X) then if p then 1 else 0 else if p then 0 else 1])", 
						false, 
						// Ensures duplicates are not returned.
						"{ ([ q(X) ]), ([ p ]) }"),	
				//
				// Basic: Illegal Argument Exceptions
				//
				new NeighboursFactorTestData(Expressions.TRUE.toString(), new TrivialPQR(), 
						"if p(X) and q(X,Y) then 1 else 0", 
						true, 
						"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testNormalize() {
		class NormalizeTestData extends TestData {
			private String V, E; 
			private Expression exprV, exprE;
			
			public NormalizeTestData(String contextualConstraint, Model model, String V, String E, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.V = V;
				this.E = E;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprV = parse(V);
				this.exprE = parse(E);
				return Expressions.apply("normalize", exprV, exprE);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_normalize_message, LPIUtil.argForNormalizeRewriteCall(exprV, exprE));
			}
		};
		
		TestData[] tests = new TestData[] {
				//
				// Basic: Straight forward cases
				//
				// This rewriter assumes that messages have logical variables conditions
				// on top, and separated from, random variable conditions.
				// It also assumes that random variables are always instances of the
				// random variable on which to normalize
				// (that is, other random variables are not supposed to be present).
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialLoopyPQandb(),
						"[p(X)]",
						"if something then if p(X) then 10 else 90 else 90",
						false,
						"if something then if p(X) then 0.1 else 0.9 else 0.5"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialLoopyPQandb(),
						"[p(X)]",
						"1" /* could be any constant */,
						false,
						"0.5"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialLoopyPQandb(),
						"[p(X)]",
						"if something then 10 else 90",
						false,
						"0.5"), // we get 0.5 because the expression is constant in p(X)
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialLoopyPQandb(),
						"[p(X)]",
						"if X = a then if p(a) then 1 else 9 else if p(X) then 2 else 8",
						false,
						"if X = a then if p(a) then 0.1 else 0.9 else if p(X) then 0.2 else 0.8"),
				//
				// Basic: Illegal Argument Exceptions
				// 
		};
		
		perform(tests);
	}
	
	@Test
	public void testBeliefForNonLoopyModels() {
		class BeliefTestData extends TestData {
			private String belief; 
			private Expression exprBelief;
			private Map<Object, Object> globalObjects;
			private LBPConfiguration.BeliefPropagationUpdateSchedule schedule = LBPConfiguration.BeliefPropagationUpdateSchedule.SYNCHRONOUS;
			
			public BeliefTestData(String contextualConstraint, Model model, String belief, boolean illegalArgumentTest, String expected) {
				this(contextualConstraint, model, belief, null, illegalArgumentTest, expected);
			};
			
			public BeliefTestData(String contextualConstraint, Model model, String belief, Map<Object, Object> globalObjects, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.belief = belief;
				this.globalObjects = globalObjects;
			};
			
			public void setUpdateSchedule(LBPConfiguration.BeliefPropagationUpdateSchedule schedule) {
				this.schedule = schedule;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprBelief = parse(belief);
				return this.exprBelief;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				if (globalObjects != null) {
					process.getGlobalObjects().putAll(globalObjects);
				}
				LBPConfiguration configuration = LBPFactory.newLBPConfiguration();
				configuration.setBeliefPropagationUpdateSchedule(schedule);
				RewritingProcess lbpProcess = LBPFactory.newLBPProcess(process.getRootExpression(), configuration, process);
				Expression belief = lbpProcess.rewrite(LBPRewriter.R_belief, exprBelief);
				Expression roundedBelief = Expressions.roundToAGivenPrecision(belief, 9, process);
				return roundedBelief;
			}
		}
		
		BeliefTestData[] tests = new BeliefTestData[] {
				
				// 
				// Test on model defined with high-level syntax
				// 		
						new BeliefTestData(Expressions.TRUE.toString(),
						Model.fromRules(
								"Model Name: Epidemic Sick",
								"Model Description: Epidemic Sick Example.",
								"sort People: 10, bob, dave, rodrigo, ciaran;\n" + 
								"// RANDOM VARIABLE DECLARATIONS:\n" + 
								"random epidemic: Boolean;\n" + 
								"random sick: People -> Boolean;\n" + 
								"random fever: People -> Boolean;\n" + 
								"random rash: People -> Boolean;\n" + 
								"random notAtWork: People -> Boolean;\n" + 
								"// RULES\n" + 
								"epidemic 0.001;" +
								"if epidemic then sick(X) 0.6 else sick(X) 0.05;\n" + 
								"if sick(X) then fever(X) 0.7 else fever(X) 0.01;\n" + 
								"if sick(X) then rash(X) 0.6 else rash(X) 0.07;\n" + 
								"if sick(X) then notAtWork(X) 0.8 else notAtWork(X) 0.05;\n" +
								"// EVIDENCE\n" +
								"notAtWork(bob);"),
						"belief([sick(X)])", 
						false, 
						"if X = bob then (if sick(bob) then 0.460002844 else 0.539997156) else (if sick(X) then 0.0531281103 else 0.94687189)"
						 ),

				
				new BeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"union("
										+ "{{(on X in People) [if sick(X) then 0.4 else 0.6]}}, "
										+ "{{ [if sick(john) then 1 else 0] }}"
										+ ")",
								"sick/1"
						),
						"belief([sick(X)])",
						false,
						"if X = john then if sick(john) then 1 else 0 else if sick(X) then 0.4 else 0.6"),
	
				new BeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"union("
										+ "{ [if epidemic then 0.1 else 0.9] }, "
										+ "{{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}, "
										+ "{{ (on X in People) [if sick(X) then 1 else 0] | X  = person1 or  X  = person2 or  X  = person3 }},"
										+ "{{ (on X in People) [if sick(X) then 0 else 1] | X != person1 and X != person2 and X != person3 }}"
										+ ")",
								"epidemic/0", "sick/1"
						),
						"belief([epidemic])",
						// Util.map(parse("|People|"), Expressions.createSymbol(20)),
						false,
						// Note: old R_basic result:
						// "if epidemic then (0.0064 * 0.6 ^ (| People | - 3)) / (0.0064 * 0.6 ^ (| People | - 3) + 9E-7 * 0.99 ^ (| People | - 3)) else (9E-7 * 0.99 ^ (| People | - 3)) / (0.0064 * 0.6 ^ (| People | - 3) + 9E-7 * 0.99 ^ (| People | - 3))"
						"if epidemic then 0.995339619 else 0.00466038114"),
	
				new BeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"union("
										+ "{ [if epidemic then 0.1 else 0.9] }, "
										+ "{{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}, "
										+ "{{ (on X in People) [if sick(X) then 1 else 0] | X  = person1 or  X  = person2 or  X  = person3 }},"
										+ "{{ (on X in People) [if sick(X) then 0 else 1] | X != person1 and X != person2 and X != person3 }}" +
										")",
								"epidemic/0", "sick/1"
						),
						"belief([epidemic])",
						Util.map(parse("| People |"), Expressions.makeSymbol(20)),
						false, 
						"if epidemic then 0.588128460 else 0.411871540"),
	
				//		
				// Basic: Straight forward non-loopy (i.e. exact) cases
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPQWithPriors(), 
						"belief([p(X)])", 
						false, 
						"if p(X) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPQWithPriors(), 
						"belief([q(X)])", 
						false, 
						"if q(X) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialThereExistsPQWithPriors(), 
						"belief(['there exists Y : p(X0, Y)'(X)])", 
						false, 
						"if 'there exists Y : p(X0, Y)'(X) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialThereExistsPQWithPriors(), 
						"belief(['there exists Y : q(X0, Y)'(X)])", 
						false, 
						"if 'there exists Y : q(X0, Y)'(X) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new WeightedPQWithPriors(), 
						"belief([p(X)])", 
						false, 
						"if p(X) then 0.223300971 else 0.776699029"),
				new BeliefTestData(Expressions.TRUE.toString(), new WeightedPQWithPriors(), 
						"belief([q(X)])", 
						false, 
						"if q(X) then 0.320388350 else 0.679611650"),
				new BeliefTestData(Expressions.TRUE.toString(), new WeightedThereExistsPQWithPriors(), 
						"belief(['there exists Y : p(X0, Y)'(X)])", 
						false, 
						"if 'there exists Y : p(X0, Y)'(X) then 0.223300971 else 0.776699029"),
				new BeliefTestData(Expressions.TRUE.toString(), new WeightedThereExistsPQWithPriors(), 
						"belief(['there exists Y : q(X0, Y)'(X)])", 
						false, 
						"if 'there exists Y : q(X0, Y)'(X) then 0.320388350 else 0.679611650"),
	
				// From ALBPTest.testIntensionalFanIn()
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
						"belief([ q(a1) ])", 
						false, 
						"if q(a1) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
						"belief([ q(a2) ])", 
						false, 
						"if q(a2) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
						"belief([ q(a3) ])", 
						false,				
						"if q(a3) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), 
						new TrivialPQPeoplea1Anda2(), 
						"belief([ q(X) ])", 
						false, 
						// Note: old R_basic result:
						// "if X = a1 then if q(a1) then 1 else 0 else if X = a2 then if q(a2) then 1 else 0 else if q(X) then 1 else 0"
						"if X = a1 then if q(a1) then 1 else 0 else if X = a2 then if q(a2) then 1 else 0 else if q(X) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
						"belief([ p ])", 
						false, 
						"if p then 1 else 0"),
				
				// From ALBPTest.testBelief()
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"belief([tall(X)])", 
						false, 
						"0.5"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"belief([tall(a1)])", 
						false, 
						"0.5"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"belief([american(X)])", 
						false, 
						"if american(X) then 0.687500000 else 0.312500000"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"belief([american(a1)])", 
						false, 
						"if american(a1) then 0.687500000 else 0.312500000"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"belief([intelligent(X)])", 
						false, 
						"0.5"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"belief([intelligent(a1)])", 
						false, 
						"0.5"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"belief([unintelligent(X)])", 
						false, 
						"0.5"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"belief([unintelligent(a1)])", 
						false, 
						"0.5"),
						
				// From ALBPTest.testExponentiatedLifted()
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"belief([ epidemic ])", 
						false, 
						// Note: old R_basic result:
						// "if epidemic then 1 / (1 + 0.8 ^ (|People| - 1)) else 0.8 ^ (|People| - 1) / (1 + 0.8 ^ (|People| - 1))"
						"if epidemic then 0.881664935 else 0.118335065"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"belief([ sick(X) ])", 
						false, 
						// Note: old R_formula_simplification result before R_normalize used instead
						// Difference is because | People | -> 10 and new result is this expression calculated correctly with that.
						// "if X != bob then if sick(X) then (0.4 * 0.8 ^ (|People| - 2) + 0.6) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else (0.4 * 0.8 ^ (|People| - 2) + 0.4) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else 0.5"
						"if X = bob then 0.500000000 else (if sick(X) then 0.588166494 else 0.411833506)"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"belief([ sick(ann) ])", 
						false, 
						// Note: old R_basic result:
						// "if sick(ann) then (0.4 * 0.8 ^ (|People| - 2) + 0.6) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else (0.4 * 0.8 ^ (|People| - 2) + 0.4) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2))"
						"if sick(ann) then 0.588166494 else 0.411833506"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"belief([ sick(bob) ])", 
						false, 
						"0.5"),
						
				// From ALBPTest.testExponentiatedLifted2()
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich() , 
						"belief([rich(bob)])", 
						false,
						// Note: old R_basic result:
						// "if rich(bob) then (1 + 2 ^ |People|) ^ |Treasure| / ((1 + 2 ^ |People|) ^ |Treasure| + 1) else 1 / ((1 + 2 ^ |People|) ^ |Treasure| + 1)"
						"if rich(bob) then 1 else 0.000000000000000000000000000000781198402"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
						"belief([rich(X)])", 
						false,
						// Note: old R_basic result
						// "if rich(X) then (1 + 2 ^ |People|) ^ |Treasure| / ((1 + 2 ^ |People|) ^ |Treasure| + 1) else 1 / ((1 + 2 ^ |People|) ^ |Treasure| + 1)"
						"if rich(X) then 1 else 0.000000000000000000000000000000781198402"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
						"belief([gaveTreasureTo(X,Z,Y)])", 
						false, 
						// Note: old R_basic and R_formula_simlification result:
						// "if |People| > 0 then if gaveTreasureTo(X, Z, Y) then (2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else if gaveTreasureTo(X, Z, Y) then (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1))"
						"if gaveTreasureTo(X, Z, Y) then 0.499512195 else 0.500487805"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
						"belief([owns(X,Y)])", 
						false,
						// Note: old R_basic and R_formula_simlification result:
						//"if owns(X, Y) then (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1))"
						"if owns(X, Y) then 0.999024390 else 0.000975609756"),
						
				// From ALBPTest.testQueryVariableSplitting()
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialSickbob(), 
						"belief([sick(Person)])", 
						false, 
						"if Person = bob then if sick(bob) then 0.8 else 0.2 else 0.5"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialSickbob(), 
						"belief([sick(bob)])", 
						false, 
						"if sick(bob) then 0.8 else 0.2"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialSickbob(), 
						"belief([sick(ann)])", 
						false, 
						"0.5"),
				
				// From ALBPTest.testMultiLevelMessagePassing()
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
						"belief([smoker(Person)])", 
						false, 
						// Note: old R_basic result:
						// "if Person = bob then if smoker(bob) then 0.15 else 0.85 else if smoker(Person) then 0.1 else 0.9"
						"if Person = bob then if smoker(bob) then 0.150943396 else 0.849056604 else if smoker(Person) then 0.100000000 else 0.900000000"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
						"belief([smoker(bob)])", 
						false, 
						"if smoker(bob) then 0.150943396 else 0.849056604"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
						"belief([sick(X)])", 
						false, 
						// Note: old R_basic result:
						// "if X = bob then if sick(bob) then 1 else 0 else if sick(X) then 0.53 else 0.47"
						// i.e. no constraint applier used, so sick(bob) is sick(X)
						"if X = bob then if sick(bob) then 1 else 0 else if sick(X) then 0.530000000 else 0.470000000"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
						"belief([sick(bob)])", 
						false, 
						"if sick(bob) then 1 else 0"),
				
				// From ALBPTest.testEpidemic()
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
						"belief([ epidemic ])", 
						false, 
				      "if epidemic then 0.28 else 0.72"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
						"belief([ sick(X) ])", 
						false,
						"if X = bob then if sick(bob) then 1 else 0 else if sick(X) then 0.34 else 0.66"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
						"belief([ sick(ann) ])", 
						false,
						"if sick(ann) then 0.34 else 0.66"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
						"belief([ sick(bob) ])", 
						false, 
						"if sick(bob) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
						"belief([ epidemic ])", 
						false, 
						// Note: old R_basic result:
						// "if epidemic then (0.1 * 0.7 ^ |People|) / (0.1 * 0.7 ^ |People| + 0.9 * 0.2 ^ |People|) else (0.9 * 0.2 ^ |People|) / (0.1 * 0.7 ^ |People| + 0.9 * 0.2 ^ |People|)"
						"if epidemic then 0.999967375 else 0.0000326248029"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
						"belief([ sick(X) ])", 
						false, 
						"if sick(X) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
						"belief([ sick(ann) ])", 
						false, 
						"if sick(ann) then 1 else 0"),
				
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"belief([ epidemic ])", 
						false,
						// Note: old R_basic result:
						// "if epidemic then (0.03 * 0.7 ^ (|People| - 3)) / (0.03 * 0.7 ^ (|People| - 3) + 0.72 * 0.2 ^ (|People| - 3)) else (0.72 * 0.2 ^ (|People| - 3)) / (0.03 * 0.7 ^ (|People| - 3) + 0.72 * 0.2 ^ (|People| - 3))"
						"if epidemic then 0.996283639 else 0.00371636130"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"belief([ sick(X) ])", 
						false,
						// Note: old R_basic result:
						// "if X != bob and X != mary and X != john then if sick(X) then 1 else 0 else if X = bob then if sick(bob) then 0 else 1 else if sick(X) then (0.14 * 0.20 ^ (| People | - 3) + 0.021 * 0.70 ^ (| People | - 3)) / (0.14 * 0.20 ^ (| People | - 3) + 0.021 * 0.70 ^ (| People | - 3) + 0.58 * 0.20 ^ (| People | - 3) + 0.0090 * 0.70 ^ (| People | - 3)) else (0.58 * 0.20 ^ (| People | - 3) + 0.0090 * 0.70 ^ (| People | - 3)) / (0.14 * 0.20 ^ (| People | - 3) + 0.021 * 0.70 ^ (| People | - 3) + 0.58 * 0.20 ^ (| People | - 3) + 0.0090 * 0.70 ^ (| People | - 3))"
						// Note: the previous equation at the end would have given:
	                    // 'if sick(X) then 0.69812117289589 else 0.30187882710411'
	                    // when calculated. We now calculate:
						// 'if sick(X) then 0.698141819      else 0.301858181'
						// for the equivalent branch. The difference is because the old
						// result is rounded to two decimal places and the difference
						// in the answers is because of this.
						"if X != bob and X != mary and X != john then if sick(X) then 1 else 0 else if X = bob then if sick(bob) then 0 else 1 else if sick(X) then 0.698141819 else 0.301858181"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"belief([ sick(ann) ])", 
						false, 
						"if sick(ann) then 1 else 0"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"belief([ sick(bob) ])", 
						false, 
						"if sick(bob) then 0 else 1"),
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"belief([ sick(mary) ])", 
						false,
						// Note: old R_basic result:
						// "if sick(mary) then (0.14 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3)) / (0.14 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.58 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) else (0.58 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) / (0.14 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.58 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3))"
						"if sick(mary) then 0.698141819 else 0.301858181"),
				
				new BeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"partition("
								+ "{{(on X in People) [if smokes(X) then if cancer(X) then 0.9 else 0.1 else 1] }}, "
								+ "{{ [if smokes(john) then 1 else 0] }}, "
								+ "{{ (on X in People) [if smokes(X) then 0.7 else 0.3] }})",
								"smokes/1", "cancer/1"
								/**
								 * The point of this example is to test short-circuiting.
								 * When we query cancer(john), messages about smokes(john) comes from two different parfactors.
								 * The first one, [if smokes(john) then 1 else 0], is deterministic and makes the second message irrelevant.
								 * Therefore, this second message does not need to be computed at all.
								 */
						),
						"belief([cancer(john)])",
						false, 
						"if cancer(john) then 0.9 else 0.1"),
								
				// From ALBPTest.testCSI()
				new BeliefTestData(Expressions.TRUE.toString(), new TrivialSunnyAvailableCanPlayWith(), 
						"belief([canPlayWith(X)])", 
						false,
						"if canPlayWith(X) then 0 else 1"),
				
				// A model that looks loopy but isn't.
				new BeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"union(" +
								"{{ (on X) [if p(X) and q(X) then 2 else 3]   | X  = a }}," +
								"{{ (on X) [if q(X) and p(X) then 10 else 20] | X != a }}" +
								")",
								"p/1", "q/1"
						), 
						"belief([p(W)])", 
						false, 
						"if W = a then if p(a) then 0.454545455 else 0.545454545 else if p(W) then 0.428571429 else 0.571428571"),				
				// 
				// Test on model defined with high-level syntax
				// 		
						new BeliefTestData(Expressions.TRUE.toString(),
						Model.fromRules(
								"Model Name: Epidemic Sick",
								"Model Description: Epidemic Sick Example.",
								"sort People: 10, bob, dave, rodrigo, ciaran;\n" + 
								"// RANDOM VARIABLE DECLARATIONS:\n" + 
								"random epidemic: Boolean;\n" + 
								"random sick: People -> Boolean;\n" + 
								"random fever: People -> Boolean;\n" + 
								"random rash: People -> Boolean;\n" + 
								"random notAtWork: People -> Boolean;\n" + 
								"// RULES\n" + 
								"epidemic 0.001;" +
								"if epidemic then sick(X) 0.6 else sick(X) 0.05;\n" + 
								"if sick(X) then fever(X) 0.7 else fever(X) 0.01;\n" + 
								"if sick(X) then rash(X) 0.6 else rash(X) 0.07;\n" + 
								"if sick(X) then notAtWork(X) 0.8 else notAtWork(X) 0.05;\n" +
								"// EVIDENCE\n" +
								"notAtWork(bob);"),
						"belief([sick(X)])", 
						false, 
						"if X = bob then (if sick(bob) then 0.460002844 else 0.539997156) else (if sick(X) then 0.0531281103 else 0.94687189 )"
						 ),
				// 
				// Basic: Illegal Argument Exceptions
				// 		
		};
		
		// Run non-loopy tests for each kind of schedule currently supported
		LBPConfiguration.BeliefPropagationUpdateSchedule[] schedules = new LBPConfiguration.BeliefPropagationUpdateSchedule[] {
				LBPConfiguration.BeliefPropagationUpdateSchedule.ASYNCHRONOUS_INDIVIDUAL_BASED_CYCLE_DETECTION,
				LBPConfiguration.BeliefPropagationUpdateSchedule.SYNCHRONOUS,
		};
		
		for (LBPConfiguration.BeliefPropagationUpdateSchedule schedule : schedules) {
			System.out.println("---- RUNNING TESTS UNDER SCHEDULE : "+schedule);
			for (BeliefTestData beliefTestData : tests) {
				beliefTestData.setUpdateSchedule(schedule);
			}
			perform(tests);
		}
	}

	@Test
	public void testBeliefForNonLoopyModelswithUnknownTypeSizes() {
		class BeliefUnknownSizeTestData extends TestData {
			private String belief; 
			private Expression exprBelief;
			
			public BeliefUnknownSizeTestData(String contextualConstraint, Model model, String belief, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.belief = belief;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprBelief = parse(belief);
				return this.exprBelief;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression roundedBelief = null;
				
				Expression belief = process.rewrite(LBPRewriter.R_belief, exprBelief);
				roundedBelief = Expressions.roundToAGivenPrecision(belief, 9, process);
					
				return roundedBelief;
			}
		};
		
		TestData[] tests = new TestData[] {

				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), 
						new Model(
								"union("
										+ "{ [if epidemic then 0.1 else 0.9] }, "
										+ "{{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}, "
										+ "{{ (on X in People) [if sick(X) then 1 else 0] | X  = person1 or  X  = person2 or  X  = person3 }},"
										+ "{{ (on X in People) [if sick(X) then 0 else 1] | X != person1 and X != person2 and X != person3 }}" +
										")",
								"epidemic/0", "sick/1"
						),
						"belief([epidemic])",
						false,
						"if epidemic then (0.0064 * 0.6 ^ (| People | - 3)) / (0.0064 * 0.6 ^ (| People | - 3) + 9E-7 * 0.99 ^ (| People | - 3)) else (9E-7 * 0.99 ^ (| People | - 3)) / (0.0064 * 0.6 ^ (| People | - 3) + 9E-7 * 0.99 ^ (| People | - 3))"
						// calculated | type(X) | = 10 :
						// if epidemic then 0.995339619 else 0.00466038114
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"belief([ epidemic ])", 
						false, 
						"if epidemic then 1 / (1 + 0.8 ^ (|People| - 1)) else 0.8 ^ (|People| - 1) / (1 + 0.8 ^ (|People| - 1))"
						// calculated | type(X) | = 10 :
						// if epidemic then 0.881664935 else 0.118335065
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"belief([ sick(X) ])", 
						false, 
						"if X = bob then 0.5 else if sick(X) then (0.4 * 0.8 ^ (|People| - 2) + 0.6) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else (0.4 * 0.8 ^ (|People| - 2) + 0.4) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2))"
						// calculated | type(.) | = 10 :
						// if X != bob then if sick(X) then 0.588166494 else 0.411833506 else 0.500000000
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"belief([ sick(ann) ])", 
						false, 
						"if sick(ann) then (0.4 * 0.8 ^ (|People| - 2) + 0.6) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else (0.4 * 0.8 ^ (|People| - 2) + 0.4) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2))"
						// calculated | type(.) | = 10 :
						// if sick(ann) then 0.588166494 else 0.411833506
						),						
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich() , 
						"belief([rich(bob)])", 
						false,
						"if rich(bob) then (1 + 2 ^ |People|) ^ |Treasure| / ((1 + 2 ^ |People|) ^ |Treasure| + 1) else 1 / ((1 + 2 ^ |People|) ^ |Treasure| + 1)"
						// calculated | type(.) | = 10 :
						// if rich(bob) then 1 else 0.000000000000000000000000000000781198402
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
						"belief([rich(X)])", 
						false,
						"if rich(X) then (1 + 2 ^ |People|) ^ |Treasure| / ((1 + 2 ^ |People|) ^ |Treasure| + 1) else 1 / ((1 + 2 ^ |People|) ^ |Treasure| + 1)"
						// calculated | type(.) | = 10 :
						// if rich(X) then 1 else 0.000000000000000000000000000000781198402
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
						"belief([gaveTreasureTo(X,Z,Y)])", 
						false, 
						"if gaveTreasureTo(X, Z, Y) then (2 ^ (| People | - 1) * (1 + 2 ^ | People |) ^ (| Treasure | - 1)) / (2 ^ (| People | - 1) * (1 + 2 ^ | People |) ^ (| Treasure | - 1) + 1 + (1 + 2 ^ | People |) ^ (| Treasure | - 1) + 2 ^ (| People | - 1) * (1 + 2 ^ | People |) ^ (| Treasure | - 1)) else (1 + (1 + 2 ^ | People |) ^ (| Treasure | - 1) + 2 ^ (| People | - 1) * (1 + 2 ^ | People |) ^ (| Treasure | - 1)) / (2 ^ (| People | - 1) * (1 + 2 ^ | People |) ^ (| Treasure | - 1) + 1 + (1 + 2 ^ | People |) ^ (| Treasure | - 1) + 2 ^ (| People | - 1) * (1 + 2 ^ | People |) ^ (| Treasure | - 1))"
						// calculated | type(.) | = 10 :
						// if gaveTreasureTo(X, Z, Y) then 0.499512195 else 0.500487805
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
						"belief([owns(X,Y)])", 
						false,
						"if owns(X, Y) then (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1))"
						// calculated | type(.) | = 10 :
						// if owns(X, Y) then 0.999024390 else 0.000975609756
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
						"belief([ epidemic ])", 
						false, 
						"if epidemic then (0.1 * 0.7 ^ |People|) / (0.1 * 0.7 ^ |People| + 0.9 * 0.2 ^ |People|) else (0.9 * 0.2 ^ |People|) / (0.1 * 0.7 ^ |People| + 0.9 * 0.2 ^ |People|)"
						// calculated | type(.) | = 10 :
						// if epidemic then 0.999967375 else 0.0000326248029
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"belief([ epidemic ])", 
						false,
						 "if epidemic then (0.03 * 0.7 ^ (|People| - 3)) / (0.03 * 0.7 ^ (|People| - 3) + 0.72 * 0.2 ^ (|People| - 3)) else (0.72 * 0.2 ^ (|People| - 3)) / (0.03 * 0.7 ^ (|People| - 3) + 0.72 * 0.2 ^ (|People| - 3))"
						// calculated | type(.) | = 10 :
						// if epidemic then 0.996283639 else 0.00371636130
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"belief([ sick(X) ])", 
						false,
						"if X != bob and X != mary and X != john then if sick(X) then 1 else 0 else (if X = bob then if sick(bob) then 0 else 1 else (if sick(X) then (0.144 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3)) / (0.144 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.576 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) else (0.576 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) / (0.144 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.576 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3))))"
						// calculated | type(.) | = 10 :
						// if X != bob and X != mary and X != john then if sick(X) then 1 else 0 else if X = bob then if sick(X) then 0 else 1 else if X = mary or X = john then if sick(X) then 0.698141819 else 0.301858181 else if sick(X) then 0.693556236 else 0.306443764
						),
				new BeliefUnknownSizeTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"belief([ sick(mary) ])", 
						false,
						"if sick(mary) then (0.144 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3)) / (0.144 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.576 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) else (0.576 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) / (0.144 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.576 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3))"
						// calculated | type(.) | = 10 :
						// if sick(mary) then 0.698141819 else 0.301858181
						),
		};
		
		Configuration.setProperty(PRAiSEConfiguration.KEY_MODEL_ALL_TYPE_SIZES_KNOWN, "false");	
		perform(tests);
	}
	
	@Test
	public void testExtractPreviousMessageSets() {
		class ExtractPreviousMessageSetsTestData extends TestData {
			private String beliefExpansion;
			private Expression exprBeliefExpansion;
			
			public ExtractPreviousMessageSetsTestData(String contextualConstraint, Model model, String beliefExpansion, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.beliefExpansion      = beliefExpansion;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprBeliefExpansion = parse(beliefExpansion);
				
				return this.exprBeliefExpansion;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return process.rewrite(LBPRewriter.R_extract_previous_msg_sets, exprBeliefExpansion);
			}
		}
		
		TestData[] tests = new TestData[] {
				//
				// Basic:
				// 
				// An intensional set without indices should be returned
				new ExtractPreviousMessageSetsTestData("X = a",
						new TrivialPQ(), 
						"previous message to [p(a)] from [ Beta ]",
						false,
						"{ (on ) ([p(a)], [Beta]) | true }"),
				// An intensional set without indices should be returned
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQ(), 
						"previous message to [p(X)] from [ Alpha ]",
						false,
						"{ (on X) ([p(X)], [Alpha]) }"),
				// An intensional set should be returned
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQ(), 
						"{ (on X) previous message to [p(X)] from [ Alpha ] }",
						false,
						"{ (on X) ([p(X)], [Alpha]) }"),
				// An intensional set should be returned
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQ(), 
						"previous message to [p(X)] from [ if p(Y) then 1 else 0 ]",
						false,
						"{ (on X, Y) ([p(X)], [if p(Y) then 1 else 0]) }"),
				// Example from pseudo-code
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQ(), 
						"if X != a then (previous message to [p(X)] from [ Alpha ]) else (previous message to [p(a)] from [ Beta ])",
						false,
						"{ (on X) ([p(X)], [Alpha]) | X != a} union { (on ) ([p(a)], [Beta]) | true }"),
				// Variants of example from pseudo-code
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQ(), 
						"if X != a then (previous message to [p(X)] from [ Alpha ]) else 1",
						false,
						"{ (on X) ([p(X)], [Alpha]) | X != a }"),
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQ(), 
						"if X != a then 1 else (previous message to [p(a)] from [ Beta ])",
						false,
						"{ (on ) ([p(a)], [Beta]) | true }"),
				// More than 1 level of nesting
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQR(), 
						"if X != a " + 
				        "then (if Y != b then (previous message to [q(X, Y)] from [ Alpha1 ]) else 1) " + 
						"else (if Y != c then (previous message to [q(X, Y)] from [ Beta1 ])  else product({ (on Y) (previous message to [q(X, Y)] from [ Beta2 ]) } ) )",
						false,
						// subtle test, as the last-occurring Y is not the same as the free Y and needs to be properly shadowed
						"{ ( on X, Y ) ( ([ q(X, Y) ]), ([ Alpha1 ]) ) | X != a and Y != b } union { ( on Y ) ( ([ q(a, Y) ]), ([ Beta1 ]) ) | Y != c } union { ( on Y ) ( ([ q(a, Y) ]), ([ Beta2 ]) ) | true }"),		
				// Embedded as a term in an arithmetic expression
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQ(), 
						"if X != a then 1 else (1 + (previous message to [p(a)] from [ Beta ]))",
						false,
						"{ ( on ) ( ([ p(a) ]), ([ Beta ]) ) | true }"),	
				// Embedded as a term in an exponentiation expression
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQ(), 
						"if X != a then 1 else (previous message to [p(a)] from [ Beta ])^2",
						false,
						"{ ( on ) ( ([ p(a) ]), ([ Beta ]) ) | true }"),	
			    // Embedded a term in a product expression and sub conditional expression
				new ExtractPreviousMessageSetsTestData(Expressions.TRUE.toString(),
						new TrivialPQ(), 
						"product({ (on A, D) if A != a then 1 else (previous message to [p(a)] from [ Beta ]) | A != D and (X = A or X = D) and A = X })",
						false,
						"{ ( on ) ( ([ p(a) ]), ([ Beta ]) ) | true }"),	
				//
				// Basic: Contextual AbstractEqualityConstraint Tests
				//
				// Test a false contextual constraint
				new ExtractPreviousMessageSetsTestData(Expressions.FALSE.toString(),
						new TrivialPQ(), 
						"previous message to [p(a)] from [ Beta ]",
						false,
						Rewriter.FALSE_CONTEXTUAL_CONTRAINT_RETURN_VALUE.toString()),
		};
		
		perform(tests);
	}
	
	@Test
	public void testGetMessageExpansions() {
		class GetMessageExpansionsTestData extends TestData {
			private String                      msgSets;
			private Expression                  exprMsgSets;
			
			public GetMessageExpansionsTestData(String contextualConstraint, Model model, String msgSets, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.msgSets = msgSets;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprMsgSets = parse(msgSets);				
				return this.exprMsgSets;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression messageExpansions = LBPFactory.newMessageExpansions(process).getMessageExpansions(exprMsgSets, process);
				
				return messageExpansions;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic:
			//
			// empty msgSets
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{ }",
					false,
					"{ }"
					),
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{{ }}",
					false,
					"{ }"
					),
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"union()",
					false,
					"{ }"
					),
			// R_msg_to_v_f
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{ (on X) ([intelligent(X)], [if intelligent(X) <=> not unintelligent(X) then 1 else 0]) }",
					false,
					"{ (on X) ([intelligent(X)], [if intelligent(X) <=> not unintelligent(X) then 1 else 0], 1) }"
					),
			// R_msg_to_f_v
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPQWithPriors(),
					"{ (on X) ([if p(X) and q(X) then 1 else 0], [p(X)]) }",
					false,
					"{ (on X) ([if p(X) and q(X) then 1 else 0], [p(X)], (if p(X) then 0.2 else 0.8)) }"
					),		
			// union msgSets
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{ (on X) ([tall(X)], [if tall(X) then 2 else 8]) } union { (on X) ([tall(X)], [if tall(X) and american(X) then 7 else 1]) }",
					false,
					"{ (on X) ([tall(X)], [if tall(X) then 2 else 8], (if tall(X) then 2 else 8)) }" +
					" union " +
					"{ (on X) ([tall(X)], [if tall(X) and american(X) then 7 else 1], (if tall(X) then 8 else 2)) }"
					),
		    // duplicate union arguments
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{ (on X) ([tall(X)], [if tall(X) then 2 else 8]) } union { (on X) ([tall(X)], [if tall(X) and american(X) then 7 else 1]) } union { (on Y) ([tall(Y)], [if tall(Y) then 2 else 8]) }",
					false,
					"{ (on X) ([tall(X)], [if tall(X) then 2 else 8], (if tall(X) then 2 else 8)) }" +
					" union " +
					"{ (on X) ([tall(X)], [if tall(X) and american(X) then 7 else 1], (if tall(X) then 8 else 2)) }"
					),
			//
			// Basic: Illegal Argument Exceptions
			//
			// not a uniset
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{{ ([intelligent(a)], [if intelligent(a) <=> not unintelligent(a) then 1 else 0]) }}",
					true,
					"{}"
					),
			// not a singleton extensional uniset
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{ ([intelligent(a)], [if intelligent(a) <=> not unintelligent(a) then 1 else 0]), ([intelligent(b)], [if intelligent(b) <=> not unintelligent(b) then 1 else 0]) }",
					true,
					"{}"
					),
			// not a tuple
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{ [intelligent(a)] }",
					true,
					"{}"
					),
			// not a tuple pair
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{ tuple([intelligent(a)]) }",
					true,
					"{}"
					),
			// not a tuple with bracketed expressions
			new GetMessageExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPeopleAmericanTallIntelligentUnintelligent(),
					"{ (intelligent(a), unintelligent(b)) }",
					true,
					"{}"
					),
		};
		
		perform(tests);
	}
	
	@Test
	public void testIterateValuesUsingExpansions() {
		class IterateValuesUsingExpansionsTestData extends TestData {
			private String     msgValues, msgExpansions;
			private Expression exprMsgValues;
			private Expression exprMsgExpansions;
			
			public IterateValuesUsingExpansionsTestData(String contextualConstraint, Model model, String msgValues, String msgExpansions, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.msgValues     = msgValues;
				this.msgExpansions = msgExpansions;
			};
			
			@Override
			public Expression getTopExpression() {
				exprMsgValues     = parse(msgValues);
				exprMsgExpansions = parse(msgExpansions);
				Expression top = Tuple.make(exprMsgValues, exprMsgExpansions);
				
				return top;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression result = LBPFactory.newIterateValuesUsingExpansions(process).iterateValuesUsingExpansions(exprMsgValues, exprMsgExpansions, process);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			// 
			// Basic:
			new IterateValuesUsingExpansionsTestData(Expressions.TRUE.toString(),
					new TrivialPQWithPArity2AndQArity1(),
					// msg values
					"{ (on X, Z) ( [p(X,Z)], [if p(X,Z) and q(Z) then 1 else 0], ( if p(X,Z) then 1 else 0 ) ) | X != c }",
					// msg expansions
					"{ (on X, Z) ( [p(X,Z)], [if p(X,Z) and q(Z) then 1 else 0], ( if Z != d then (previous message to [p(X,Z)] from [if p(X,Z) and q(Z) then 1 else 0]) else 0 ) ) | X != c }",
					false,
					"{ (on X, Z) ( [p(X,Z)], [if p(X,Z) and q(Z) then 1 else 0], ( if Z = d then 0.5 else (if p(X,Z) then 1 else 0) ) ) | X != c }"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testUseValuesForPreviousMessages() {
		class UseValuesForPreviousMessagesTestData extends TestData {
			private String     expansion, msgValues;
			private Expression exprExpansion, exprMsgValues;
			
			public UseValuesForPreviousMessagesTestData(String contextualConstraint, Model model, String expansion, String msgValues, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.expansion = expansion;
				this.msgValues = msgValues;

			};
			
			@Override
			public Expression getTopExpression() {
				exprExpansion = parse(expansion);
				exprMsgValues = parse(msgValues);
				
				Expression top = Tuple.make(exprExpansion, exprMsgValues);
				
				return top;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression result = LBPFactory.newUseValuesForPreviousMessages(process).useValuesForPreviousMessages(exprExpansion, exprMsgValues, process);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
				//
				// Basic:
				//
				// Example 1:
				new UseValuesForPreviousMessagesTestData(Expressions.TRUE.toString(),
						new TrivialPQWithPArity2AndQArity1(),
						"if Y != d then (previous message to [p(b,Y)] from [if p(b,Y) and q(Y) then 1 else 0]) else 0",
						"{ (on X, Z) ( [p(X,a)], [if p(X,Z) and q(Z) then 1 else 0], ( if p(X,Z) then 1 else 0 ) ) | X != c }" +
						" union " +
						"{ (on X, Z) ( [p(X,Z)], [if p(X,Z) and q(Z) then 1 else 0], ( if p(X,Z) then 1 else 0 ) ) | X != c and Z != a}",
						false,
						"if Y = d then 0 else (if p(b, Y) then 1 else 0)"),
						//"if Y != d then if Y = a then if p(b, a) then 1 else 0 else (if p(b, Y) then 1 else 0) else 0"),
				// Example 2:
				new UseValuesForPreviousMessagesTestData(Expressions.TRUE.toString(),
						new TrivialPQWithPArity2AndQArity1(),
						"if Y != d then (previous message to [p(b,Y)] from [if p(b,Y) and q(Y) then 1 else 0]) else 0",
						"{ (on X, Z) ( [p(X,Z)], [if p(X,Z) and q(Z) then 1 else 0], ( if p(X,Z) then 1 else 0 ) ) | X != c }",
						false,
						"if Y = d then 0 else (if p(b,Y) then 1 else 0)"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testPickSingleElement() {
		class PickSingleElementTestData extends TestData {
			private String     intensionalSetString;
			private Expression intensionalSet;
			
			public PickSingleElementTestData(String contextualConstraint, Model model, String intensionalSet, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.intensionalSetString = intensionalSet;

			};
			
			@Override
			public Expression getTopExpression() {
				intensionalSet = parse(intensionalSetString);
				return intensionalSet;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression result = PickSingleElement.pickSingleElement(intensionalSet, process);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
				//
				// Basic:
				//
				new PickSingleElementTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"{ ([p(a)], [if p(a) then 1 else 0], 1) | true }",
						false,
						"([p(a)], [if p(a) then 1 else 0], 1)"
						),
				new PickSingleElementTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"{ (on X) ([p(X)], [if p(X) then 1 else 0], 1) | X = a}",
						false,
						"([p(a)], [if p(a) then 1 else 0], 1)"
						),
				new PickSingleElementTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"{(on X, Y) ([p(X)], [if p(X) and q(Y) then 1 else 0], 1) | X = a and Y = b}",
						false,
						"([p(a)], [if p(a) and q(b) then 1 else 0], 1)"
						),
				// Expression is free of the index expressions so should
				// be selected straight away
				new PickSingleElementTestData(Expressions.TRUE.toString(),
						new TrivialSickbob(),
						"{ ( on X' in People ) (if sick(X) then 1 else 0) | X = X' }",
						false,
						"if sick(X) then 1 else 0"
						),
				// Ensure the free variable is selected
				new PickSingleElementTestData(Expressions.TRUE.toString(),
						new TrivialSickbob(),
						"{ ( on X' in People ) (if sick(X') then 1 else 0) | X = X' }",
						false,
						"if sick(X) then 1 else 0"
						),
				// Ensure variables take precedence over constants.
				new PickSingleElementTestData(Expressions.TRUE.toString(),
						new TrivialSickbob(),
						"{ ( on X' in People ) (if sick(X') then 1 else 0) | X = X' = person1 }",
						false,
						"if sick(X) then 1 else 0"
						// "if sick(person1) then 1 else 0"
						),
				// Ensure the free variable X is selected from the common set of disjuncts
				new PickSingleElementTestData(Expressions.TRUE.toString(),
						new TrivialSickbob(),
						"{ ( on X' in People ) (if sick(X') then 1 else 0) | X = X' = person1 or X = X' = person2 or X = X' = person3 }",
						false,
						"if sick(X) then 1 else 0"
						),	
				// Ensure the free Variable X is selected and not the scoped W variable
				new PickSingleElementTestData("X = person1 or X = person2 or X = person3",
						new TrivialSickbob(),
						"{ ( on W in People, X' in People ) (if sick(X') then 1 else 0) | W = X = X' = person1 or W = X = X' = person2 or W = X = X' = person3 }",
						false,
						"if sick(X) then 1 else 0"
						// "if sick(if X = person1 then person1 else (if X = person2 then person2 else person3)) then 1 else 0"
						),	
				// Ensure the free Variable X is selected and not the scoped W variable
				new PickSingleElementTestData(Expressions.TRUE.toString(),
						new TrivialSickbob(),
						"{ ( on X' in People, W in People ) (if sick(X') then 1 else 0) | W = X = X' }",
						false,
						"if sick(X) then 1 else 0"
						),
				//
				//
				new PickSingleElementTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"{ ( on X', Y ) (if p(X') or p(Y) then 1 else 0) | (X = X' or X = Y) and X' = Y }",
						false,
						"if p(X) or p(X) then 1 else 0"
						),
				// 
				new PickSingleElementTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"{ ( on X', Y )  (if p(X') then 2 else 1) | (X = X' or X = Y) and X' != Y and X' = X }",
						false,
						"if p(X) then 2 else 1"
						),
				//
				new PickSingleElementTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"{ ( on X', Y)  (if p(Y) then 2 else 1) | (X = X' or X = Y) and X' != Y and X' != X }",
						false,
						"if p(X) then 2 else 1"
						),
				//
				// Complex Case 1: from running R_belief with partitioning on intersection logic
				new PickSingleElementTestData("X != w7 and X != X3 and X3 != w7 and X1 != X2 and X1 != X3 and X2 != X3 and (X2 != w7 or X1 != X) and X' != Y and X' != Z and Z != Y and (X2 = X' and X1 = Y or X2 = X' and X1 = Z) and not (X != w7 and X1 != w7 and X1 != X and X' != Y and X' != Z and Z != Y and (X' = w7 and X1 = Y or X' = w7 and X1 = Z) and X2 = w7) and X != w7 and X1 != X2 and (X2 != w7 or X1 != X) and X' != Y and X' != Z and Z != Y and (X2 = X' and X1 = Y or X2 = X' and X1 = Z) and (X2 != w7 or X2 = w7 and (X1 = X or X1 = w7 or (X' != w7 or X1 != Y) and (X' != w7 or X1 != Z) or X' = w7 and Y = X and Z = X1)) and (X2 = w7 and X1 != X and X1 != w7 and (X' = w7 and X1 = Y or X' = w7 and X1 = Z) and (X' != w7 or Y != X or Z != X1) or X1 != w7 and X1 != X and X' = w7 and Y = X and Z = X1 and X2 = w7)",
						new Model(Model.getModelDeclarationFromResource("Example4DefectIteration2.model")),
						"{ ( on Z'', X'', Y', Z' ) (if Y' = Z'' then 0.5 else (if not r(X'', Z') then 0.75 else 0.25)) }",
						false,
						// Note: Was -
						// "if w7 = w7 then 0.5 else (if not r(w7, w7) then 0.75 else 0.25)"
						// Now -
						"if X = X then 0.5 else (if not r(X, X) then 0.75 else 0.25)"
						),
				//
				// Complex Case 2: from running R_belief with partitioning on intersection logic
				// LBPStressIT.testStressTest2() example that arises when using new intersection one liner: 
				new PickSingleElementTestData("X != b and X != Y and Y != Y' and (Y' = X' or Y' = Y'') and (X' != Y or Y'' != Y') and not (X != Y' and (X' != X or Y'' != Y')) and not (X = X' and Y' = Y'' and Y' != X) and not ((X = Y' or X' = X and Y'' = Y') and (Y' = X or X' != X or Y'' != Y'))",
				        new Model(Model.getModelDeclarationFromResource("Example4DefectIteration2.model")),
				        "{ ( on Y''', X'', Y'''' ) (if X'' = Y'''' then if p(X'') or p(Y'''') then 1 else 0 else (if X'' = Y''' then if p(X'') then 0.666666667 else 0.333333333 else (if p(Y'''') then 0.666666667 else 0.333333333))) }",
				        false,
				        // Note: Was -
				        // "if b = b then if p(b) or p(b) then 1 else 0 else (if b = b then if p(b) then 0.666666667 else 0.333333333 else (if p(b) then 0.666666667 else 0.333333333))"
				        // Now -
				        "if X = X then if p(X) or p(X) then 1 else 0 else (if X = X then if p(X) then 0.666666667 else 0.333333333 else (if p(X) then 0.666666667 else 0.333333333))"
				        ), 		
				//
				// Complex Case 3: from running R_belief with partitioning on intersection logic
				// LBPStressIT.testStressTest3() example that arises when using new intersection one liner:
		        new PickSingleElementTestData("Y' != bob and Y != X and (Y' = X or Y' = Y) and (X != bob or Y != Y') and not (Y' != ann and (X != ann or Y != Y')) and not (X = ann and Y' = Y and Y' != ann) and not ((Y' = ann or X = ann and Y = Y') and (Y' = ann or X != ann or Y != Y'))",
		                new Model(Model.getModelDeclarationFromResource("Example4DefectIteration2.model")),
		                "{ ( on Y'', Y''', X' ) (if X' = Y'' then 0.5 else (if smoker(Y''') then 0.55 else 0.45)) }",
		                false,
		                // Note: Was -
		                // "if bob = bob then 0.5 else (if smoker(bob) then 0.55 else 0.45)"
		                // Now -
		                "if Y' = Y' then 0.5 else (if smoker(Y') then 0.55 else 0.45)"
		                ),
				//
				// Complex Case 4: from running R_belief with partitioning on intersection logic
		        // LBPStressIT.testStressTest4DefectIteration2() example that arises when using new intersection one liner: 
		        new PickSingleElementTestData("Z != X and X != w7 and Z != w7 and Z' != Z and Z' != w7",
		        		new Model(Model.getModelDeclarationFromResource("Example4DefectIteration2.model")),
		                "{ ( on X3 ) (if Z' != X then 1 else (if X != X3 then 1 else 1)) | Z' != X3 and X3 != w7 }",
		                false,
		                "if Z' != X then 1 else (if X != Z then 1 else 1)"
		                ),
		        //
		        // Complex Case 5: conditional single value example
		        new PickSingleElementTestData(Expressions.TRUE.toString(),
		        		new Model(Model.getModelDeclarationFromResource("Example4DefectIteration2.model")),
		                "{ (on X) X | (Z = a => X = c) and (Z != a => X = d) }",
		                false,
		                "if Z = d then Z else if Z = a then c else d"
//		                "if Z = a then c else d"
		                ),
                new PickSingleElementTestData("not (X = b)",
                		new com.sri.ai.praise.model.example.TrivialLoopyPQandb(),
		                "{ ( on X', Y ) (if p(X) then if X' != b then if Y = b then if true then 1 else 0 else (if true then 1 else 0) else (if true then 1 else 0) else 1) | ((X = X' or X = Y) and not (X' = Y)) and X' = X }",
		                false,
		                "if p(X) then if X != b then if b = b then if true then 1 else 0 else (if true then 1 else 0) else (if true then 1 else 0) else 1"
		                // null
		                ),
		        // Simplified version of the above test
                new PickSingleElementTestData("not (X = b)",
                		new com.sri.ai.praise.model.example.TrivialLoopyPQandb(),
		                "{ ( on X', Y ) (if p(X) then 1 else 1)  | ((X = X' or X = Y) and not (X' = Y)) and X' = X }",
		                false,
		                "if p(X) then 1 else 1"
		                ), 
		        // Even more simplified version of the above test
                new PickSingleElementTestData("not (X = b)",
                		new com.sri.ai.praise.model.example.TrivialLoopyPQandb(),
		                "{ ( on X', Y ) 1  | ((X = X' or X = Y) and not (X' = Y)) and X' = X }",
		                false,
		                "1"
		                ),			                
		                
		};
		
		perform(tests);
	}
	
	@Test
	public void testPickValue() {
		class PickValueTestData extends TestData {
			private String     X, I, C;
			private Expression exprX, exprC;
			Expression exprI;
			
			public PickValueTestData(String contextualConstraint, Model model, String X, String I, String C, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.X = X;
				this.I = I;
				this.C = C;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprX = parse(X);
				this.exprI = parse(I);
				this.exprC = parse(C);
				
				Expression top = Tuple.make(exprX, exprI, exprC);
				
				return top;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression result = PickSingleElement.pickValue(exprX, ExtensionalSet.getElements(exprI), exprC, process);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
				//
				// Basic:
				//
				new PickValueTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"X",
						"{X}",
						"X = a",
						false,
						"a"
						),
				new PickValueTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"X",
						"{X, Y}",
						"X = a and Y = b",
						false,
						"a"
						),
				new PickValueTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"X",
						"{X}",
						"not X != a and X = Y",
						false,
						"Y" // this tests the preference for variables, as 'X = a' is also correct
						),
				new PickValueTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"Y",
						"{X, Y}",
						"X = a and Y = b",
						false,
						"b"
						),
				new PickValueTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"X'",
						"{X'}",
						"X = X'",
						false,
						"X"
						),
				new PickValueTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"X'",
						"{X'}",
						"X = X' = person1",
						false,
						"person1"
						),
				new PickValueTestData("X = person1 or X = person2 or X = person3", 
						new TrivialPQ(),
						"X'",
						"{X'}",
						"X = X' = person1 or X = X' = person2 or X = X' = person3",
						false,
						"X"
						),
				new PickValueTestData("X = person1 or X = person2 or X = person3", 
						new TrivialPQ(),
						"X'",
						"{X', W}",
						"W = X = X' = person1 or W = X = X' = person2 or W = X = X' = person3",
						false,
						"X"
						// "if X = person1 then person1 else (if X = person2 then person2 else person3)" // Used to be this before moving preference for variables
						),
				new PickValueTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"X'",
						"{X', W}",
						"W = X = X'",
						false,
						"X"
						),
				//
				// Basic: Requires There Exists formula simplification
				//					
				new PickValueTestData(Expressions.TRUE.toString(), 
						new TrivialPQ(),
						"Y",
						"{X, Y, Z}",
						"X = a and (Y = b or Y = c) and (Z = b => Y = c) and Z = b ",
						false,
						"c"
						),
		};
		
		perform(tests);
	}

	@Test
	public void testGetConditionalSingleValue() {
		Expression formula;
		Expression variable;
		Expression expected;

		variable = parse("X");
		
		formula = parse("X = a and X != a");
		expected = null;
		runGetConditionalSingleValueTest(formula, variable, expected);
		
		formula = parse("X = a");
		expected = parse("a");
		runGetConditionalSingleValueTest(formula, variable, expected);
		
		formula = parse("Y = a and X = a and Z = b");
		expected = parse("a");
		runGetConditionalSingleValueTest(formula, variable, expected);
		
		formula = parse("Y = a and X = a and Z = b  or  X = b");
		expected = parse("if Y = a and Z = b then a else b");
		runGetConditionalSingleValueTest(formula, variable, expected);
		
		formula = parse("(Y = a and X = b) or (Y = c and X = d)");
		expected = parse("if Y = a then b else d"); // no need to test Y = c since it is implied by the formula and Y != a
		runGetConditionalSingleValueTest(formula, variable, expected);
	}

	public void runGetConditionalSingleValueTest(Expression formula, Expression variable, Expression expected) {
		Expression actual;
		RewritingProcess process;
		process = LBPFactory.newBoundLBPProcess(formula);
		process = GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(formula, process); 
		actual = PickSingleElement.getConditionalSingleValueOrNullIfNotDefinedInAllContexts(variable, formula, process);
		System.out.println("Formula : " + formula);	
		System.out.println("Expected: " + expected);	
		System.out.println("Actual  : " + actual);	
		assertTrue(Util.equals(expected, actual));
	}
	
	@Test
	public void testFriendshipModel() {
		newRewritingProcess(null);
		// create new process so that all newly constructed expressions are interpreted according to this process's modules,
		// particularly order normalization, which is cached upon the first use of the equals method.

		Expression query;
		Model      model;
		Expression actual;
		Expression expected;
		
		query = parse("friends(dave, X)");
		model = Model.fromRules(
				"sort People: 10, ann, bob, dave, rodrigo, ciaran;" + 
				"" + 
				"random friends: People x People -> Boolean;" + 
				"" + 
				"friends(X,Y) <=> friends(Y,X);" + 
				"not friends(X,X);" + 
				"" + 
				"friends(bob, dave);" + 
				"");
		expected = parse("if X = dave then if friends(dave, dave) then 0 else 1 else (if X = bob then if friends(dave, bob) then 1 else 0 else 0.5)");
		Solver solver = new LiftedBeliefPropagationSolver(true);
		actual = solver.marginal(query, model);
		Assert.assertEquals(expected, actual);

		GrinderUtil.doTreeUtilWaitUntilClosed();
	}
	
	@Test
	public void testBeliefForLoopyModels() {
		class LoopyBeliefTestData extends TestData {
			private String belief; 
			private Expression beliefDefinition;
			private Map<Object, Object> globalObjects;
			
			public LoopyBeliefTestData(String contextualConstraint, Model model, String belief, boolean illegalArgumentTest, String expected) {
				this(contextualConstraint, model, belief, null, illegalArgumentTest, expected);
			};
			
			public LoopyBeliefTestData(String contextualConstraint, Model model, String belief, Map<Object, Object> globalObjects, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.belief = belief;
				this.globalObjects = globalObjects;
			};
			
			@Override
			public Expression getTopExpression() {
				this.beliefDefinition = parse(belief);
				return this.beliefDefinition;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				if (globalObjects != null) {
					process.getGlobalObjects().putAll(globalObjects);
				}
				LBPConfiguration configuration = LBPFactory.newLBPConfiguration();
				// Currently only schedule that will work with a loopy model
				configuration.setBeliefPropagationUpdateSchedule(LBPConfiguration.BeliefPropagationUpdateSchedule.SYNCHRONOUS);
				RewritingProcess lbpProcess = LBPFactory.newLBPProcess(process.getRootExpression(), configuration, process);
				
				
				Expression belief = lbpProcess.rewrite(LBPRewriter.R_belief, beliefDefinition);
				Expression roundedBelief = Expressions.roundToAGivenPrecision(belief, 9, process);
				return roundedBelief;
			}
		};
		
		TestData[] tests = new TestData[] {
				//
				// Basic:
				// 
				
				// #0
				new LoopyBeliefTestData(Expressions.TRUE.toString(),
					Model.fromRules("Model Name: Happy Pet",
									"Model Description: Happy Pet Example.",
							        "sort People: 10, ann, bob, dave, rodrigo, ciaran;" +
									"sort Pets: 20, astor, rover;" +

									"random isPetOf: Pets x People -> Boolean;" +
									"random happy: People -> Boolean;" +

									"if isPetOf(Pet, Person) then happy(Person) 0.6 else happy(Person) 0.5;" +

									"if Pet != rover and Pet != astor then not isPetOf(Pet, bob) else isPetOf(Pet, bob);" +

									"if isPetOf(Pet, Person1) then not isPetOf(Pet, Person2);"),
					"belief([happy(bob)])", 
					false, 
					"if happy(bob) then 0.692307692 else 0.307692308"), // two pets exactly, so 0.6*0.6*0.5^18/(0.6*0.6*0.5^18 + 0.4*0.4*0.5^18)
					
				// #1
				new LoopyBeliefTestData(Expressions.TRUE.toString(),
					new TrivialLoopyPQ(), 
					"belief([p(X)])", 
					false, 
					// Note: (LBP 10 iterations) - these values oscillate
					// and this can be seen in grounded versions of
					// this model (type size 10).
					// --------------------
					// MRF - varelim
					// --------------------
					//                 True    False
					//     p(obj1) 0.749878 0.250122
					//     q(obj1) 0.749878 0.250122
					// ...
					// --------------------
					// MRF - libdaiBP
					// --------------------
					//                 True    False
					//     p(obj1) 0.998927 0.001073
					//     q(obj1) 0.505314 0.494686
					// ...
					"if p(X) then 0.998927766 else 0.00107223386"
					 ),
					
				// #2
				new LoopyBeliefTestData(Expressions.TRUE.toString(),
					new TrivialLoopyPQWithPriors(), 
					"belief([p(X)])", 
					false, 
					// Note: (LBP 10 iterations) - these values are equivalent
					// to the values returned from running variable eliminations (i.e. exact)
					// on a grounded version of this model (type size 10).
					// --------------------
					// MRF - varelim
					// --------------------
					//                 True    False
					//     p(obj1) 0.996017 0.003983
					//     q(obj1) 0.997635 0.002365
					//     ...
					//"if p(X) then 0.996016637 else 0.00398336255" // value obtained before inclusion of simple and deterministic messages treatment -- the difference is a little odd given that the model does not have determinism, but perhaps this changes the order of computations and numerical differences occur.
					"if p(X) then 0.996016485 else 0.00398351518"
					),				
					
				// #3
				new LoopyBeliefTestData(Expressions.TRUE.toString(),
					new TrivialLoopyParfactorsExample(),
					"belief([m(X)])",
					false,
					// Note: (LBP 10 iterations)
					// --------------------
					// MRF - varelim
					// --------------------
					//                 True    False
					//     g(obj1) 0.246396 0.753604
					//     m(obj1) 0.246396 0.753604
					// ...
					// --------------------
					// MRF - libdaiBP
					// --------------------
					//                 True    False
					//     g(obj1) 0.225974 0.774026
					//     m(obj1) 0.225974 0.774026
				    // ...
					// Appears not to be converging.
					"if m(X) then 0.103394195 else 0.896605805"
					),
		};

		perform(tests);
	}
	
	// TODO: debug
	@Test
	public void testDistinctTypeComparisons() {
		newRewritingProcess(null);
		// create new process so that all newly constructed expressions are interpreted according to this process's modules,
		// particularly order normalization, which is cached upon the first use of the equals method.

		Model model;
		Expression queryAnswer;
		
		model = new Model("model(sort(People, 10), sort(Dogs, 10), randomVariable(equalityOccurred, 0, Boolean),"
				+ "parfactors("
				+ "{{(on X in People, Y in Dogs) [if equalityOccurred then 1 else 0] | X = Y }}" // should have no effect
				+ ") )");
		
		Solver solver = new LiftedBeliefPropagationSolver();
		
		queryAnswer = solver.marginal(parse("equalityOccurred"), model);
		assertEquals(parse("0.5"), queryAnswer);
		
		model = new Model("model(sort(People, 10), sort(Dogs, 10), randomVariable(equalityOccurred, 0, Boolean),"
				+ "parfactors("
				+ "{{(on X in People, Y in People) [if equalityOccurred then 1 else 0] | X = Y }}" // should have an effect since X = Y for some values
				+ ") )");
		queryAnswer = solver.marginal(parse("equalityOccurred"), model);
		assertEquals(parse("if equalityOccurred then 1 else 0"), queryAnswer);
		
		GrinderUtil.doTreeUtilWaitUntilClosed();
		
		// Below: rule converter not properly converting this model, but it should be tested once that's fixed.
//		Model model = Model.fromRules(
//				"sort People: 10;"
//						+ "sort Dogs: 10;"
//						+ "random happy: People -> Boolean;"
//						+ "random fluffy: Dogs -> Boolean;"
//						+ "random absurd: Boolean;"
//						+ ""
//						+ "happy(X); // every person is happy"
//						+ "fluffy(Y); // every dog is fluffy"
//						+ "happy(X) and fluffy(Y) and (X = Y <=> absurd); // X is person and Y is dog, so they can't be the same");
//		
//		// The test here is that if our inference engine knows that two variables of different types are necessarily distinct, then the probability of absurd will be 1, else 0.
//		// The reason we need the happy and fluffy predicates is that variable types are inferred from the predicates they are used in.
//		
//		Expression probabilityOfAbsurd = Belief.compute(parse("absurd"), model);
//		System.out.println("Result: " + probabilityOfAbsurd);	
//		assertEquals(parse("if absurd then 0 else 1"), probabilityOfAbsurd);
	}
	
	@Test
	public void testLBPQueryEngine() {
		final LBPQueryEngine queryEngine = LBPFactory.newLBPQueryEngine();
		final String queryUUID1 = queryEngine.newQueryUUID();
		// Note: for the second query I'm changing the query options so that the
		// type size is not known so should get a formula back with this query.
		final String queryUUID2 = queryEngine.newQueryUUID(new LBPQueryEngine.QueryOptions(false, true, true));
		final String queryUUID3 = queryEngine.newQueryUUID();
		
		final Map<String, StringBuilder> queryTraceOutput = new LinkedHashMap<String, StringBuilder>();
		queryTraceOutput.put(queryUUID1, new StringBuilder()); 
		queryTraceOutput.put(queryUUID2, new StringBuilder()); 
		queryTraceOutput.put(queryUUID3, new StringBuilder());
		final Map<String, StringBuilder> queryJustificationOutput = new LinkedHashMap<String, StringBuilder>();
		queryJustificationOutput.put(queryUUID1, new StringBuilder()); 
		queryJustificationOutput.put(queryUUID2, new StringBuilder());
		queryJustificationOutput.put(queryUUID3, new StringBuilder()); 
		queryEngine.addQueryStepListener(new LBPQueryEngine.QueryStepListener() {
			
			@Override
			public void queryStepStarting(String queryUUID, String description) {
				System.out.println("Starting: "+description+ " " + queryUUID);				
			}
			
			@Override
			public void queryStepComplete(String queryUUID, QueryStep completedStep) {
				System.out.println("Finished: " + completedStep.getDescription() + " in " + completedStep.getTimeInNanoseconds() + " nanoseconds " + queryUUID);
				for (QueryStep subStep : completedStep.getSubSteps()) {
					System.out.println("    "+subStep.getDescription() + " in " + subStep.getTimeInNanoseconds() + " nanoseconds "+queryUUID);
				}
			}
		});
		LBPQueryEngine.TraceListener traceListener = new LBPQueryEngine.TraceListener() {
			
			@Override
			public void traceEvent(String queryUUID, int traceLevel, Long rootProfileInfo, Long profileInfo, Marker marker,
					String formattedMsg, Object... args) {
				StringBuilder sb = queryTraceOutput.get(queryUUID);
				sb.append("|");
				sb.append(indent(traceLevel));
				sb.append(formattedMsg);
				sb.append("\n");
			}
		};
		queryEngine.addTraceListener(traceListener);
		LBPQueryEngine.JustificationListener justificationListener = new LBPQueryEngine.JustificationListener() {			
			@Override
			public void justificationEvent(String queryUUID, int justificationLevel,
					Marker marker, String formattedMsg, Object... args) {
				StringBuilder sb = queryJustificationOutput.get(queryUUID);
				sb.append("|");
				sb.append(indent(justificationLevel));
				sb.append(formattedMsg);
				if ("".equals(formattedMsg) && args != null && args.length == 1) {
					sb.append(args[0]);
				}
				sb.append("\n");
			}
		};
		queryEngine.addJustificationListener(justificationListener);
		
		final Map<String, String> results = new LinkedHashMap<String, String>();
		
		Runnable callQuery1 = new Runnable() {
			@Override
			public void run() {
				String result = queryEngine.queryBeliefOfRandomVariable(queryUUID1,
						"belief([rich(X)])", 
						(new TrivialGaveTreasureToOwnsRich()).getModelDeclaration());
				results.put(queryUUID1, result);
			}
		};
		Runnable callQuery2 = new Runnable() {
			@Override
			public void run() {
				String result = queryEngine.queryBeliefOfRandomVariable(queryUUID2,
						"belief([rich(bob)])", 
						(new TrivialGaveTreasureToOwnsRich()).getModelDeclaration());
				results.put(queryUUID2, result);
			}
		};
		Runnable callQuery3 = new Runnable() {
			@Override
			public void run() {
				String result = queryEngine.queryBeliefOfRandomVariable(queryUUID3,
						"belief([rich(bob)])", 
						(new TrivialGaveTreasureToOwnsRich()).getModelDeclaration(),
						// Here I have evidence that bob has 100% probability of being rich.
						"parfactors({[if rich(bob) then 1 else 0]})");
				results.put(queryUUID3, result);
			}
		};
		Thread callQuery1Thread = new Thread(callQuery1);
		Thread callQuery2Thread = new Thread(callQuery2);
		Thread callQuery3Thread = new Thread(callQuery3);
		
		// Note: The tree util UI is not multi-threaded so want to ensure is turned off
		Configuration.setProperty(GrinderConfiguration.KEY_DISPLAY_TREE_UTIL_UI, "false");
		Configuration.inheritConfiguration(Thread.currentThread(), callQuery1Thread);
		Configuration.inheritConfiguration(Thread.currentThread(), callQuery2Thread);
		Configuration.inheritConfiguration(Thread.currentThread(), callQuery3Thread);
		try {
			callQuery1Thread.start();
			callQuery2Thread.start();
			callQuery3Thread.start();
			
			callQuery1Thread.join();
			callQuery2Thread.join();
			callQuery3Thread.join();
		} catch (InterruptedException ie) {
			Assert.fail();
		} 
		
		queryEngine.removeTraceListener(traceListener);
		queryEngine.removeJustificationListener(justificationListener);
		
		System.out.println("QUERY 1 - TRACE");
		System.out.println(queryTraceOutput.get(queryUUID1).toString());
		System.out.println("QUERY 1 - JUSTIFICATION");
		System.out.println(queryJustificationOutput.get(queryUUID1).toString());
		
		System.out.println("QUERY 2 - TRACE");
		System.out.println(queryTraceOutput.get(queryUUID2).toString());
		System.out.println("QUERY 2 - JUSTIFICATION");
		System.out.println(queryJustificationOutput.get(queryUUID2).toString());
		
		System.out.println("QUERY 3 (with Evidence) - TRACE");
		System.out.println(queryTraceOutput.get(queryUUID3).toString());
		System.out.println("QUERY 3 (with Evidence) - JUSTIFICATION");
		System.out.println(queryJustificationOutput.get(queryUUID3).toString());
		
		// Note: old R_basic result
		// Assert.assertEquals("if rich(X) then (1 + 2 ^ | People |) ^ | Treasure | / ((1 + 2 ^ | People |) ^ | Treasure | + 1) else 1 / ((1 + 2 ^ | People |) ^ | Treasure | + 1)", results.get(queryUUID1));
		Assert.assertEquals("if rich(X) then 1 else 0.000000000000000000000000000000781198", results.get(queryUUID1));
		// Note: old R_basic result
		Assert.assertEquals("if rich(bob) then (1 + 2 ^ | People |) ^ | Treasure | / ((1 + 2 ^ | People |) ^ | Treasure | + 1) else 1 / ((1 + 2 ^ | People |) ^ | Treasure | + 1)", results.get(queryUUID2));
		// Assert.assertEquals("if rich(bob) then (1 + 2 ^ | type(X) |) ^ | type(Z) | / ((1 + 2 ^ | type(X) |) ^ | type(Z) | + 1) else 1 / ((1 + 2 ^ | type(X) |) ^ | type(Z) | + 1)", results.get(queryUUID2));
		// Note: This is essentially the evidence reflected back.
		Assert.assertEquals("if rich(bob) then 1 else 0", results.get(queryUUID3));
	}
	
	private String indent(int level) {
		StringBuilder indent = new StringBuilder();
		for (int i = 0; i < level; i++) {
			indent.append("    ");
		}
		return indent.toString();
	}
}
