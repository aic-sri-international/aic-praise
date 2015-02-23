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

import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityOfType;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.example.TrivialGaveTreasureToOwnsRich;
import com.sri.ai.praise.model.example.TrivialLoopyPQandb;
import com.sri.ai.praise.model.example.TrivialPQBothArity2;
import com.sri.ai.praise.model.example.TrivialPQR;
import com.sri.ai.praise.model.example.TrivialPQRWithPriors;
import com.sri.ai.praise.model.example.TrivialPRWithNonDeterministicFactor;
import com.sri.ai.test.praise.AbstractLPITest;

public class NormalizeAndCompleteNormalizeTest extends AbstractLPITest {

	@Test
	public void testNormalizePassesFormulaSimplificationTests() {
		// R_formula_simplification(E)
		// This is the top rewriter for equality boolean formulas simplification, described elsewhere.
		// It includes the fact that random variable expressions are bijective:
		// replace all subexpressions of the form [ v(t1, ..., tn) ] = [ v'(r1, ..., rm) ]
		//    by R_formula_simplification(v = v' and n = m and t1 = r1 and ... tn = rm)
		TestData[] tests = new TestData[] {
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"a = a", 
						"true"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"X = X", 
						"true"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"a = b", 
						"false"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"X = a", 
						"X = a"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"m(a,b) = m(b,a)", 
						"m(a,b) = m(b,a)"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"[ p(a) ] = [ p(a) ]", 
						"true"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"[ p(X) ] = [ p(X) ]", 
						"true"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"[ p(a) ] = [ p(b) ]", 
						"false"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"[ p(X) ] = [ p(b) ]", 
						"X = b"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"[ q(X, Z) ] = [ q(X, Y) ]", 
						"Z = Y"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"[ q(X, X1) ] = [ q(X, Y) ]", 
						"X1 = Y"),
				new NormalizeTestData(Expressions.TRUE.toString(),
						new TrivialPQR(),
						"[p(X) and q(Y, Z)] = [p(A) and q(B, C)] ",
						"X = A and Y = B and Z = C "),
				new NormalizeTestData(Expressions.TRUE.toString(),
						new TrivialPQR(),
						"[p(X) and q(X, Y)] = [p(A) and q(B, C)] ",
						"X = A and X = B and Y = C "),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQRWithPriors(), 
						"[ q(X) ] = [ q(Y) ]", 
						"X = Y"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQRWithPriors(), 
						"[ q(Y') ] = [ q(Y) ]", 
						"Y' = Y"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQRWithPriors(), 
						"[ q(Y') ] = [ q(Y'') ]", 
						"Y' = Y''"),
				// START - from ALBPTest.testBracketedExpressionEquality()
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQBothArity2(), 
						"[p(a, X)] = [p(Y, b)]", 
						"Y = a and X = b"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQBothArity2(), 
						"[p(a, X)] != [p(Y, b)]", 
						"Y != a or X != b"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQBothArity2(), 
						"[if p(X, Y) then 0.5 else 0.1] = [if p(Y, Z) then 0.5 else 0.1]", 
						"X = Y and Y = Z"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQBothArity2(), 
						"[p(a, X)] = [q(Y, b)]", 
						"false"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQBothArity2(), 
						"[p(a, X)] != [q(Y, b)]", 
						"true"),
				// END - from ALBPTest.testBracketedExpressionEquality()
				// START- from EqualityTest.testSolve().
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"X = a and X != a", 
						"false"),
// Note: these kinds of simplifications (Conjunction of cluster and normalized equality ) are currently
// not to be supported by R_normalize.
//				new NormalizeTestData(Expressions.TRUE.toString(), 
//						new TrivialPQR(), 
//						"X = a and X != b", 
//						"X = a"),
//				new NormalizeTestData(Expressions.TRUE.toString(), 
//						new TrivialPQR(), 
//						"Y != b and X = Y", 
//						"X = Y and X != b"),
//				new NormalizeTestData(Expressions.TRUE.toString(), 
//						new TrivialPQR(), 
//						"X = a and Y != b and X = Y", 
//						"X = Y = a"),
				// END - from EqualityTest.testSolve().
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"(X = a) => (b = X)", 
						"X != a"),
				// Tests for resolved ALBP-72.
				// Note: semantics are that a and b belong to
			    // the type X is part of if in an equality
				// therefore type is at least size 2.
				new NormalizeTestData(Expressions.TRUE.toString(),
						new TrivialPQR(), 
						"there exists X: (X = a) => (X = b)", 
						"true"),	
				//
				// Basic: Contextual AbstractEqualityConstraint Tests
				//
				// Test a false contextual constraint
				new NormalizeTestData(Expressions.FALSE.toString(), 
						new TrivialPQR(), 
						"a = a",
						Rewriter.FALSE_CONTEXTUAL_CONTRAINT_RETURN_VALUE.toString()),						
		};
		
		perform(tests);
	}
	
	@Test
	public void testNormalizePassesBasicTests() {

		// Tests specific extensions in albp.Basic over that what is in grinder.Bbasic
		TestData[] tests = new TestData[] {
				// the next two tests test the differentiated parsing of parenthesized expressions and tuples (required to have at least two elements separated by comma)
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"(1) = 1",
						"true"),
				//
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"(1, 2) = (1, 3)",
						"false"),
				//
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"(X, Y) = (1, 2, 3)",
						"false"),
				//
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"(X, 2) = (1, Y)",
						"X = 1 and Y = 2"),
				//
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"0 ^ ( if p(b) then 1 else 0)",
						"if p(b) then 0 else 1"),
				// Test: Bracketed Expression Handling with respect to unions
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"union((if [ p(X) ] = [ p(a1) ] then { ([ if p(a1) then 1 else 0 ]) } else { }))",
						"if X = a1 then { ([ if p(a1) then 1 else 0 ]) } else { }"),
				// Ensuring random variables handled correctly
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"{{ ( on X, Y ) ([ if p(b) and q(X, Y) and r then 1 else 0 ]) | X != a }} union {{ ( on X, Y ) ([ if p(X) or p(Y) then 1 else 0 ]) | [ p(b) ] = [ p(X) ] or [ p(b) ] = [ p(Y) ] }}",
						"{{ ( on X, Y ) ([ if p(b) and q(X, Y) and r then 1 else 0 ]) | X != a }} union {{ ( on X, Y ) ([ if p(X) or p(Y) then 1 else 0 ]) | X = b or Y = b }}"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"[if p(X) then 0.1 else 0.2] = [if p(X) then 0.1 else 0.2]",
						"true"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"[if p(X) then 0.1 else 0.2] = [if p(Y) then 0.1 else 0.2]",
						"X=Y"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"[if p(X) and p(b) then 0.1 else 0.2] = [if p(a) and p(X) then 0.1 else 0.2]",
						"false"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"[if p(X) then 0.1 else 0.2] = [if p(Y) then 0.5 else 0.5]",
						"false"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if [ p(a1) ] = [ p(X) ] then 2 ^ |People| else 3 ^ |People| ",
						// Note: old result before CardinalityTypeOfLogicalVariable added support for looking up logical variables
						// representing the size of a type.
						//"if X = a1 then 2 ^ |People| else 3 ^ |People|",
						"if X = a1 then 1267650600228229401496703205376 else 515377520732011331036461129765621272702107522001"),
			    new NormalizeTestData(Expressions.TRUE.toString(), 
			    		new TrivialPQR(),
			    		"(if [ p(a1) ] = [ p(X) ] then 2 else 3) ^ |People|",
						// Note: old result before CardinalityTypeOfLogicalVariable added support for looking up logical variables
						// representing the size of a type.
			    		//"if X = a1 then 2 ^ |People| else 3 ^ |People|".
			    		"if X = a1 then 1267650600228229401496703205376 else 515377520732011331036461129765621272702107522001"),
				//
				// Test: Type Rewriter capability
				new NormalizeTestData(Expressions.TRUE.toString(),  // Should not be rewritten
						new TrivialPQR(),
						"type(X)",
						"type(X)"),						
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"type(p(X))",
						"{{false, true}}"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"type(p(a1))",
						"{{false, true}}"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"type(q(X, Y))",
						"{{false, true}}"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"type(q(a1, Y))",
						"{{false, true}}"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"type(q(a1, a2))",
						"{{false, true}}"),
				//
				// Test: ExternalizeConditionalMessage Rewriter capability
				new NormalizeTestData(Expressions.TRUE.toString(),  // No change expected as the condition comprises only of a logical equality
						new TrivialPQR(),
						"if X = a then E1 else E2",
						"if X = a then E1 else E2"),	
				new NormalizeTestData(Expressions.TRUE.toString(), // Expected as we want equalities in the condition by default
						new TrivialPQR(),
						"if X != a then E1 else E2",
						"if X = a then E2 else E1"),
				new NormalizeTestData(Expressions.TRUE.toString(),  // No change expected as the condition comprises only of two logical inequalities
						new TrivialPQR(),
						"if X != a and X != b then E1 else E2",
						"if X != a and X != b then E1 else E2"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if X = a and p(X) then E1 else E2",
						// Note: assigning variables to their contextual values is no longer part of simplification
						// "if X = a then if p(a) then E1 else E2 else E2"
						"if X = a then if p(a) then E1 else E2 else E2"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if X != a and p(X) then E1 else E2",
						"if X = a then E2 else (if p(X) then E1 else E2)"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if X = a and Y = b and q(X, Y) then E1 else E2",
						// Note: assigning variables to their contextual values is no longer part of simplification
						// "if X = a and Y = b then if q(a, b) then E1 else E2 else E2"
						"if X = a and Y = b then if q(a, b) then E1 else E2 else E2"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if X = a and r then E1 else E2",
						"if X = a then if r then E1 else E2 else E2"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if X = a and p(X) then if Y = b then E1 else E2 else if Y = c then E3 else E4",
						// Note: assigning variables to their contextual values is no longer part of simplification
						// "if Y = b then if X = a then if p(a) then E1 else E4 else E4 else if Y = c then if X = a then if p(a) then E2 else E3 else E3 else if X = a then if p(a) then E2 else E4 else E4"
						"if X = a then if Y = b then if p(a) then E1 else E4 else (if Y = c then if p(a) then E2 else E3 else (if p(a) then E2 else E4)) else (if Y = c then E3 else E4)"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if [ p(a1) ] = [ p(X) ] and q(X) then 2 ^ |People| else 3 ^ |People| ",
						// Note: assigning variables to their contextual values is no longer part of simplification
						// "if X = a1 then if q(a1) then 2 ^ |People| else 3 ^ |People| else 3 ^ |People|"
						// Note: old result before CardinalityTypeOfLogicalVariable added support for looking up logical variables
						// representing the size of a type.
						//"if X = a1 then if q(X) then 2 ^ |People| else 3 ^ |People| else 3 ^ |People|"
						"if X = a1 then if q(a1) then 1267650600228229401496703205376 else 515377520732011331036461129765621272702107522001 else 515377520732011331036461129765621272702107522001"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if X = a and Y = b and p(X) and q(X, Y) then E1 else E2",
						// Note: assigning variables to their contextual values is no longer part of simplification
						// "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else E2"
						"if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else E2"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if X = a and p(X) then if Y = b and q(X, Y) then E1 else E2 else if Y = c and q(X, Y) then E3 else E4",
						// Note: assigning variables to their contextual values is no longer part of simplification
						// "if X = a then if Y = b then if p(a) then if q(a, b) then E1 else E2 else E4 else if Y = c then if p(a) then E2 else if q(a, c) then E3 else E4 else if p(a) then E2 else E4 else if Y = c then if q(X, c) then E3 else E4 else E4"
						"if X = a then if Y = b then if p(a) then if q(a, b) then E1 else E2 else E4 else if Y = c then if p(a) then E2 else if q(a, c) then E3 else E4 else if p(a) then E2 else E4 else if Y = c then if q(X, c) then E3 else E4 else E4"),
				//
				// Case where R_incomplete_top_implied_certainty would apply itself to the lambda function argument and
			    // flip it from true to false continuously
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialLoopyPQandb(),
						"(if if b = X then false else p(b) then product({{ ( on X', Y ) (('lambda . : .'(q(X', Y), previous message to [ if p(b) and q(X', Y) then 2 else 1 ] from [ q(X', Y) ]))(false) + 2 * ('lambda . : .'(q(X', Y), previous message to [ if p(b) and q(X', Y) then 2 else 1 ] from [ q(X', Y) ]))(true)) | X' != a and X = b }}) else product({{ ( on X', Y ) (('lambda . : .'(q(X', Y), previous message to [ if p(b) and q(X', Y) then 2 else 1 ] from [ q(X', Y) ]))(false) + ('lambda . : .'(q(X', Y), previous message to [ if p(b) and q(X', Y) then 2 else 1 ] from [ q(X', Y) ]))(true)) | X' != a and X = b }})) ",
						"if X = b then product({{ ( on X', Y ) (('lambda . : .'(q(X', Y), previous message to [ if p(b) and q(X', Y) then 2 else 1 ] from [ q(X', Y) ]))(false) + ('lambda . : .'(q(X', Y), previous message to [ if p(b) and q(X', Y) then 2 else 1 ] from [ q(X', Y) ]))(true)) | X' != a }}) else 1"),
//
// Note: these kinds of simplifications (i.e. cardinality on extensional sets) are currently
// not to be supported by R_normalize.
//				new NormalizeTestData(Expressions.TRUE.toString(), 
//						new TrivialPQRWithPriors(),
//						"(if p(b) and q(X', Y) then if [ p(b) ] = [ p(X) ] then 2 else 4 else 0) ^ | {{ ( on X', Y ) E | X' != a and X = b }} |",
//					    "if X = b then if p(b) and q(X', Y) then 2 ^ (| type(Y) | * | type(X') - { a } |) else 0 ^ (| type(Y) | * | type(X') - { a } |) else 1"),
		};
		perform(tests);

	}
	
	@Test
	public void testNormalizePassesSimplifiedIfThenElseTests() {
		//
		// R_simplified_if_then_else(if C then E1 else E2)
		// C is a boolean expression
		// Returns an equivalent expression after simplifying E1 using the fact that its interpretation is subject to C 
		// being true, and E2 using the fact that its interpretation is subject to C being false
		// 
		// C <- R_formula_simplification(C)
		// E1 <- R_constrained_simplification(C, E1)
		// E2 <- R_constrained_simplification(C, E2)
		// return if C then E1 else E2
		TestData[] tests = new TestData[] {
				//
				// Collapse due to being deterministic
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if true then false else true", "false"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if false then false else true", "true"),
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if C = true then or(C, B) else or(C, B)", 
						"C or B"),
				//
				// Move random variables down
				new NormalizeTestData(Expressions.TRUE.toString(), 
						new TrivialPQR(),
						"if p(X) then if X = a then Alpha else Beta else Gamma", 
						"if X = a then if p(a) then Alpha else Gamma else if p(X) then Beta else Gamma"),
				//
// TODO - need fix for ALBP-67
//				new NormalizeTestData(Expressions.TRUE.toString(),
//						null,
//						"if C = true then or(C, B) else and(C, B)", 
//						"if C = true then true else false"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testCompleteNormalizeRequired() {
		TestData[] tests = new TestData[] {
			// None currently defined as QuantifierElimination added
			// to R_normalize (removed the 1 test that had been here).
		};
		
		perform(tests);
	}
	
	@Test
	public void testQuantifierElimination() {
		//
		// Based on: LBPTest.testPickSingleElement() tests
		TestData[] tests = new TestData[] {
				// Note: This requires known size information in order to simplify correctly.
				new NormalizeTestData(Expressions.TRUE.toString(),
						new TrivialPQR(), 
						"there exists X' : W = X = X' = person1 or W = X = X' = person2 or W = X = X' = person3", 
						// R_formula_simplification =
						// W = X = person1 or W = X = person2 or W = X = person3
						//"or((X = person1 and X = W), or((X = W and X = person2), (X = person3 and X = W)))"
						GrinderUtil.usePlain
						? "(W = X) and ((X = person1) or (X = person2) or (X = person3))" // plain cardinality 
						: "X = person1 and W = person1 or X = person2 and W = person2 or X = person3 and W = person3" // direct cardinality
						
						),
				new NormalizeTestData(Expressions.TRUE.toString(),
						new TrivialGaveTreasureToOwnsRich(), 
						"there exists X' in People : ([ if gaveTreasureTo(X, Z, Y) then if owns(Y, Z) then 1 else 0 else 1 ] = [ if gaveTreasureTo(X', Z, Y) then if owns(Y, Z) then 1 else 0 else 1 ])", 
						// R_formula_simplification =
						// | People | > 0
						"true"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testLiftProduct() {
		TestData[] tests = new TestData[] {
				new NormalizeTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on X)  6 }} )", 
						2, 
				        "36"),
				new NormalizeTestData(Expressions.TRUE.toString(),new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on X)  if r then 2 else 3 }} )", 
						2, 
				        "if r then 4 else 9"),
				new NormalizeTestData(Expressions.TRUE.toString(),new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on X)  if X = a then if r then 2 else 3 else if r then 4 else 5 }} )", 
						3, 
						// this is (if r then 2^1 else 3^1)*(if r then 4^2 else 5^2)
						"if r then 32 else 75"),
				new NormalizeTestData(Expressions.TRUE.toString(),new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on X)  if X = a and r then 2 else 3 }} )", 
						3, 
						// this is (if r then 2^1 else 3^1)*(3^2)
						"if r then 18 else 27"),
				new NormalizeTestData(Expressions.TRUE.toString(),new TrivialPRWithNonDeterministicFactor(), 
						"product({{ (on X)  if X = Y and r then 2 else 3 }} )", 
						3, 
						// this is (if r then 2^1 else 3^1)*(3^2)
						"if r then 18 else 27"),
				new NormalizeTestData("not (X = b)",
						new com.sri.ai.praise.model.example.TrivialLoopyPQandb(), 
						"product({{ ( on X', Y ) (if p(X) then if X' != b then if Y = b then if true then 1 else 0 else (if true then 1 else 0) else (if true then 1 else 0) else 1) | ((X = X' or X = Y) and not (X' = Y)) and X' = X }})", 
						100, 
						"1")
		};
		
		perform(tests);	
	}
	
	@Test
	public void testLambdaApplicationOnMessage() {
		TestData[] tests = new TestData[] {
			//
		    // Basic: should be applied
			new NormalizeTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
					"(lambda f(X) : 2 + f(X))(1)", 
			        "3"),
			new NormalizeTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
					"(lambda q(Y) : if q(Y) then 1 else 0)(false) ",  
			        "0"),	
			new NormalizeTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
					"(lambda q(Y) : if q(Y) then 1 else 0)(true) ",  
			        "1"),	
			//
			// Basic: should not be applied
			new NormalizeTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
					"(lambda q(Y) : (previous message to [q(X)] from [ if q(Y) then 1 else 0 ]))(true) ",  
			        "(lambda q(Y) : (previous message to [q(X)] from [ if q(Y) then 1 else 0 ]))(true)") 
		};
		
		perform(tests);
	}
	
	@Test
	public void testIncompleteLinearImpliedCertainty() {
		TestData[] tests = new TestData[] {
			//
		    // Basic: Ensure R_incomplete_top_implied_certainty does not loop
			new NormalizeTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
					"if X = b then 1 else ({{ true | X = b }})", 
			        "if X = b then 1 else ({ })"),
			new NormalizeTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
					"if X = b then 1 else ({{ false | X = b }})", 
			        "if X = b then 1 else ({ })"),
			new NormalizeTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
					"if X = b then 1 else ({{ true | X != b }})", 
			        "if X = b then 1 else ({{ true | true }})"),
			new NormalizeTestData(Expressions.TRUE.toString(), new TrivialPRWithNonDeterministicFactor(), 
					"if X = b then 1 else ({{ false | X != b }})", 
			        "if X = b then 1 else ({{ false | true }})"),
		};
		
		perform(tests);
	}
	
    @Test
    public void testExpandsIndefinitelyUsingNormalize() {
        TestData[] tests = new TestData[] {
            // This instance (found from from LBPStressTest#2 4 June 2013) 
        	// appears to expand its rewriting indefinitely using R_normalize.
            new NormalizeTestData("X != b and X != Y",
                new com.sri.ai.praise.model.example.TrivialLoopyPQandb(),
                "(if p(b) then 1 else 0) ^ (if Y = b then 90 else 0) * ((if p(Y) or p(Y) then 1 else 0) ^ 1 * ((if p(Y) then 0.666666667 else 0.333333333) ^ 9 * (if p(Y) then 0.666666667 else 0.333333333) ^ 8))",
                "if Y = b then if p(b) then 0.00101495924 else 0 else (if p(Y) then 0.00101495924 else 0)"
                ),
        };
       
        perform(tests);
    } 
    
    @Test
    public void testExpandsIndefinitelyUsingCompleteNormalize() {
        TestData[] tests = new TestData[] {
            // This instance (found from from LBPStressTest#2 4 June 2013) 
        	// appears to expand its rewriting indefinitely using R_complete_normalize.
            new CompleteNormalizeTestData("X != b and X != Y",
                new com.sri.ai.praise.model.example.TrivialLoopyPQandb(),
                "(if p(b) then 1 else 0) ^ (if Y = b then 90 else 0) * ((if p(Y) or p(Y) then 1 else 0) ^ 1 * ((if p(Y) then 0.666666667 else 0.333333333) ^ 9 * (if p(Y) then 0.666666667 else 0.333333333) ^ 8))",
                "if Y = b then if p(b) then 0.00101495924 else 0 else (if p(Y) then 0.00101495924 else 0)"
                ),
        };
       
        perform(tests);
    } 
    
	//
	// PRIVATE METHODS
	//
	class NormalizeTestData extends TestData implements CardinalityOfType.TypeSizeOfSymbolOrType {
		private String E; 
		private Expression exprE;
		private int cardinality;
		private Map<Object, Object> globalObjects;
		
		public NormalizeTestData(String contextualConstraint, Model model, String E, String expected) {
			this(contextualConstraint, model, E, 100, expected);
		}
		
		public NormalizeTestData(String contextualConstraint, Model model, String E, int cardinality, String expected) {
			this(contextualConstraint, model, E, null, cardinality, expected);
		}
		
		public NormalizeTestData(String contextualConstraint, Model model, String E, Map<Object, Object> globalObjects, int cardinality, String expected) {
			super(contextualConstraint, model, false, expected);
			this.E = E;
			this.cardinality = cardinality;
			this.globalObjects = globalObjects;
		}
		
		//
		// START-TypeSizeOfLogicalVariable
		@Override
		public Integer getSize(Expression logicalVariable, RewritingProcess process) {
			return cardinality;
		}
		// END-TypeSizeOfLogicalVariable
		//
		
		@Override
		public Expression getTopExpression() {
			this.exprE = parse(E);
			
			return exprE;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			if (globalObjects != null) {
				process.getGlobalObjects().putAll(globalObjects);
			}
			
			// Ensure explicit counts added for all variable types.
			CardinalityOfType.registerTypeSizeOfSymbolOrTypeWithProcess(this, process);
			
			return simplify(exprE, process);
		}
		
		//
		// PROTECTED METHODS
		protected Expression simplify(Expression expression, RewritingProcess process) {
			Expression result = process.rewrite(LBPRewriter.R_normalize, expression);
			
			result = Expressions.roundToAGivenPrecision(result, 9, process);
			
			return result;
		}
	};
	
	class CompleteNormalizeTestData extends NormalizeTestData {
		public CompleteNormalizeTestData(String contextualConstraint, Model model, String E, String expected) {
			super(contextualConstraint, model, E, expected);
		}
		
		public CompleteNormalizeTestData(String contextualConstraint, Model model, String E, int cardinality, String expected) {
			super(contextualConstraint, model, E, cardinality, expected);
		}
		
		@Override
		protected Expression simplify(Expression expression, RewritingProcess process) {
			Expression result = process.rewrite(LBPRewriter.R_complete_normalize, expression);
			
			result = Expressions.roundToAGivenPrecision(result, 9, process);
			
			return result;
		}
	}
}
