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
package com.sri.ai.test.praise.solver;

import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.query.ExpressionBasedQuerySolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.query.byalgorithm.evaluation.EvaluationExpressionBasedQuerySolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.query.byalgorithm.exactbp.ExactBPExpressionBasedQuerySolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedQuery;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedQuery;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.util.Util;

public class ExpressionBasedSolverTest {
	
	// The definitions of categorical types
	Map<String, String> mapFromCategoricalTypeNameToSizeString;

	// The definitions of other types
	Collection<Type> additionalTypes;
	
	// The definitions of variables
	Map<String, String> mapFromRandomVariableNameToTypeName;

	// The definitions of non-uniquely named constants
	Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName;

	// The definitions of uniquely named constants
	Map<String, String> mapFromUniquelyNamedConstantNameToTypeName;

	boolean isBayesianNetwork;
	List<Expression> factors;
	Expression evidence;
	Expression queryExpression;
	Expression expected;
	Expression expectedWithNoFactorization;
	Expression expectedWithFactorization;

	@Test
	public void test() {
		
		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = Util.map(
				"Folks", "10",
				"Boolean", "2");

		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"earthquake", "Boolean",
				"burglar",    "Folks", // a multi-value random variable
				"alarm",      "Boolean"
				);

		// The definitions of non-uniquely named constants
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map(
				"seismicLocation", "Boolean"
				);

		// The definitions of non-uniquely named constants
		mapFromUniquelyNamedConstantNameToTypeName = Util.map("none", "Folks", "tom", "Folks");

		// a variant of the earthquake/burglary model in which some burglars are more active than others.
		isBayesianNetwork = true;
		factors = Times.getMultiplicands(parse("" + 
				"(if earthquake then 0.01 else 0.99) * " +
				"(if burglar = none then 0.7 else if burglar = tom then 0.1 else 0.2 / (|Folks| - 2)) * " +
				// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
				"(if burglar != none or earthquake "
				+    "then if alarm then 0.9 else 0.1 "
				+    "else if alarm then 0.05 else 0.95) " +
				""));

		queryExpression = parse("earthquake");
		evidence = null; // no evidence
		expected = parse("if earthquake then 0.01 else 0.99"); // the prior
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("earthquake");
		evidence = parse("not alarm"); // can be any boolean expression
		expected = parse("if earthquake then 0.00145127349 else 0.998548727");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("earthquake");
		evidence = parse("alarm");
		expected = parse("if earthquake then 0.0289435601 else 0.97105644");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("earthquake");
		evidence = parse("alarm and burglar = none");
		expected = parse("if earthquake then 0.153846154 else 0.846153846");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("earthquake");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if earthquake then 0.01 else 0.99"); // the prior
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("earthquake");
		evidence = parse("true"); // no information
		expected = parse("if earthquake then 0.01 else 0.99"); // the prior
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

	
		
		queryExpression = parse("alarm");
		evidence = null; // no evidence
		expected = parse("if alarm then 0.31095 else 0.68905"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("alarm");
		evidence = parse("not alarm");
		expected = parse("if alarm then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("alarm");
		evidence = parse("alarm");
		expected = parse("if alarm then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("alarm");
		evidence = parse("alarm and burglar = none");
		expected = parse("if alarm then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("alarm");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if alarm then 0.31095 else 0.68905"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("alarm");
		evidence = parse("true"); // no information
		expected = parse("if alarm then 0.31095 else 0.68905"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

	
		
		queryExpression = parse("burglar = none");
		evidence = null; // no evidence
		expected = parse("if burglar = none then 0.7 else 0.3"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = none");
		evidence = parse("not alarm");
		expected = parse("if burglar = none then 0.956461795 else 0.0435382048");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = none");
		evidence = parse("alarm");
		expected = parse("if burglar = none then 0.131693198 else 0.868306802");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = none");
		evidence = parse("alarm and burglar = none");
		expected = parse("if burglar = none then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = none");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if burglar = none then 0.7 else 0.3"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = none");
		evidence = parse("true"); // no information
		expected = parse("if burglar = none then 0.7 else 0.3"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

	
		
		queryExpression = parse("burglar = tom");
		evidence = null; // no evidence
		expected = parse("if burglar = tom then 0.1 else 0.9"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = tom");
		evidence = parse("not alarm");
		expected = parse("if burglar = tom then 0.0145127349 else 0.985487265");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = tom");
		evidence = parse("alarm");
		expected = parse("if burglar = tom then 0.289435601 else 0.710564399");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = tom");
		evidence = parse("alarm and burglar = none");
		expected = parse("if burglar = tom then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = tom");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if burglar = tom then 0.1 else 0.9"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar = tom");
		evidence = parse("true"); // no information
		expected = parse("if burglar = tom then 0.1 else 0.9"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

	
	
	
		// queries on non-Boolean variable burglar:
		
		queryExpression = parse("burglar");
		evidence = null; // no evidence
		expected = parse("if burglar = none then 0.7 else if burglar = tom then 0.1 else 0.025"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar");
		evidence = parse("not alarm");
		expected = parse("if burglar = none then 0.956461795 else if burglar = tom then 0.0145127349 else 0.00362818373");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar");
		evidence = parse("alarm");
		expected = parse("if burglar = none then 0.131693198 else if burglar = tom then 0.289435601 else 0.0723589001");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar");
		evidence = parse("alarm and burglar = none");
		expected = parse("if burglar = none then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if burglar = none then 0.7 else if burglar = tom then 0.1 else 0.025"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("burglar");
		evidence = parse("true"); // no information
		expected = parse("if burglar = none then 0.7 else if burglar = tom then 0.1 else 0.025"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

	
	
	
	
		// now using the constant 'seismicLocation'
		factors = Times.getMultiplicands(parse("" + 
				"(if seismicLocation then if earthquake then 0.1 else 0.9 else if earthquake then 0.01 else 0.99) * " +
				"(if burglar = none then 0.7 else if burglar = tom then 0.1 else 0.2 / (|Folks| - 2)) * " +
				// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
				"(if burglar != none or earthquake "
				+    "then if alarm then 0.9 else 0.1 "
				+    "else if alarm then 0.05 else 0.95) " +
				""));

		queryExpression = parse("earthquake");
		evidence = null; // no evidence
		expected = parse("if seismicLocation then if earthquake then 0.1 else 0.9 else if earthquake then 0.01 else 0.99"); // the prior, parameterized by seismicLocation
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

		queryExpression = parse("earthquake");
		evidence = parse("not alarm"); // can be any boolean expression
		expected = parse("if seismicLocation then if earthquake then 0.0157356412 else 0.984264359 else if earthquake then 0.00145127349 else 0.998548727");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

		queryExpression = parse("burglar = tom");
		evidence = parse("alarm");
		// Here factorization creates two distinct but equivalent solutions; eventually we should have an equivalence test (didn't do it now because that would require full relational support even in the absence of random relational variables).
		expectedWithNoFactorization = parse("if seismicLocation then if burglar = tom then 0.24691358 else 0.75308642 else if burglar = tom then 0.289435601 else 0.710564399");
		expectedWithFactorization   = parse("if burglar = tom then if seismicLocation then 0.24691358 else 0.289435601 else if seismicLocation then 0.75308642 else 0.710564399");
		runTest(queryExpression, evidence, expectedWithNoFactorization, expectedWithFactorization, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
	}

	@Test
	public void testAPI() {
		// IMPORTANT: this test is reproduced in the User Guide as an example,
		// so it should be kept in sync with it.
		
		String modelString = ""
				+  
				"random earthquake: Boolean;" + 
				"random burglary: Boolean;" + 
				"random alarm: Boolean;" + 
				"" + 
				"earthquake 0.01;" + 
				"burglary 0.1;" + 
				"" + 
				"if earthquake" + 
				"   then if burglary" + 
				"      then alarm 0.95" + 
				"      else alarm 0.6" + 
				"   else if burglary" + 
				"      then alarm 0.9" + 
				"      else alarm 0.01;" + 
				"     " + 
				"not alarm;"
				+ "";
		
		Expression evidence = parse("not alarm");
		// can be any boolean expression

		boolean exploitFactorization = true;
		// exploit factorization (that is, employ ExpressionVariable Elimination,
		// as opposed to summing over the entire joint probability distribution).

		DefaultExpressionBasedQuery query;
		Expression marginal;

		HOGMExpressionBasedModel model = new HOGMExpressionBasedModel(modelString);
		model = model.getConditionedModel(evidence);
		Expression queryExpression;

		ExpressionBasedQuerySolver solver = new EvaluationExpressionBasedQuerySolver(exploitFactorization);

		queryExpression = parse("not earthquake");
		// can be any boolean expression, or any random variable
		query = new DefaultExpressionBasedQuery(model, queryExpression);
		marginal = solver.solve(query);
		System.out.println("Marginal is " + marginal);
		
		queryExpression = parse("earthquake");
		query = new DefaultExpressionBasedQuery(model, queryExpression);
		marginal = solver.solve(query);
		System.out.println("Marginal is " + marginal);
	}

	@Test
	public void relationalConstants() {

		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = Util.map(
				"Folks", "10",
				"Boolean", "2");

		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"happy", "Boolean",
				"boss",  "Folks"
				);

		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();

		mapFromUniquelyNamedConstantNameToTypeName = Util.map("tom", "Folks");

		isBayesianNetwork = true;
		factors = Times.getMultiplicands(parse(""
				+ "(1/|Folks|) *"  // uniform prior for 'boss' does not depend on the actual value of 'boss'
				+ "(if boss = tom then if happy then 1 else 0 else if happy then 0 else 1)"));

		queryExpression = parse("happy");
		evidence = null; // no evidence
		expected = parse("if happy then 0.1 else 0.9");
//		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

		queryExpression = parse("happy");
		evidence = parse("boss = tom");
		expected = parse("if happy then 1 else 0");
//		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

		// Now 'boss' is a constant:

		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"happy", "Boolean"
				);

		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map(
				"boss",  "Folks"
				);

		isBayesianNetwork = true;
		factors = Times.getMultiplicands(parse("" // no need for a prior for 'boss' now
				+ "(if boss = tom then if happy then 1 else 0 else if happy then 0 else 1)"));

		queryExpression = parse("happy");
		evidence = null; // no evidence
		expected = parse("if boss = tom then if happy then 1 else 0 else if happy then 0 else 1"); // query is a function of the constant
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());

// Ignore: as we implement function-typed theories
		
//		// Now 'boss' is a constant unary predicate:
//
//		// The definitions of variables
//		mapFromRandomVariableNameToTypeName = Util.map(
//				"happy", "Boolean"
//				);
//
//		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map(
//				"boss",  "->(x(Folks), Boolean)"
//				);
//
//		isBayesianNetwork = true;
//		factors = Times.getMultiplicands(parse("" // no need for a prior for 'boss' now
//				+ "(if boss(tom) then if happy then 0.9 else 0.1 else if happy then 0.2 else 0.8)"));
//
//		queryExpression = parse("happy");
//		evidence = null; // no evidence
//		expected = parse("if boss(tom) then if happy then 0.9 else 0.1 else if happy then 0.2 else 0.8"); // query is a function of the constant
//		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
//
//		queryExpression = parse("happy");
//		evidence = parse("happy");
//		expected = parse("if happy then 1 else 0"); // query is NOT a function of the constant in this case
//		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
	}

	@Test
	public void lucky() {
		
		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = Util.map(
				"People", "1000",
				"Boolean", "2");
	
		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"lucky",  "Boolean",
				"winner", "People"
				);
		
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();

		mapFromUniquelyNamedConstantNameToTypeName = Util.map("rodrigo", "People");
		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(""
				+ "(if lucky then 1 else 0)*"
				+ "(if lucky then if winner = rodrigo then 1 else 0 else 0.5)"));
	
		queryExpression = parse("winner = rodrigo");
		evidence = null;
		expected = parse("if winner = rodrigo then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
	}
	

	@Test
	public void smartest() {
		
		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = Util.map(
				"People", "1000",
				"Boolean", "2");
	
		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"boss",   "People",
				"smartest", "People"
				);
		
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();

		mapFromUniquelyNamedConstantNameToTypeName = Util.map("bob", "People", "mary", "People", "tom", "People");
		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(""
				+ "(if (for all P in People :   smartest = P   =>  boss = P) then 1 else 0) * " + // same as (if smartest = boss then 1 else 0) 
				"( if smartest != bob and smartest != mary then 1 else 0)" + 
				""));
		
		queryExpression = parse("boss = tom");
		evidence = null;
		expected = parse("if boss = tom then 0.00100200401 else 0.998997996"); // tom is 1 out of 998 people left to be the smartest and boss
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
		
		queryExpression = parse("boss");
		evidence = null;
		expected = parse("if boss = bob then 0 else if boss = mary then 0 else 0.00100200401"); // gives the distribution to all values of queried variable
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, list());
	}

	@Test
	public void differenceArithmeticOnIntervals() {
		
		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = Util.map(
				"Boolean", "2",
				"Country", "200");
	
		additionalTypes = list(new IntegerInterval(0, 99));
		
		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"age",   "0..99",
				"country", "Country",
				"minor", "Boolean",
				"workingAge", "Boolean",
				"senior", "Boolean"
				);
		
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();

		mapFromUniquelyNamedConstantNameToTypeName = Util.map("japan", "Country", "brazil", "Country");
		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(""
				+ "(if age >= 65 then if senior then 1 else 0 else if senior then 0 else 1)"
				+ "*(if age < 18 then if minor then 1 else 0 else if minor then 0 else 1)"
				+ "*(if age >= 21 and age < 65 then if workingAge then 1 else 0 else if workingAge then 0 else 1)"));
		
		queryExpression = parse("senior");
		evidence = null;
		expected = parse("if senior then 0.35 else 0.65");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("workingAge");
		evidence = null;
		expected = parse("if workingAge then 0.44 else 0.56");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor");
		evidence = null;
		expected = parse("if minor then 0.18 else 0.82");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor and senior");
		evidence = null;
		expected = parse("if minor and senior then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

	
	
		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(""
				+ "(if age < 50 then 2/3 else 1/3)"
				+ "*(if age >= 65 then if senior then 1 else 0 else if senior then 0 else 1)"
				+ "*(if age < 18 then if minor then 1 else 0 else if minor then 0 else 1)"
				+ "*(if age >= 21 and age < 65 then if workingAge then 1 else 0 else if workingAge then 0 else 1)"));
		
		queryExpression = parse("senior");
		evidence = null;
		expected = parse("if senior then 0.233333333 else 0.766666667");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("workingAge");
		evidence = null;
		expected = parse("if workingAge then 0.486666667 else 0.513333333");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor");
		evidence = null;
		expected = parse("if minor then 0.24 else 0.76");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor and senior");
		evidence = null;
		expected = parse("if minor and senior then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);



	
	
		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(""
				+ "(if country = brazil then (if age < 50 then 2/3 else 1/3) else 1/100)" // P(age | country)
				+ "*(if age >= 65 then if senior then 1 else 0 else if senior then 0 else 1)"
				+ "*(if age < 18 then if minor then 1 else 0 else if minor then 0 else 1)"
				+ "*(if age >= 21 and age < 65 then if workingAge then 1 else 0 else if workingAge then 0 else 1)"));
		
		queryExpression = parse("senior");
		evidence = parse("country = brazil");
		expected = parse("if senior then 0.233333333 else 0.766666667");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("workingAge");
		evidence = parse("country = brazil");
		expected = parse("if workingAge then 0.486666667 else 0.513333333");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor");
		evidence = parse("country = brazil");
		expected = parse("if minor then 0.24 else 0.76");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor and senior");
		evidence = parse("country = brazil");
		expected = parse("if minor and senior then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		
		
		
		
		queryExpression = parse("senior");
		evidence = parse("country != brazil");
		expected = parse("if senior then 0.35 else 0.65");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("workingAge");
		evidence = parse("country != brazil");
		expected = parse("if workingAge then 0.44 else 0.56");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor");
		evidence = parse("country != brazil");
		expected = parse("if minor then 0.18 else 0.82");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor and senior");
		evidence = parse("country != brazil");
		expected = parse("if minor and senior then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);
	}

	@Test
	public void differenceArithmetic() {
		
		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = Util.map(
				"Boolean", "2",
				"Country", "200");
	
		additionalTypes = list(new IntegerInterval(0, 99));
		
		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"age",   "Integer",
				"country", "Country",
				"minor", "Boolean",
				"workingAge", "Boolean",
				"senior", "Boolean"
				);
		
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();
	
		mapFromUniquelyNamedConstantNameToTypeName = Util.map("japan", "Country", "brazil", "Country");
		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(""
				+ "(if age >= 65 then if senior then 1 else 0 else if senior then 0 else 1)"
				+ "*(if age < 18 then if minor then 1 else 0 else if minor then 0 else 1)"
				+ "*(if age >= 21 and age < 65 then if workingAge then 1 else 0 else if workingAge then 0 else 1)"));
		
		queryExpression = parse("senior");
		evidence = parse("age >= 0 and age < 100");
		expected = parse("if senior then 0.35 else 0.65");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("workingAge");
		evidence = parse("age >= 0 and age < 100");
		expected = parse("if workingAge then 0.44 else 0.56");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor");
		evidence = parse("age >= 0 and age < 100");
		expected = parse("if minor then 0.18 else 0.82");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor and senior");
		evidence = parse("age >= 0 and age < 100");
		expected = parse("if minor and senior then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

	
	
		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(""
				+ "(if age < 50 then 2/3 else 1/3)"
				+ "*(if age >= 65 then if senior then 1 else 0 else if senior then 0 else 1)"
				+ "*(if age < 18 then if minor then 1 else 0 else if minor then 0 else 1)"
				+ "*(if age >= 21 and age < 65 then if workingAge then 1 else 0 else if workingAge then 0 else 1)"));
		
		queryExpression = parse("senior");
		evidence = parse("age >= 0 and age < 100");
		expected = parse("if senior then 0.233333333 else 0.766666667");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("workingAge");
		evidence = parse("age >= 0 and age < 100");
		expected = parse("if workingAge then 0.486666667 else 0.513333333");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor");
		evidence = parse("age >= 0 and age < 100");
		expected = parse("if minor then 0.24 else 0.76");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor and senior");
		evidence = parse("age >= 0 and age < 100");
		expected = parse("if minor and senior then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);



	
	
		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(""
				+ "(if country = brazil then (if age < 50 then 2/3 else 1/3) else 1/100)" // P(age | country)
				+ "*(if age >= 65 then if senior then 1 else 0 else if senior then 0 else 1)"
				+ "*(if age < 18 then if minor then 1 else 0 else if minor then 0 else 1)"
				+ "*(if age >= 21 and age < 65 then if workingAge then 1 else 0 else if workingAge then 0 else 1)"));
		
		queryExpression = parse("senior");
		evidence = parse("age >= 0 and age < 100 and country = brazil");
		expected = parse("if senior then 0.233333333 else 0.766666667");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("workingAge");
		evidence = parse("age >= 0 and age < 100 and country = brazil");
		expected = parse("if workingAge then 0.486666667 else 0.513333333");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor");
		evidence = parse("age >= 0 and age < 100 and country = brazil");
		expected = parse("if minor then 0.24 else 0.76");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor and senior");
		evidence = parse("age >= 0 and age < 100 and country = brazil");
		expected = parse("if minor and senior then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		
		
		
		
		queryExpression = parse("senior");
		evidence = parse("age >= 0 and age < 100 and country != brazil");
		expected = parse("if senior then 0.35 else 0.65");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("workingAge");
		evidence = parse("age >= 0 and age < 100 and country != brazil");
		expected = parse("if workingAge then 0.44 else 0.56");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor");
		evidence = parse("age >= 0 and age < 100 and country != brazil");
		expected = parse("if minor then 0.18 else 0.82");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		queryExpression = parse("minor and senior");
		evidence = parse("age >= 0 and age < 100 and country != brazil");
		expected = parse("if minor and senior then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);


		queryExpression = parse("minor and senior");
		evidence = parse("age = age and age >= 0 and age < 100 and country != brazil");
		expected = parse("if minor and senior then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);
	}

	@Test
	public void differenceArithmeticOnIntervalsWithMultipleVariables() {
		
		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = Util.map();
	
		additionalTypes = list(new IntegerInterval(0, 99));
		
		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"I",   "0..99",
				"J",   "0..99",
				"K",   "0..99"
				);
		
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();
	
		mapFromUniquelyNamedConstantNameToTypeName = Util.map();
		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(
				"(if J > I then 0.3 else 0.7) *" + 
				"(if K > J then 0.6 else 0.4)"));
		
		queryExpression = parse("I");
		evidence = null;
		expected = parse("if I < 98 then (-(0.04 * I ^ 2) + 23.88 * I + 1520.92) / 257164 else if I < 99 then (-(0.06 * I ^ 2) -(0.18 * I) + 4070.88) / 257164 else (0.14 * I ^ 2 + 0.42 * I + 2079.28) / 257164");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);
	}

	@Test
	public void linearRealArithmeticOnIntervalsWithMultipleVariables() {
		
		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = Util.map();
	
		additionalTypes = list(new RealInterval("[0;100]"));
		
		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"X",   "[0;100]",
				"Y",   "[0;100]",
				"Z",   "[0;100]"
				);
		
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();
	
		mapFromUniquelyNamedConstantNameToTypeName = Util.map();

		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(
				"(if X < 50 then 1 else 2)"));
		
		queryExpression = parse("X");
		evidence = null;
		expected = parse("if X < 50 then 0.00666666667 else 0.0133333333"); // density
		//runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);
	
		queryExpression = parse("X < 50");
		evidence = null;
		expected = parse("if X < 50 then 0.333333333 else 0.666666667"); // probability of the random event
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(
				"(if Y > X then 0.3 else 0.7) *" + 
				"(if Z > Y then 0.6 else 0.4)"));
		
		queryExpression = parse("X");
		evidence = null;
		expected = parse("if X < 100 then (-(0.04 * X ^ 2) + 24 * X + 1500) / 256666.667 else (0.14 * X ^ 2 + 2100) / 256666.667");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

		
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(
				"(if Y > X then 0.3*X else 0.7*X) *" + 
				"(if Z > Y then 0.6*Y else 0.4*Y)"));
		
		queryExpression = parse("X");
		evidence = null;
		expected = parse("if X < 100 then (-(0.0266666667 * X ^ 4) + 12 * X ^ 3 + 70000 * X) / 596666667 else (0.0933333333 * X ^ 4 + 70000 * X) / 596666667");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);

	
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(
				"if X = 10 then 1 else 2"));
		
		queryExpression = parse("X");
		evidence = null;
		expected = parse("if X = 10 then 0.005 else 0.01");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);
	}

	@Test
	public void linearRealArithmeticOnPositionSimple() {
		
		// The definitions of types
		mapFromCategoricalTypeNameToSizeString = Util.map();
	
		additionalTypes = list(new RealInterval("[0;10]"));
		
		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"position",         "[0;10]",
				"observedPosition", "Real"
				);
		
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();
	
		mapFromUniquelyNamedConstantNameToTypeName = Util.map();
	
		isBayesianNetwork = false;
		factors = Times.getMultiplicands(parse(
				"(position)*(if observedPosition > 4 and observedPosition < 5 then 1 else 0)"));
		
		queryExpression = parse("position");
		evidence = null;
		expected = parse("position/50"); // density
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);
	}
	
	/**
	 * @param queryExpression
	 * @param evidence
	 * @param expectedWithNoFactorization
	 * @param expectedWithFactorization
	 * @param isBayesianNetwork
	 * @param factors
	 * @param mapFromRandomVariableNameToTypeName
	 * @param mapFromNonUniquelyNamedConstantNameToTypeName
	 * @param mapFromUniquelyNamedConstantNameToTypeName
	 * @param mapFromCategoricalTypeNameToSizeString
	 * @param additionalTypes TODO
	 */
	private void runTest(Expression queryExpression, Expression evidence, Expression expectedWithNoFactorization, Expression expectedWithFactorization, boolean isBayesianNetwork, List<Expression> factors, Map<String, String> mapFromRandomVariableNameToTypeName, Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName, Map<String, String> mapFromUniquelyNamedConstantNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes) {
		runTestWithFactorizationOption(false, queryExpression, evidence, expectedWithNoFactorization, isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);
		runTestWithFactorizationOption(true,  queryExpression, evidence, expectedWithFactorization,   isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromCategoricalTypeNameToSizeString, additionalTypes);
	}

	/**
	 * @param useFactorization
	 * @param queryExpression
	 * @param evidence
	 * @param expected
	 * @param isBayesianNetwork
	 * @param factorGraph
	 * @param mapFromRandomVariableNameToTypeName
	 * @param mapFromNonUniquelyNamedConstantNameToTypeName
	 * @param mapFromUniquelyNamedConstantNameToTypeName
	 * @param mapFromCategoricalTypeNameToSizeString
	 */
	private void runTestWithFactorizationOption(
			boolean useFactorization,
			Expression queryExpression,
			Expression evidence,
			Expression expected,
			boolean isBayesianNetwork,
			List<Expression> factors,
			Map<String, String> mapFromRandomVariableNameToTypeName,
			Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes) {
		
		HOGMExpressionBasedModel model = new HOGMExpressionBasedModel(
				factors,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isBayesianNetwork);
		
		model = model.getConditionedModel(evidence);
		
		runTest(queryExpression, model, expected, useFactorization);
	}

	private void runTest(
			Expression queryExpression, 
			ExpressionBasedModel model, 
			Expression expected,
			boolean useFactorization) throws AssertionError {
		
		
		ExpressionBasedQuery query = new DefaultExpressionBasedQuery(model, queryExpression);

		ExpressionBasedQuerySolver[] querySolvers = new ExpressionBasedQuerySolver[] {

				new EvaluationExpressionBasedQuerySolver(useFactorization),
				new ExactBPExpressionBasedQuerySolver()
				
		};
		
		for (ExpressionBasedQuerySolver solver : querySolvers) {
			Expression marginal = solver.solve(query);
			checkResult(query, expected, marginal, solver);
		}
	}

	private void checkResult(ExpressionBasedQuery query, Expression expected, Expression marginal, ExpressionBasedQuerySolver solver)
			throws AssertionError {
		TrueContext context = new TrueContext();
		marginal = Expressions.roundToAGivenPrecision(marginal, 9, context);
		expected = Expressions.roundToAGivenPrecision(expected, 9, context);
		if (expected.equals(marginal)) {
			println(solver + " on " + queryExpression + ": passed");
		}
		// check if they are not identical, but equivalent expressions
		else if (query.getContext().evaluate(apply(MINUS, expected, marginal)).equals(ZERO)) { // first attempt was to compare with equality, but this requires a more complete test of equality theory literals to exclude such a complex equality from being considered a literal, which is much more expensive
			// Ok!
		}
		else {
			throw new AssertionError(solver + ": expected:<" + expected + "> but was:<" + marginal + ">, which is not even equivalent.");
		}
	}
}
