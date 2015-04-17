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
package com.sri.ai.test.praise.sgsolver;

import static com.sri.ai.util.Util.list;
import static org.junit.Assert.assertEquals;

import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.praise.sgsolver.solver.InferenceForFactorGraphAndEvidence;
import com.sri.ai.test.praise.AbstractLPITest;
import com.sri.ai.util.Util;

public class InferenceForFactorGraphAndEvidenceTest extends AbstractLPITest {
	
	// The definitions of types
	Map<String, String> mapFromTypeNameToSizeString;

	// The definitions of variables
	Map<String, String> mapFromRandomVariableNameToTypeName;

	// The definitions of non-uniquely named constants
	Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName;

	// The definitions of uniquely named constants
	Map<String, String> mapFromUniquelyNamedConstantNameToTypeName;

	boolean isBayesianNetwork;
	Expression factorGraph;
	Expression evidence;
	Expression queryExpression;
	Expression expected;
	Expression expectedWithNoFactorization;
	Expression expectedWithFactorization;

	@Test
	public void test() {
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		// The definitions of types
		mapFromTypeNameToSizeString = Util.map(
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
		mapFromUniquelyNamedConstantNameToTypeName = Util.map();

		// a variant of the earthquake/burglary model in which some burglars are more active than others.
		isBayesianNetwork = true;
		factorGraph = parse("" + 
				"(if earthquake then 0.01 else 0.99) * " +
				"(if burglar = none then 0.7 else if burglar = tom then 0.1 else 0.2 / (|Folks| - 2)) * " +
				// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
				"(if burglar != none or earthquake "
				+    "then if alarm then 0.9 else 0.1 "
				+    "else if alarm then 0.05 else 0.95) " +
				"");

		queryExpression = parse("earthquake");
		evidence = null; // no evidence
		expected = parse("if earthquake then 0.01 else 0.99"); // the prior
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("not alarm"); // can be any boolean expression
		expected = parse("if earthquake then 0.00145127349 else 0.998548727");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("alarm");
		expected = parse("if earthquake then 0.0289435601 else 0.97105644");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("alarm and burglar = none");
		expected = parse("if earthquake then 0.153846154 else 0.846153846");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if earthquake then 0.01 else 0.99"); // the prior
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("true"); // no information
		expected = parse("if earthquake then 0.01 else 0.99"); // the prior
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

	
		
		queryExpression = parse("alarm");
		evidence = null; // no evidence
		expected = parse("if alarm then 0.31095 else 0.68905"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("not alarm");
		expected = parse("if alarm then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("alarm");
		expected = parse("if alarm then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("alarm and burglar = none");
		expected = parse("if alarm then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if alarm then 0.31095 else 0.68905"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("true"); // no information
		expected = parse("if alarm then 0.31095 else 0.68905"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

	
		
		queryExpression = parse("burglar = none");
		evidence = null; // no evidence
		expected = parse("if burglar = none then 0.7 else 0.3"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("not alarm");
		expected = parse("if burglar = none then 0.956461795 else 0.0435382048");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("alarm");
		expected = parse("if burglar = none then 0.131693198 else 0.868306802");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("alarm and burglar = none");
		expected = parse("if burglar = none then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if burglar = none then 0.7 else 0.3"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("true"); // no information
		expected = parse("if burglar = none then 0.7 else 0.3"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

	
		
		queryExpression = parse("burglar = tom");
		evidence = null; // no evidence
		expected = parse("if burglar = tom then 0.1 else 0.9"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("not alarm");
		expected = parse("if burglar = tom then 0.0145127349 else 0.985487265");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("alarm");
		expected = parse("if burglar = tom then 0.289435601 else 0.710564399");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("alarm and burglar = none");
		expected = parse("if burglar = tom then 0 else 1");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if burglar = tom then 0.1 else 0.9"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("true"); // no information
		expected = parse("if burglar = tom then 0.1 else 0.9"); // the marginal
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

	
	
	
	
		// now using the constant 'seismicLocation'
		factorGraph = parse("" + 
				"(if seismicLocation then if earthquake then 0.1 else 0.9 else if earthquake then 0.01 else 0.99) * " +
				"(if burglar = none then 0.7 else if burglar = tom then 0.1 else 0.2 / (|Folks| - 2)) * " +
				// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
				"(if burglar != none or earthquake "
				+    "then if alarm then 0.9 else 0.1 "
				+    "else if alarm then 0.05 else 0.95) " +
				"");

		queryExpression = parse("earthquake");
		evidence = null; // no evidence
		expected = parse("if seismicLocation then if earthquake then 0.1 else 0.9 else if earthquake then 0.01 else 0.99"); // the prior, parameterized by seismicLocation
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

		queryExpression = parse("earthquake");
		evidence = parse("not alarm"); // can be any boolean expression
		expected = parse("if seismicLocation then if earthquake then 0.0157356412 else 0.984264359 else if earthquake then 0.00145127349 else 0.998548727");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

		queryExpression = parse("burglar = tom");
		evidence = parse("alarm");
		// Here factorization creates two distinct but equivalent solutions; eventually we should have an equivalence test (didn't do it now because that would require full relational support even in the absence of random relational variables).
		expectedWithNoFactorization = parse("if seismicLocation then if burglar = tom then 0.24691358 else 0.75308642 else if burglar = tom then 0.289435601 else 0.710564399");
		expectedWithFactorization   = parse("if burglar = tom then if seismicLocation then 0.24691358 else 0.289435601 else if seismicLocation then 0.75308642 else 0.710564399");
		runTest(queryExpression, evidence, expectedWithNoFactorization, expectedWithFactorization, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
	}

	@Test
	public void testBurglary() {
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		// The definitions of types
		mapFromTypeNameToSizeString = Util.map(
				"Boolean", "2");

		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"burglary",   "Boolean",
				"alarm",      "Boolean",
				"call",       "Boolean"
				);

		// The definitions of non-uniquely named constants
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();

		// The definitions of non-uniquely named constants
		mapFromUniquelyNamedConstantNameToTypeName = Util.map();

		isBayesianNetwork = false;
		factorGraph = parse(""
				+ "(if alarm then if call then 0.7 else 0.3 else if call then 0 else 1)*"
				+ "(if burglary then if alarm then 0.9 else 0.1 else if alarm then 0.01 else 0.99)*"
				+ "(if burglary then 0.1 else 0.9)");

		InferenceForFactorGraphAndEvidence inferencer;
		inferencer = new InferenceForFactorGraphAndEvidence(
				factorGraph,
				isBayesianNetwork,
				evidence,
				false,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromTypeNameToSizeString);
		Expression result = inferencer.sum(list(parse("alarm")), factorGraph);
		System.out.println(result);
	}

	@Test
	public void relationalConstants() {

		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		// The definitions of types
		mapFromTypeNameToSizeString = Util.map(
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
		factorGraph = parse(""
				+ "1/|Folks|*"  // uniform prior for 'boss' does not depend on the actual value of 'boss'
				+ "(if boss = tom then if happy then 1 else 0 else if happy then 0 else 1)");

		queryExpression = parse("happy");
		evidence = null; // no evidence
		expected = parse("if happy then 0.1 else 0.9");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

		queryExpression = parse("happy");
		evidence = parse("boss = tom");
		expected = parse("if happy then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

		// Now 'boss' is a constant:

		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"happy", "Boolean"
				);

		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map(
				"boss",  "Folks"
				);

		isBayesianNetwork = true;
		factorGraph = parse("" // no need for a prior for 'boss' now
				+ "(if boss = tom then if happy then 1 else 0 else if happy then 0 else 1)");

		queryExpression = parse("happy");
		evidence = null; // no evidence
		expected = parse("if boss = tom then if happy then 1 else 0 else if happy then 0 else 1"); // query is a function of the constant
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);


		// Now 'boss' is a constant unary predicate:

		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"happy", "Boolean"
				);

		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map(
				"boss",  "'->'(Folks, Boolean)"
				);

		isBayesianNetwork = true;
		factorGraph = parse("" // no need for a prior for 'boss' now
				+ "(if boss(tom) then if happy then 0.9 else 0.1 else if happy then 0.2 else 0.8)");

		queryExpression = parse("happy");
		evidence = null; // no evidence
		expected = parse("if boss(tom) then if happy then 0.9 else 0.1 else if happy then 0.2 else 0.8"); // query is a function of the constant
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

		queryExpression = parse("happy");
		evidence = parse("happy");
		expected = parse("if happy then 1 else 0"); // query is NOT a function of the constant in this case
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
	}

	@Test
	public void lucky() {
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		// The definitions of types
		mapFromTypeNameToSizeString = Util.map(
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
		factorGraph = parse(""
				+ "(if lucky then 1 else 0)*"
				+ "(if lucky then if winner = rodrigo then 1 else 0 else 0.5)");
	
		queryExpression = parse("winner = rodrigo");
		evidence = null;
		expected = parse("if winner = rodrigo then 1 else 0");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
	}
	

	@Test
	public void smartest() {
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		// The definitions of types
		mapFromTypeNameToSizeString = Util.map(
				"People", "1000",
				"Boolean", "2");
	
		// The definitions of variables
		mapFromRandomVariableNameToTypeName = Util.map(
				"boss",   "People",
				"smartest", "People"
				);
		
		mapFromNonUniquelyNamedConstantNameToTypeName = Util.map();

		mapFromUniquelyNamedConstantNameToTypeName = Util.map("bob", "People", "tom", "People");
		
		isBayesianNetwork = false;
		factorGraph = parse(""
				+ "(if (for all P in People :   smartest = P   =>  boss = P) then 1 else 0) * " + 
				"( if smartest != bob then 1 else 0)" + 
				"");
	
		queryExpression = parse("boss = tom");
		evidence = null;
		expected = parse("if boss = tom then 0.001 else 0.999");
		runTest(queryExpression, evidence, expected, expected, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
	}

	/**
	 * @param queryExpression
	 * @param evidence
	 * @param expectedWithNoFactorization
	 * @param expectedWithFactorization
	 * @param isBayesianNetwork
	 * @param factorGraph
	 * @param mapFromRandomVariableNameToTypeName
	 * @param mapFromNonUniquelyNamedConstantNameToTypeName
	 * @param mapFromUniquelyNamedConstantNameToTypeName
	 * @param mapFromTypeNameToSizeString
	 */
	private void runTest(Expression queryExpression, Expression evidence, Expression expectedWithNoFactorization, Expression expectedWithFactorization, boolean isBayesianNetwork, Expression factorGraph, Map<String, String> mapFromRandomVariableNameToTypeName, Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName, Map<String, String> mapFromUniquelyNamedConstantNameToTypeName, Map<String, String> mapFromTypeNameToSizeString) {
		//runTestWithFactorizationOption(false, queryExpression, evidence, expectedWithNoFactorization, isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		runTestWithFactorizationOption(true,  queryExpression, evidence, expectedWithFactorization,   isBayesianNetwork, factorGraph, mapFromRandomVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
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
	 * @param mapFromTypeNameToSizeString
	 */
	private void runTestWithFactorizationOption(
			boolean useFactorization,
			Expression queryExpression,
			Expression evidence,
			Expression expected,
			boolean isBayesianNetwork,
			Expression factorGraph,
			Map<String, String> mapFromRandomVariableNameToTypeName,
			Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromTypeNameToSizeString) {
		
		InferenceForFactorGraphAndEvidence inferencer;
		Expression marginal;
		inferencer = new InferenceForFactorGraphAndEvidence(
				factorGraph,
				isBayesianNetwork,
				evidence,
				useFactorization,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromTypeNameToSizeString);
		marginal = inferencer.solve(queryExpression);
		DefaultRewritingProcess process = new DefaultRewritingProcess(null);
		marginal = Expressions.roundToAGivenPrecision(marginal, 9, process);
		expected = Expressions.roundToAGivenPrecision(expected, 9, process);
		assertEquals(expected, marginal);

// Not working yet, need to debug		
//		Expression negationMarginal;
//		negationMarginal = inferencer.solve(Not.make(queryExpression));
//		negationMarginal = Expressions.roundToAGivenPrecision(negationMarginal, 9, process);
//		expected = inferencer.evaluate(parse(negationMarginal + " = 1 - " + marginal));
//		assertEquals(expected, TRUE);
	}
}
