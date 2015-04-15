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
	
	@Test
	public void test() {
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		// The definitions of types
		Map<String, String> mapFromTypeNameToSizeString = Util.map(
				"Folks", "10",
				"Boolean", "2");

		// The definitions of variables
		Map<String, String> mapFromVariableNameToTypeName = Util.map(
				"earthquake", "Boolean",
				"burglar",    "Folks", // a multi-value random variable
				"alarm",      "Boolean"
				);

		// The definitions of non-uniquely named constants
		Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = Util.map(
				"seismicLocation", "Boolean"
				);

		Expression bayesianNetwork;
		Expression evidence;
		Expression queryExpression;
		Expression expected;
		
		// a variant of the earthquake/burglary model in which some burglars are more active than others.
		bayesianNetwork = parse("" + 
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
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("not alarm"); // can be any boolean expression
		expected = parse("if earthquake then 0.00145127349 else 0.998548727");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("alarm");
		expected = parse("if earthquake then 0.0289435601 else 0.97105644");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("alarm and burglar = none");
		expected = parse("if earthquake then 0.153846154 else 0.846153846");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if earthquake then 0.01 else 0.99"); // the prior
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("earthquake");
		evidence = parse("true"); // no information
		expected = parse("if earthquake then 0.01 else 0.99"); // the prior
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

	
		
		queryExpression = parse("alarm");
		evidence = null; // no evidence
		expected = parse("if alarm then 0.31095 else 0.68905"); // the marginal
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("not alarm");
		expected = parse("if alarm then 0 else 1");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("alarm");
		expected = parse("if alarm then 1 else 0");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("alarm and burglar = none");
		expected = parse("if alarm then 1 else 0");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if alarm then 0.31095 else 0.68905"); // the marginal
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("alarm");
		evidence = parse("true"); // no information
		expected = parse("if alarm then 0.31095 else 0.68905"); // the marginal
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

	
		
		queryExpression = parse("burglar = none");
		evidence = null; // no evidence
		expected = parse("if burglar = none then 0.7 else 0.3"); // the marginal
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("not alarm");
		expected = parse("if burglar = none then 0.956461795 else 0.0435382048");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("alarm");
		expected = parse("if burglar = none then 0.131693198 else 0.868306802");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("alarm and burglar = none");
		expected = parse("if burglar = none then 1 else 0");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if burglar = none then 0.7 else 0.3"); // the marginal
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = none");
		evidence = parse("true"); // no information
		expected = parse("if burglar = none then 0.7 else 0.3"); // the marginal
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

	
		
		queryExpression = parse("burglar = tom");
		evidence = null; // no evidence
		expected = parse("if burglar = tom then 0.1 else 0.9"); // the marginal
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("not alarm");
		expected = parse("if burglar = tom then 0.0145127349 else 0.985487265");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("alarm");
		expected = parse("if burglar = tom then 0.289435601 else 0.710564399");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("alarm and burglar = none");
		expected = parse("if burglar = tom then 0 else 1");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("alarm or not alarm"); // no information
		expected = parse("if burglar = tom then 0.1 else 0.9"); // the marginal
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		
		queryExpression = parse("burglar = tom");
		evidence = parse("true"); // no information
		expected = parse("if burglar = tom then 0.1 else 0.9"); // the marginal
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

	
	
	
	
		// now using the constant 'seismicLocation'
		bayesianNetwork = parse("" + 
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
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

		queryExpression = parse("earthquake");
		evidence = parse("not alarm"); // can be any boolean expression
		expected = parse("if seismicLocation then if earthquake then 0.0157356412 else 0.984264359 else if earthquake then 0.00145127349 else 0.998548727");
		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);

		// TODO: debug: result changes depending on the value of expected! Both options are correct, but using one leads the result to be the other.
//		queryExpression = parse("burglar = tom");
//		evidence = parse("alarm");
//		expected = parse("if seismicLocation then if burglar = tom then 0.24691358 else 0.75308642 else if burglar = tom then 0.289435601 else 0.710564399");
////		expected = parse("if burglar = tom then if seismicLocation then 0.24691358 else 0.289435601 else if seismicLocation then 0.75308642 else 0.710564399");
//		runTest(queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
	}

	/**
	 * @param queryExpression
	 * @param evidence
	 * @param expected
	 * @param bayesianNetwork
	 * @param mapFromVariableNameToTypeName
	 * @param mapFromNonUniquelyNamedConstantNameToTypeName TODO
	 * @param mapFromTypeNameToSizeString
	 */
	private void runTest(Expression queryExpression, Expression evidence, Expression expected, Expression bayesianNetwork, Map<String, String> mapFromVariableNameToTypeName, Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName, Map<String, String> mapFromTypeNameToSizeString) {
		runTestWithFactorizationOption(false, queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
		runTestWithFactorizationOption(true,  queryExpression, evidence, expected, bayesianNetwork, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
	}

	/**
	 * @param useFactorization
	 * @param queryExpression
	 * @param evidence
	 * @param expected
	 * @param bayesianNetwork
	 * @param mapFromVariableNameToTypeName
	 * @param mapFromNonUniquelyNamedConstantNameToTypeName TODO
	 * @param mapFromTypeNameToSizeString
	 */
	private void runTestWithFactorizationOption(boolean useFactorization, Expression queryExpression, Expression evidence, Expression expected, Expression bayesianNetwork, Map<String, String> mapFromVariableNameToTypeName, Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName, Map<String, String> mapFromTypeNameToSizeString) {
		InferenceForFactorGraphAndEvidence inferencer;
		Expression marginal;
		inferencer = new InferenceForFactorGraphAndEvidence(bayesianNetwork, useFactorization, evidence, useFactorization, mapFromVariableNameToTypeName, mapFromNonUniquelyNamedConstantNameToTypeName, mapFromTypeNameToSizeString);
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
