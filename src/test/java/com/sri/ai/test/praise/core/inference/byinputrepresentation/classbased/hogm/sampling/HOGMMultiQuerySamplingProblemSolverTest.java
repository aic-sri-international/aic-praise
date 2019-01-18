package com.sri.ai.test.praise.core.inference.byinputrepresentation.classbased.hogm.sampling;

import static com.sri.ai.expresso.helper.Expressions.areEqualUpToNumericDifference;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.sampling.HOGMMultiQuerySamplingProblemSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver.HOGMProblemResult;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionSamplingFactor;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.SamplingFactorDiscretizedProbabilityDistributionFunction;
import com.sri.ai.util.function.api.functions.Functions;
import com.sri.ai.util.graph2d.api.GraphSet;

class HOGMMultiQuerySamplingProblemSolverTest {

	/**
	 * If property receives any value, a graph "graph.png" is generated in the current directory with the test's result graph.
	 */
	private static final String PROPERTY_KEY_GENERATING_GRAPH_FILE = "generateGraphFileForHOGMSamplingTest";

	@Test
	public void normalSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"x = Normal(0.0, 15.0);"
				;

		String query = "x";
		Expression expected = parse("if x < -7.5 then 0.12 else if x < -2.5 then 0.249 else if x < 2.5 then 0.267 else if x < 7.5 then 0.249 else 0.114");
		int numberOfInitialSamples = 1000;
		int numberOfDiscreteValues = 5;

		runTest(model, query, expected, numberOfInitialSamples, numberOfDiscreteValues);
	}

	//@Test
	public void normalAndEqualitySamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"y = Normal(0.0, 2);" +
				"x = y;"
				;

		String query = "x";
		Expression expected = parse("if x < -7.5 then 0.12 else if x < -2.5 then 0.249 else if x < 2.5 then 0.267 else if x < 7.5 then 0.249 else 0.114");
		int numberOfInitialSamples = 100000;
		int numberOfDiscreteValues = 50;

		runTest(model, query, expected, numberOfInitialSamples, numberOfDiscreteValues);
	}

	@Test
	public void doubleNormalSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"y = Normal(-9, 0.01);" +
				"x = Normal(y, 1);"
				;

		String query = "x";
		Expression expected = parse("if x < -7.5 then 0.917 else if x < -2.5 then 0.076 else if x < 2.5 then 0.002 else if x < 7.5 then 0.002 else 0.002");
		int numberOfInitialSamples = 1000;
		int numberOfDiscreteValues = 5;

		runTest(model, query, expected, numberOfInitialSamples, numberOfDiscreteValues);
	}

//	@Test
//	public void equalitySamplingTest() {
//
//		String model = "" +
//				"random x : [-10;10];" +
//				"random y : [-10;10];" +
//				"x = y;" +
//				"y = 1.0;"
//				;
//
//		String query = "x";
//		Expression expected = parse("if x = 1.0 then 1 else 0");
//		int numberOfInitialSamples = 100;
//		int numberOfDiscreteValues = 5;
//
//		runTest(model, query, expected, numberOfInitialSamples, numberOfDiscreteValues);
//	}
	
	private void runTest(String model, String query, Expression expected, int numberOfInitialSamples, int numberOfDiscreteValues) {
		HOGMMultiQuerySamplingProblemSolver solver = 
				new HOGMMultiQuerySamplingProblemSolver(
						model, 
						list(query),
						v -> numberOfDiscreteValues, 
						numberOfInitialSamples, 
						new Random());

		List<? extends HOGMProblemResult> results = solver.getResults();
		setPrecisionAndCheckResult(query, expected, results);
	}

	private void setPrecisionAndCheckResult(String query, Expression expected, List<? extends HOGMProblemResult> results)
			throws Error {
		int oldPrecision = ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(3);
		try {
			checkResult(query, expected, results);
		} catch (Throwable t) {
			throw new Error(t);
		}
		finally {
			ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(oldPrecision);
		}
	}

	private void checkResult(String query, Expression expected, List<? extends HOGMProblemResult> results) {
		assertEquals(1, results.size());
		HOGMProblemResult result = getFirst(results);
		assertNoErrors(result);
		printAndCompare(query, result.getResult(), expected);
	}

	private void assertNoErrors(HOGMProblemResult result) {
		result.getErrors().stream().forEach(e -> println(e));
		assertFalse(result.hasErrors());
	}

	private void printAndCompare(String query, Expression resultValue, Expression expected) {
		println("query: " + query);
		println("expected: " + expected);
		println("actual  : " + resultValue);
		if (System.getProperty(PROPERTY_KEY_GENERATING_GRAPH_FILE) != null) {
			ExpressionSamplingFactor expressionSamplingFactor = (ExpressionSamplingFactor) resultValue;
			SamplingFactorDiscretizedProbabilityDistributionFunction function = expressionSamplingFactor.getSamplingFactorDiscretizedProbabilityDistributionFunction();
			Functions functions = Functions.functions(function);
			GraphSet.plot(functions, 0, "graph");
		}
		String reasonForDifference = areEqualUpToNumericDifference(expected, resultValue, 0.1);
		if (reasonForDifference != "") {
			println("Failure: " + reasonForDifference);
		}
		assertEquals("", reasonForDifference);
	}
}
