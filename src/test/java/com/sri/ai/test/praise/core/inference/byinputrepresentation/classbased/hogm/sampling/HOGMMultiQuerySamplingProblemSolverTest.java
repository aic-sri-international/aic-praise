package com.sri.ai.test.praise.core.inference.byinputrepresentation.classbased.hogm.sampling;

import static com.sri.ai.expresso.helper.Expressions.areEqualUpToNumericProportion;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.repeat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.sampling.HOGMMultiQuerySamplingProblemSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver.HOGMProblemResult;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionWithProbabilityFunction;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.NonBooleanFactorError;
import com.sri.ai.util.Util;
import com.sri.ai.util.function.api.functions.Function;
import com.sri.ai.util.function.api.functions.Functions;
import com.sri.ai.util.graph2d.api.GraphSet;

public class HOGMMultiQuerySamplingProblemSolverTest {

	/**
	 * If property receives any value, a graph file named <code>GRAPH_FILENAME_WITHOUT_EXTENSION + ".png"</code>
	 * is generated in the current directory with the test's result graph.
	 */
	private static final String PROPERTY_KEY_GENERATING_GRAPH_FILE = "generateGraphFileForHOGMSamplingTest";
	private static final String GRAPH_FILENAME_WITHOUT_EXTENSION = "HOGMMultiQuerySamplingProblemSolverTest";

	@Test
	public void emptyModelOnRealTest() {

		String model = "" +
				"random x : [-10;10];"
				;

		String query = "x";
		Expression expected = parse("if x < -9.583 then 0.021 else if x < -8.75 then 0.042 else if x < -7.917 then 0.041 else if x < -7.083 then 0.042 else if x < -6.25 then 0.042 else if x < -5.417 then 0.041 else if x < -4.583 then 0.042 else if x < -3.75 then 0.042 else if x < -2.917 then 0.042 else if x < -2.083 then 0.042 else if x < -1.25 then 0.042 else if x < -0.417 then 0.042 else if x < 0.417 then 0.042 else if x < 1.25 then 0.042 else if x < 2.083 then 0.041 else if x < 2.917 then 0.042 else if x < 3.75 then 0.041 else if x < 4.583 then 0.042 else if x < 5.417 then 0.042 else if x < 6.25 then 0.042 else if x < 7.083 then 0.041 else if x < 7.917 then 0.042 else if x < 8.75 then 0.042 else if x < 9.583 then 0.042 else 0.021");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void emptyModelOnIntegerTest() {

		String model = "" +
				"random i : 0..10;"+
				""
				;

		String query = "i";
		Expression expected = parse("if i = 0 then 0.091 else if i = 1 then 0.089 else if i = 2 then 0.09 else if i = 3 then 0.093 else if i = 4 then 0.089 else if i = 5 then 0.091 else if i = 6 then 0.092 else if i = 7 then 0.093 else if i = 8 then 0.091 else if i = 9 then 0.091 else 0.09");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void emptyModelOnCategoricalTest() {

		String model = "" +
				"sort Person: 4, bob, mary;" +
				"random neighbor : Person;" +
				"";

		String query = "neighbor";
		Expression expected = parse("if neighbor = bob then 0.25 else if neighbor = mary then 0.25 else if neighbor = person3 then 0.25 else 0.25");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph - waiting for bar chart implementation */ false);
	}

	@Test
	public void ifThenElseSamplingTest() {

		String model = "" +
				"random p : Boolean;" +
				"random x : [-10;10];" +
				"x = Normal(0.0, 3.0);" +
				"if p then x > -3 and x < -1 else x > 1 and x < 2;" +
				"p;" +
				"";

		String query = "x";
		Expression expected = parse("if x < -9.796 then 0 else if x < -9.388 then 0 else if x < -8.98 then 0 else if x < -8.571 then 0 else if x < -8.163 then 0 else if x < -7.755 then 0 else if x < -7.347 then 0 else if x < -6.939 then 0 else if x < -6.531 then 0 else if x < -6.122 then 0 else if x < -5.714 then 0 else if x < -5.306 then 0 else if x < -4.898 then 0 else if x < -4.49 then 0 else if x < -4.082 then 0 else if x < -3.673 then 0 else if x < -3.265 then 0 else if x < -2.857 then 0.053 else if x < -2.449 then 0.169 else if x < -2.041 then 0.198 else if x < -1.633 then 0.214 else if x < -1.224 then 0.235 else if x < -0.816 then 0.13 else if x < -0.408 then 0 else if x < -1.2E-15 then 0 else if x < 0.408 then 0 else if x < 0.816 then 0 else if x < 1.224 then 0 else if x < 1.633 then 0 else if x < 2.041 then 0 else if x < 2.449 then 0 else if x < 2.857 then 0 else if x < 3.265 then 0 else if x < 3.673 then 0 else if x < 4.082 then 0 else if x < 4.49 then 0 else if x < 4.898 then 0 else if x < 5.306 then 0 else if x < 5.714 then 0 else if x < 6.122 then 0 else if x < 6.531 then 0 else if x < 6.939 then 0 else if x < 7.347 then 0 else if x < 7.755 then 0 else if x < 8.163 then 0 else if x < 8.571 then 0 else if x < 8.98 then 0 else if x < 9.388 then 0 else if x < 9.796 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 50;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void ifThenElseInverseSamplingTest() {

		String model = "" +
				"random p : Boolean;" +
				"random x : [-10;10];" +
				"x = -2;" +
				"if p then x > -3 and x < -1 else x > 1 and x < 2;" +
				"";

		String query = "p";
		Expression expected = parse("if p then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 50;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void ifThenElseIndeterminateInverseSamplingTest() {

		String model = "" +
				"random p : Boolean;" +
				"random x : [-10;10];" +
				"x = -2;" +
				"if p then x > -3 and x < -1 else x = -2;" +
				"";

		String query = "p";
		Expression expected = parse("if p then 1 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 50;

		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
		}
		catch (Throwable e) {
			println(e);
			if (!e.getMessage().contains("Could not sample p")) {
				throw e;
			}
		}
	}

	@Test
	public void ifThenElseSalvageableIndeterminateInverseSamplingTest() {

		String model = "" +
				"random p : Boolean;" +
				"random x : [-10;10];" +
				"x = -2;" +
				"if p then x > -3 and x < -1 else x = -2;" +
				"p;" +
				"";

		String query = "p";
		Expression expected = parse("if p then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 50;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void ifThenElseFunctionSamplingTest() {

		String model = "" +
				"random p : Boolean;" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"x = Normal(0.0, 3.0);" +
				"y = if p then x else 3;" +
				"p;" +
				"";

		String query = "y";
		Expression expected = parse("if y < -9.796 then 0 else if y < -9.388 then 0 else if y < -8.98 then 0.001 else if y < -8.571 then 0.001 else if y < -8.163 then 0.001 else if y < -7.755 then 0.001 else if y < -7.347 then 0.002 else if y < -6.939 then 0.003 else if y < -6.531 then 0.004 else if y < -6.122 then 0.006 else if y < -5.714 then 0.008 else if y < -5.306 then 0.01 else if y < -4.898 then 0.013 else if y < -4.49 then 0.016 else if y < -4.082 then 0.019 else if y < -3.673 then 0.023 else if y < -3.265 then 0.028 else if y < -2.857 then 0.032 else if y < -2.449 then 0.038 else if y < -2.041 then 0.04 else if y < -1.633 then 0.045 else if y < -1.224 then 0.049 else if y < -0.816 then 0.051 else if y < -0.408 then 0.053 else if y < -1.2E-15 then 0.054 else if y < 0.408 then 0.055 else if y < 0.816 then 0.053 else if y < 1.224 then 0.051 else if y < 1.633 then 0.048 else if y < 2.041 then 0.045 else if y < 2.449 then 0.041 else if y < 2.857 then 0.036 else if y < 3.265 then 0.032 else if y < 3.673 then 0.029 else if y < 4.082 then 0.023 else if y < 4.49 then 0.02 else if y < 4.898 then 0.016 else if y < 5.306 then 0.013 else if y < 5.714 then 0.01 else if y < 6.122 then 0.008 else if y < 6.531 then 0.006 else if y < 6.939 then 0.005 else if y < 7.347 then 0.003 else if y < 7.755 then 0.002 else if y < 8.163 then 0.002 else if y < 8.571 then 0.001 else if y < 8.98 then 0.001 else if y < 9.388 then 0 else if y < 9.796 then 0 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 50;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void normalSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"x = Normal(0.0, 3.0);"
				;

		String query = "x";
		Expression expected = parse("if x < -9.583 then 0 else if x < -8.75 then 0.001 else if x < -7.917 then 0.002 else if x < -7.083 then 0.005 else if x < -6.25 then 0.009 else if x < -5.417 then 0.017 else if x < -4.583 then 0.028 else if x < -3.75 then 0.042 else if x < -2.917 then 0.059 else if x < -2.083 then 0.08 else if x < -1.25 then 0.095 else if x < -0.417 then 0.107 else if x < 0.417 then 0.111 else if x < 1.25 then 0.106 else if x < 2.083 then 0.095 else if x < 2.917 then 0.078 else if x < 3.75 then 0.059 else if x < 4.583 then 0.042 else if x < 5.417 then 0.028 else if x < 6.25 then 0.016 else if x < 7.083 then 0.01 else if x < 7.917 then 0.005 else if x < 8.75 then 0.003 else if x < 9.583 then 0.001 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test 
	public void queryWithTwoVariablesSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"constant b : [-10;10];" +
				"x = Normal(b, 3.0);" +
				"";

		String query = "x";
		boolean quantitativeTests = false; // too large to parse due to a bug (not to generate)
		Expression expected = null; // parse("if (x < -9.583) and (b < -9.583) then 0.109 else if (x < -9.583) and (b < -8.75) then 0.089 else if (x < -9.583) and (b < -7.917) then 0.068 else if (x < -9.583) and (b < -7.083) then 0.051 else if (x < -9.583) and (b < -6.25) then 0.036 else if (x < -9.583) and (b < -5.417) then 0.026 else if (x < -9.583) and (b < -4.583) then 0.016 else if (x < -9.583) and (b < -3.75) then 0.009 else if (x < -9.583) and (b < -2.917) then 0.006 else if (x < -9.583) and (b < -2.083) then 0.003 else if (x < -9.583) and (b < -1.25) then 0.001 else if (x < -9.583) and (b < -0.417) then 0.001 else if (x < -9.583) and (b < 0.417) then 0 else if (x < -9.583) and (b < 1.25) then 0 else if (x < -9.583) and (b < 2.083) then 0 else if (x < -9.583) and (b < 2.917) then 0 else if (x < -9.583) and (b < 3.75) then 0 else if (x < -9.583) and (b < 4.583) then 0 else if (x < -9.583) and (b < 5.417) then 0 else if (x < -9.583) and (b < 6.25) then 0 else if (x < -9.583) and (b < 7.083) then 0 else if (x < -9.583) and (b < 7.917) then 0 else if (x < -9.583) and (b < 8.75) then 0 else if (x < -9.583) and (b < 9.583) then 0 else if (x < -9.583) and (b < 10) then 0 else if (x < -8.75) and (b < -9.583) then 0.212 else if (x < -8.75) and (b < -8.75) then 0.181 else if (x < -8.75) and (b < -7.917) then 0.149 else if (x < -8.75) and (b < -7.083) then 0.119 else if (x < -8.75) and (b < -6.25) then 0.09 else if (x < -8.75) and (b < -5.417) then 0.066 else if (x < -8.75) and (b < -4.583) then 0.044 else if (x < -8.75) and (b < -3.75) then 0.028 else if (x < -8.75) and (b < -2.917) then 0.017 else if (x < -8.75) and (b < -2.083) then 0.009 else if (x < -8.75) and (b < -1.25) then 0.005 else if (x < -8.75) and (b < -0.417) then 0.002 else if (x < -8.75) and (b < 0.417) then 0.001 else if (x < -8.75) and (b < 1.25) then 0 else if (x < -8.75) and (b < 2.083) then 0 else if (x < -8.75) and (b < 2.917) then 0 else if (x < -8.75) and (b < 3.75) then 0 else if (x < -8.75) and (b < 4.583) then 0 else if (x < -8.75) and (b < 5.417) then 0 else if (x < -8.75) and (b < 6.25) then 0 else if (x < -8.75) and (b < 7.083) then 0 else if (x < -8.75) and (b < 7.917) then 0 else if (x < -8.75) and (b < 8.75) then 0 else if (x < -8.75) and (b < 9.583) then 0 else if (x < -8.75) and (b < 10) then 0 else if (x < -7.917) and (b < -9.583) then 0.19 else if (x < -7.917) and (b < -8.75) then 0.174 else if (x < -7.917) and (b < -7.917) then 0.156 else if (x < -7.917) and (b < -7.083) then 0.135 else if (x < -7.917) and (b < -6.25) then 0.109 else if (x < -7.917) and (b < -5.417) then 0.086 else if (x < -7.917) and (b < -4.583) then 0.063 else if (x < -7.917) and (b < -3.75) then 0.044 else if (x < -7.917) and (b < -2.917) then 0.028 else if (x < -7.917) and (b < -2.083) then 0.018 else if (x < -7.917) and (b < -1.25) then 0.01 else if (x < -7.917) and (b < -0.417) then 0.005 else if (x < -7.917) and (b < 0.417) then 0.002 else if (x < -7.917) and (b < 1.25) then 0.001 else if (x < -7.917) and (b < 2.083) then 0 else if (x < -7.917) and (b < 2.917) then 0 else if (x < -7.917) and (b < 3.75) then 0 else if (x < -7.917) and (b < 4.583) then 0 else if (x < -7.917) and (b < 5.417) then 0 else if (x < -7.917) and (b < 6.25) then 0 else if (x < -7.917) and (b < 7.083) then 0 else if (x < -7.917) and (b < 7.917) then 0 else if (x < -7.917) and (b < 8.75) then 0 else if (x < -7.917) and (b < 9.583) then 0 else if (x < -7.917) and (b < 10) then 0 else if (x < -7.083) and (b < -9.583) then 0.159 else if (x < -7.083) and (b < -8.75) then 0.154 else if (x < -7.083) and (b < -7.917) then 0.149 else if (x < -7.083) and (b < -7.083) then 0.139 else if (x < -7.083) and (b < -6.25) then 0.122 else if (x < -7.083) and (b < -5.417) then 0.104 else if (x < -7.083) and (b < -4.583) then 0.082 else if (x < -7.083) and (b < -3.75) then 0.062 else if (x < -7.083) and (b < -2.917) then 0.044 else if (x < -7.083) and (b < -2.083) then 0.028 else if (x < -7.083) and (b < -1.25) then 0.017 else if (x < -7.083) and (b < -0.417) then 0.01 else if (x < -7.083) and (b < 0.417) then 0.005 else if (x < -7.083) and (b < 1.25) then 0.003 else if (x < -7.083) and (b < 2.083) then 0.001 else if (x < -7.083) and (b < 2.917) then 0.001 else if (x < -7.083) and (b < 3.75) then 0 else if (x < -7.083) and (b < 4.583) then 0 else if (x < -7.083) and (b < 5.417) then 0 else if (x < -7.083) and (b < 6.25) then 0 else if (x < -7.083) and (b < 7.083) then 0 else if (x < -7.083) and (b < 7.917) then 0 else if (x < -7.083) and (b < 8.75) then 0 else if (x < -7.083) and (b < 9.583) then 0 else if (x < -7.083) and (b < 10) then 0 else if (x < -6.25) and (b < -9.583) then 0.122 else if (x < -6.25) and (b < -8.75) then 0.129 else if (x < -6.25) and (b < -7.917) then 0.135 else if (x < -6.25) and (b < -7.083) then 0.133 else if (x < -6.25) and (b < -6.25) then 0.129 else if (x < -6.25) and (b < -5.417) then 0.115 else if (x < -6.25) and (b < -4.583) then 0.1 else if (x < -6.25) and (b < -3.75) then 0.081 else if (x < -6.25) and (b < -2.917) then 0.061 else if (x < -6.25) and (b < -2.083) then 0.043 else if (x < -6.25) and (b < -1.25) then 0.028 else if (x < -6.25) and (b < -0.417) then 0.017 else if (x < -6.25) and (b < 0.417) then 0.01 else if (x < -6.25) and (b < 1.25) then 0.005 else if (x < -6.25) and (b < 2.083) then 0.002 else if (x < -6.25) and (b < 2.917) then 0.001 else if (x < -6.25) and (b < 3.75) then 0 else if (x < -6.25) and (b < 4.583) then 0 else if (x < -6.25) and (b < 5.417) then 0 else if (x < -6.25) and (b < 6.25) then 0 else if (x < -6.25) and (b < 7.083) then 0 else if (x < -6.25) and (b < 7.917) then 0 else if (x < -6.25) and (b < 8.75) then 0 else if (x < -6.25) and (b < 9.583) then 0 else if (x < -6.25) and (b < 10) then 0 else if (x < -5.417) and (b < -9.583) then 0.083 else if (x < -5.417) and (b < -8.75) then 0.1 else if (x < -5.417) and (b < -7.917) then 0.11 else if (x < -5.417) and (b < -7.083) then 0.119 else if (x < -5.417) and (b < -6.25) then 0.122 else if (x < -5.417) and (b < -5.417) then 0.117 else if (x < -5.417) and (b < -4.583) then 0.113 else if (x < -5.417) and (b < -3.75) then 0.096 else if (x < -5.417) and (b < -2.917) then 0.079 else if (x < -5.417) and (b < -2.083) then 0.059 else if (x < -5.417) and (b < -1.25) then 0.044 else if (x < -5.417) and (b < -0.417) then 0.028 else if (x < -5.417) and (b < 0.417) then 0.017 else if (x < -5.417) and (b < 1.25) then 0.01 else if (x < -5.417) and (b < 2.083) then 0.005 else if (x < -5.417) and (b < 2.917) then 0.002 else if (x < -5.417) and (b < 3.75) then 0.001 else if (x < -5.417) and (b < 4.583) then 0 else if (x < -5.417) and (b < 5.417) then 0 else if (x < -5.417) and (b < 6.25) then 0 else if (x < -5.417) and (b < 7.083) then 0 else if (x < -5.417) and (b < 7.917) then 0 else if (x < -5.417) and (b < 8.75) then 0 else if (x < -5.417) and (b < 9.583) then 0 else if (x < -5.417) and (b < 10) then 0 else if (x < -4.583) and (b < -9.583) then 0.055 else if (x < -4.583) and (b < -8.75) then 0.068 else if (x < -4.583) and (b < -7.917) then 0.083 else if (x < -4.583) and (b < -7.083) then 0.097 else if (x < -4.583) and (b < -6.25) then 0.108 else if (x < -4.583) and (b < -5.417) then 0.116 else if (x < -4.583) and (b < -4.583) then 0.114 else if (x < -4.583) and (b < -3.75) then 0.107 else if (x < -4.583) and (b < -2.917) then 0.094 else if (x < -4.583) and (b < -2.083) then 0.08 else if (x < -4.583) and (b < -1.25) then 0.06 else if (x < -4.583) and (b < -0.417) then 0.042 else if (x < -4.583) and (b < 0.417) then 0.028 else if (x < -4.583) and (b < 1.25) then 0.017 else if (x < -4.583) and (b < 2.083) then 0.01 else if (x < -4.583) and (b < 2.917) then 0.005 else if (x < -4.583) and (b < 3.75) then 0.003 else if (x < -4.583) and (b < 4.583) then 0.001 else if (x < -4.583) and (b < 5.417) then 0.001 else if (x < -4.583) and (b < 6.25) then 0 else if (x < -4.583) and (b < 7.083) then 0 else if (x < -4.583) and (b < 7.917) then 0 else if (x < -4.583) and (b < 8.75) then 0 else if (x < -4.583) and (b < 9.583) then 0 else if (x < -4.583) and (b < 10) then 0 else if (x < -3.75) and (b < -9.583) then 0.033 else if (x < -3.75) and (b < -8.75) then 0.047 else if (x < -3.75) and (b < -7.917) then 0.06 else if (x < -3.75) and (b < -7.083) then 0.074 else if (x < -3.75) and (b < -6.25) then 0.09 else if (x < -3.75) and (b < -5.417) then 0.105 else if (x < -3.75) and (b < -4.583) then 0.111 else if (x < -3.75) and (b < -3.75) then 0.112 else if (x < -3.75) and (b < -2.917) then 0.108 else if (x < -3.75) and (b < -2.083) then 0.095 else if (x < -3.75) and (b < -1.25) then 0.078 else if (x < -3.75) and (b < -0.417) then 0.06 else if (x < -3.75) and (b < 0.417) then 0.041 else if (x < -3.75) and (b < 1.25) then 0.028 else if (x < -3.75) and (b < 2.083) then 0.018 else if (x < -3.75) and (b < 2.917) then 0.01 else if (x < -3.75) and (b < 3.75) then 0.005 else if (x < -3.75) and (b < 4.583) then 0.002 else if (x < -3.75) and (b < 5.417) then 0.001 else if (x < -3.75) and (b < 6.25) then 0 else if (x < -3.75) and (b < 7.083) then 0 else if (x < -3.75) and (b < 7.917) then 0 else if (x < -3.75) and (b < 8.75) then 0 else if (x < -3.75) and (b < 9.583) then 0 else if (x < -3.75) and (b < 10) then 0 else if (x < -2.917) and (b < -9.583) then 0.02 else if (x < -2.917) and (b < -8.75) then 0.028 else if (x < -2.917) and (b < -7.917) then 0.04 else if (x < -2.917) and (b < -7.083) then 0.053 else if (x < -2.917) and (b < -6.25) then 0.07 else if (x < -2.917) and (b < -5.417) then 0.087 else if (x < -2.917) and (b < -4.583) then 0.1 else if (x < -2.917) and (b < -3.75) then 0.11 else if (x < -2.917) and (b < -2.917) then 0.111 else if (x < -2.917) and (b < -2.083) then 0.105 else if (x < -2.917) and (b < -1.25) then 0.096 else if (x < -2.917) and (b < -0.417) then 0.079 else if (x < -2.917) and (b < 0.417) then 0.061 else if (x < -2.917) and (b < 1.25) then 0.041 else if (x < -2.917) and (b < 2.083) then 0.028 else if (x < -2.917) and (b < 2.917) then 0.017 else if (x < -2.917) and (b < 3.75) then 0.009 else if (x < -2.917) and (b < 4.583) then 0.005 else if (x < -2.917) and (b < 5.417) then 0.002 else if (x < -2.917) and (b < 6.25) then 0.001 else if (x < -2.917) and (b < 7.083) then 0 else if (x < -2.917) and (b < 7.917) then 0 else if (x < -2.917) and (b < 8.75) then 0 else if (x < -2.917) and (b < 9.583) then 0 else if (x < -2.917) and (b < 10) then 0 else if (x < -2.083) and (b < -9.583) then 0.01 else if (x < -2.083) and (b < -8.75) then 0.016 else if (x < -2.083) and (b < -7.917) then 0.024 else if (x < -2.083) and (b < -7.083) then 0.035 else if (x < -2.083) and (b < -6.25) then 0.049 else if (x < -2.083) and (b < -5.417) then 0.066 else if (x < -2.083) and (b < -4.583) then 0.082 else if (x < -2.083) and (b < -3.75) then 0.098 else if (x < -2.083) and (b < -2.917) then 0.108 else if (x < -2.083) and (b < -2.083) then 0.113 else if (x < -2.083) and (b < -1.25) then 0.105 else if (x < -2.083) and (b < -0.417) then 0.097 else if (x < -2.083) and (b < 0.417) then 0.078 else if (x < -2.083) and (b < 1.25) then 0.059 else if (x < -2.083) and (b < 2.083) then 0.043 else if (x < -2.083) and (b < 2.917) then 0.028 else if (x < -2.083) and (b < 3.75) then 0.017 else if (x < -2.083) and (b < 4.583) then 0.01 else if (x < -2.083) and (b < 5.417) then 0.005 else if (x < -2.083) and (b < 6.25) then 0.003 else if (x < -2.083) and (b < 7.083) then 0.001 else if (x < -2.083) and (b < 7.917) then 0.001 else if (x < -2.083) and (b < 8.75) then 0 else if (x < -2.083) and (b < 9.583) then 0 else if (x < -2.083) and (b < 10) then 0 else if (x < -1.25) and (b < -9.583) then 0.005 else if (x < -1.25) and (b < -8.75) then 0.008 else if (x < -1.25) and (b < -7.917) then 0.013 else if (x < -1.25) and (b < -7.083) then 0.022 else if (x < -1.25) and (b < -6.25) then 0.034 else if (x < -1.25) and (b < -5.417) then 0.044 else if (x < -1.25) and (b < -4.583) then 0.063 else if (x < -1.25) and (b < -3.75) then 0.083 else if (x < -1.25) and (b < -2.917) then 0.097 else if (x < -1.25) and (b < -2.083) then 0.108 else if (x < -1.25) and (b < -1.25) then 0.111 else if (x < -1.25) and (b < -0.417) then 0.104 else if (x < -1.25) and (b < 0.417) then 0.096 else if (x < -1.25) and (b < 1.25) then 0.079 else if (x < -1.25) and (b < 2.083) then 0.059 else if (x < -1.25) and (b < 2.917) then 0.044 else if (x < -1.25) and (b < 3.75) then 0.028 else if (x < -1.25) and (b < 4.583) then 0.017 else if (x < -1.25) and (b < 5.417) then 0.01 else if (x < -1.25) and (b < 6.25) then 0.005 else if (x < -1.25) and (b < 7.083) then 0.003 else if (x < -1.25) and (b < 7.917) then 0.001 else if (x < -1.25) and (b < 8.75) then 0.001 else if (x < -1.25) and (b < 9.583) then 0 else if (x < -1.25) and (b < 10) then 0 else if (x < -0.417) and (b < -9.583) then 0.002 else if (x < -0.417) and (b < -8.75) then 0.005 else if (x < -0.417) and (b < -7.917) then 0.007 else if (x < -0.417) and (b < -7.083) then 0.012 else if (x < -0.417) and (b < -6.25) then 0.019 else if (x < -0.417) and (b < -5.417) then 0.03 else if (x < -0.417) and (b < -4.583) then 0.045 else if (x < -0.417) and (b < -3.75) then 0.062 else if (x < -0.417) and (b < -2.917) then 0.079 else if (x < -0.417) and (b < -2.083) then 0.096 else if (x < -0.417) and (b < -1.25) then 0.107 else if (x < -0.417) and (b < -0.417) then 0.11 else if (x < -0.417) and (b < 0.417) then 0.107 else if (x < -0.417) and (b < 1.25) then 0.096 else if (x < -0.417) and (b < 2.083) then 0.08 else if (x < -0.417) and (b < 2.917) then 0.061 else if (x < -0.417) and (b < 3.75) then 0.043 else if (x < -0.417) and (b < 4.583) then 0.028 else if (x < -0.417) and (b < 5.417) then 0.017 else if (x < -0.417) and (b < 6.25) then 0.01 else if (x < -0.417) and (b < 7.083) then 0.006 else if (x < -0.417) and (b < 7.917) then 0.003 else if (x < -0.417) and (b < 8.75) then 0.001 else if (x < -0.417) and (b < 9.583) then 0.001 else if (x < -0.417) and (b < 10) then 0 else if (x < 0.417) and (b < -9.583) then 0.001 else if (x < 0.417) and (b < -8.75) then 0.002 else if (x < 0.417) and (b < -7.917) then 0.004 else if (x < 0.417) and (b < -7.083) then 0.006 else if (x < 0.417) and (b < -6.25) then 0.011 else if (x < 0.417) and (b < -5.417) then 0.018 else if (x < 0.417) and (b < -4.583) then 0.029 else if (x < 0.417) and (b < -3.75) then 0.043 else if (x < 0.417) and (b < -2.917) then 0.06 else if (x < 0.417) and (b < -2.083) then 0.078 else if (x < 0.417) and (b < -1.25) then 0.095 else if (x < 0.417) and (b < -0.417) then 0.106 else if (x < 0.417) and (b < 0.417) then 0.111 else if (x < 0.417) and (b < 1.25) then 0.105 else if (x < 0.417) and (b < 2.083) then 0.094 else if (x < 0.417) and (b < 2.917) then 0.079 else if (x < 0.417) and (b < 3.75) then 0.06 else if (x < 0.417) and (b < 4.583) then 0.043 else if (x < 0.417) and (b < 5.417) then 0.03 else if (x < 0.417) and (b < 6.25) then 0.018 else if (x < 0.417) and (b < 7.083) then 0.011 else if (x < 0.417) and (b < 7.917) then 0.006 else if (x < 0.417) and (b < 8.75) then 0.004 else if (x < 0.417) and (b < 9.583) then 0.002 else if (x < 0.417) and (b < 10) then 0.001 else if (x < 1.25) and (b < -9.583) then 0 else if (x < 1.25) and (b < -8.75) then 0.001 else if (x < 1.25) and (b < -7.917) then 0.001 else if (x < 1.25) and (b < -7.083) then 0.003 else if (x < 1.25) and (b < -6.25) then 0.006 else if (x < 1.25) and (b < -5.417) then 0.01 else if (x < 1.25) and (b < -4.583) then 0.018 else if (x < 1.25) and (b < -3.75) then 0.029 else if (x < 1.25) and (b < -2.917) then 0.043 else if (x < 1.25) and (b < -2.083) then 0.06 else if (x < 1.25) and (b < -1.25) then 0.077 else if (x < 1.25) and (b < -0.417) then 0.095 else if (x < 1.25) and (b < 0.417) then 0.106 else if (x < 1.25) and (b < 1.25) then 0.11 else if (x < 1.25) and (b < 2.083) then 0.108 else if (x < 1.25) and (b < 2.917) then 0.096 else if (x < 1.25) and (b < 3.75) then 0.077 else if (x < 1.25) and (b < 4.583) then 0.061 else if (x < 1.25) and (b < 5.417) then 0.045 else if (x < 1.25) and (b < 6.25) then 0.03 else if (x < 1.25) and (b < 7.083) then 0.02 else if (x < 1.25) and (b < 7.917) then 0.012 else if (x < 1.25) and (b < 8.75) then 0.008 else if (x < 1.25) and (b < 9.583) then 0.004 else if (x < 1.25) and (b < 10) then 0.002 else if (x < 2.083) and (b < -9.583) then 0 else if (x < 2.083) and (b < -8.75) then 0 else if (x < 2.083) and (b < -7.917) then 0.001 else if (x < 2.083) and (b < -7.083) then 0.001 else if (x < 2.083) and (b < -6.25) then 0.003 else if (x < 2.083) and (b < -5.417) then 0.005 else if (x < 2.083) and (b < -4.583) then 0.01 else if (x < 2.083) and (b < -3.75) then 0.017 else if (x < 2.083) and (b < -2.917) then 0.028 else if (x < 2.083) and (b < -2.083) then 0.042 else if (x < 2.083) and (b < -1.25) then 0.061 else if (x < 2.083) and (b < -0.417) then 0.078 else if (x < 2.083) and (b < 0.417) then 0.095 else if (x < 2.083) and (b < 1.25) then 0.107 else if (x < 2.083) and (b < 2.083) then 0.112 else if (x < 2.083) and (b < 2.917) then 0.107 else if (x < 2.083) and (b < 3.75) then 0.097 else if (x < 2.083) and (b < 4.583) then 0.08 else if (x < 2.083) and (b < 5.417) then 0.063 else if (x < 2.083) and (b < 6.25) then 0.046 else if (x < 2.083) and (b < 7.083) then 0.033 else if (x < 2.083) and (b < 7.917) then 0.022 else if (x < 2.083) and (b < 8.75) then 0.014 else if (x < 2.083) and (b < 9.583) then 0.009 else if (x < 2.083) and (b < 10) then 0.005 else if (x < 2.917) and (b < -9.583) then 0 else if (x < 2.917) and (b < -8.75) then 0 else if (x < 2.917) and (b < -7.917) then 0 else if (x < 2.917) and (b < -7.083) then 0.001 else if (x < 2.917) and (b < -6.25) then 0.001 else if (x < 2.917) and (b < -5.417) then 0.002 else if (x < 2.917) and (b < -4.583) then 0.005 else if (x < 2.917) and (b < -3.75) then 0.01 else if (x < 2.917) and (b < -2.917) then 0.018 else if (x < 2.917) and (b < -2.083) then 0.028 else if (x < 2.917) and (b < -1.25) then 0.041 else if (x < 2.917) and (b < -0.417) then 0.061 else if (x < 2.917) and (b < 0.417) then 0.078 else if (x < 2.917) and (b < 1.25) then 0.093 else if (x < 2.917) and (b < 2.083) then 0.107 else if (x < 2.917) and (b < 2.917) then 0.11 else if (x < 2.917) and (b < 3.75) then 0.107 else if (x < 2.917) and (b < 4.583) then 0.097 else if (x < 2.917) and (b < 5.417) then 0.082 else if (x < 2.917) and (b < 6.25) then 0.065 else if (x < 2.917) and (b < 7.083) then 0.049 else if (x < 2.917) and (b < 7.917) then 0.036 else if (x < 2.917) and (b < 8.75) then 0.023 else if (x < 2.917) and (b < 9.583) then 0.015 else if (x < 2.917) and (b < 10) then 0.01 else if (x < 3.75) and (b < -9.583) then 0 else if (x < 3.75) and (b < -8.75) then 0 else if (x < 3.75) and (b < -7.917) then 0 else if (x < 3.75) and (b < -7.083) then 0 else if (x < 3.75) and (b < -6.25) then 0 else if (x < 3.75) and (b < -5.417) then 0.001 else if (x < 3.75) and (b < -4.583) then 0.003 else if (x < 3.75) and (b < -3.75) then 0.005 else if (x < 3.75) and (b < -2.917) then 0.01 else if (x < 3.75) and (b < -2.083) then 0.017 else if (x < 3.75) and (b < -1.25) then 0.027 else if (x < 3.75) and (b < -0.417) then 0.042 else if (x < 3.75) and (b < 0.417) then 0.059 else if (x < 3.75) and (b < 1.25) then 0.079 else if (x < 3.75) and (b < 2.083) then 0.094 else if (x < 3.75) and (b < 2.917) then 0.106 else if (x < 3.75) and (b < 3.75) then 0.113 else if (x < 3.75) and (b < 4.583) then 0.109 else if (x < 3.75) and (b < 5.417) then 0.1 else if (x < 3.75) and (b < 6.25) then 0.085 else if (x < 3.75) and (b < 7.083) then 0.068 else if (x < 3.75) and (b < 7.917) then 0.053 else if (x < 3.75) and (b < 8.75) then 0.039 else if (x < 3.75) and (b < 9.583) then 0.028 else if (x < 3.75) and (b < 10) then 0.019 else if (x < 4.583) and (b < -9.583) then 0 else if (x < 4.583) and (b < -8.75) then 0 else if (x < 4.583) and (b < -7.917) then 0 else if (x < 4.583) and (b < -7.083) then 0 else if (x < 4.583) and (b < -6.25) then 0 else if (x < 4.583) and (b < -5.417) then 0.001 else if (x < 4.583) and (b < -4.583) then 0.001 else if (x < 4.583) and (b < -3.75) then 0.003 else if (x < 4.583) and (b < -2.917) then 0.005 else if (x < 4.583) and (b < -2.083) then 0.009 else if (x < 4.583) and (b < -1.25) then 0.017 else if (x < 4.583) and (b < -0.417) then 0.029 else if (x < 4.583) and (b < 0.417) then 0.043 else if (x < 4.583) and (b < 1.25) then 0.06 else if (x < 4.583) and (b < 2.083) then 0.077 else if (x < 4.583) and (b < 2.917) then 0.094 else if (x < 4.583) and (b < 3.75) then 0.108 else if (x < 4.583) and (b < 4.583) then 0.114 else if (x < 4.583) and (b < 5.417) then 0.112 else if (x < 4.583) and (b < 6.25) then 0.103 else if (x < 4.583) and (b < 7.083) then 0.09 else if (x < 4.583) and (b < 7.917) then 0.075 else if (x < 4.583) and (b < 8.75) then 0.061 else if (x < 4.583) and (b < 9.583) then 0.046 else if (x < 4.583) and (b < 10) then 0.034 else if (x < 5.417) and (b < -9.583) then 0 else if (x < 5.417) and (b < -8.75) then 0 else if (x < 5.417) and (b < -7.917) then 0 else if (x < 5.417) and (b < -7.083) then 0 else if (x < 5.417) and (b < -6.25) then 0 else if (x < 5.417) and (b < -5.417) then 0 else if (x < 5.417) and (b < -4.583) then 0 else if (x < 5.417) and (b < -3.75) then 0.001 else if (x < 5.417) and (b < -2.917) then 0.002 else if (x < 5.417) and (b < -2.083) then 0.005 else if (x < 5.417) and (b < -1.25) then 0.01 else if (x < 5.417) and (b < -0.417) then 0.017 else if (x < 5.417) and (b < 0.417) then 0.027 else if (x < 5.417) and (b < 1.25) then 0.043 else if (x < 5.417) and (b < 2.083) then 0.058 else if (x < 5.417) and (b < 2.917) then 0.079 else if (x < 5.417) and (b < 3.75) then 0.098 else if (x < 5.417) and (b < 4.583) then 0.108 else if (x < 5.417) and (b < 5.417) then 0.115 else if (x < 5.417) and (b < 6.25) then 0.115 else if (x < 5.417) and (b < 7.083) then 0.11 else if (x < 5.417) and (b < 7.917) then 0.098 else if (x < 5.417) and (b < 8.75) then 0.084 else if (x < 5.417) and (b < 9.583) then 0.071 else if (x < 5.417) and (b < 10) then 0.056 else if (x < 6.25) and (b < -9.583) then 0 else if (x < 6.25) and (b < -8.75) then 0 else if (x < 6.25) and (b < -7.917) then 0 else if (x < 6.25) and (b < -7.083) then 0 else if (x < 6.25) and (b < -6.25) then 0 else if (x < 6.25) and (b < -5.417) then 0 else if (x < 6.25) and (b < -4.583) then 0 else if (x < 6.25) and (b < -3.75) then 0 else if (x < 6.25) and (b < -2.917) then 0.001 else if (x < 6.25) and (b < -2.083) then 0.002 else if (x < 6.25) and (b < -1.25) then 0.005 else if (x < 6.25) and (b < -0.417) then 0.01 else if (x < 6.25) and (b < 0.417) then 0.016 else if (x < 6.25) and (b < 1.25) then 0.027 else if (x < 6.25) and (b < 2.083) then 0.042 else if (x < 6.25) and (b < 2.917) then 0.06 else if (x < 6.25) and (b < 3.75) then 0.079 else if (x < 6.25) and (b < 4.583) then 0.097 else if (x < 6.25) and (b < 5.417) then 0.112 else if (x < 6.25) and (b < 6.25) then 0.122 else if (x < 6.25) and (b < 7.083) then 0.121 else if (x < 6.25) and (b < 7.917) then 0.118 else if (x < 6.25) and (b < 8.75) then 0.11 else if (x < 6.25) and (b < 9.583) then 0.097 else if (x < 6.25) and (b < 10) then 0.085 else if (x < 7.083) and (b < -9.583) then 0 else if (x < 7.083) and (b < -8.75) then 0 else if (x < 7.083) and (b < -7.917) then 0 else if (x < 7.083) and (b < -7.083) then 0 else if (x < 7.083) and (b < -6.25) then 0 else if (x < 7.083) and (b < -5.417) then 0 else if (x < 7.083) and (b < -4.583) then 0 else if (x < 7.083) and (b < -3.75) then 0 else if (x < 7.083) and (b < -2.917) then 0.001 else if (x < 7.083) and (b < -2.083) then 0.001 else if (x < 7.083) and (b < -1.25) then 0.002 else if (x < 7.083) and (b < -0.417) then 0.005 else if (x < 7.083) and (b < 0.417) then 0.009 else if (x < 7.083) and (b < 1.25) then 0.017 else if (x < 7.083) and (b < 2.083) then 0.028 else if (x < 7.083) and (b < 2.917) then 0.042 else if (x < 7.083) and (b < 3.75) then 0.06 else if (x < 7.083) and (b < 4.583) then 0.081 else if (x < 7.083) and (b < 5.417) then 0.1 else if (x < 7.083) and (b < 6.25) then 0.115 else if (x < 7.083) and (b < 7.083) then 0.128 else if (x < 7.083) and (b < 7.917) then 0.133 else if (x < 7.083) and (b < 8.75) then 0.133 else if (x < 7.083) and (b < 9.583) then 0.126 else if (x < 7.083) and (b < 10) then 0.121 else if (x < 7.917) and (b < -9.583) then 0 else if (x < 7.917) and (b < -8.75) then 0 else if (x < 7.917) and (b < -7.917) then 0 else if (x < 7.917) and (b < -7.083) then 0 else if (x < 7.917) and (b < -6.25) then 0 else if (x < 7.917) and (b < -5.417) then 0 else if (x < 7.917) and (b < -4.583) then 0 else if (x < 7.917) and (b < -3.75) then 0 else if (x < 7.917) and (b < -2.917) then 0 else if (x < 7.917) and (b < -2.083) then 0 else if (x < 7.917) and (b < -1.25) then 0.001 else if (x < 7.917) and (b < -0.417) then 0.002 else if (x < 7.917) and (b < 0.417) then 0.005 else if (x < 7.917) and (b < 1.25) then 0.009 else if (x < 7.917) and (b < 2.083) then 0.017 else if (x < 7.917) and (b < 2.917) then 0.029 else if (x < 7.917) and (b < 3.75) then 0.044 else if (x < 7.917) and (b < 4.583) then 0.062 else if (x < 7.917) and (b < 5.417) then 0.082 else if (x < 7.917) and (b < 6.25) then 0.103 else if (x < 7.917) and (b < 7.083) then 0.121 else if (x < 7.917) and (b < 7.917) then 0.137 else if (x < 7.917) and (b < 8.75) then 0.15 else if (x < 7.917) and (b < 9.583) then 0.157 else if (x < 7.917) and (b < 10) then 0.157 else if (x < 8.75) and (b < -9.583) then 0 else if (x < 8.75) and (b < -8.75) then 0 else if (x < 8.75) and (b < -7.917) then 0 else if (x < 8.75) and (b < -7.083) then 0 else if (x < 8.75) and (b < -6.25) then 0 else if (x < 8.75) and (b < -5.417) then 0 else if (x < 8.75) and (b < -4.583) then 0 else if (x < 8.75) and (b < -3.75) then 0 else if (x < 8.75) and (b < -2.917) then 0 else if (x < 8.75) and (b < -2.083) then 0 else if (x < 8.75) and (b < -1.25) then 0 else if (x < 8.75) and (b < -0.417) then 0.001 else if (x < 8.75) and (b < 0.417) then 0.003 else if (x < 8.75) and (b < 1.25) then 0.005 else if (x < 8.75) and (b < 2.083) then 0.009 else if (x < 8.75) and (b < 2.917) then 0.017 else if (x < 8.75) and (b < 3.75) then 0.028 else if (x < 8.75) and (b < 4.583) then 0.043 else if (x < 8.75) and (b < 5.417) then 0.063 else if (x < 8.75) and (b < 6.25) then 0.086 else if (x < 8.75) and (b < 7.083) then 0.111 else if (x < 8.75) and (b < 7.917) then 0.133 else if (x < 8.75) and (b < 8.75) then 0.154 else if (x < 8.75) and (b < 9.583) then 0.176 else if (x < 8.75) and (b < 10) then 0.188 else if (x < 9.583) and (b < -9.583) then 0 else if (x < 9.583) and (b < -8.75) then 0 else if (x < 9.583) and (b < -7.917) then 0 else if (x < 9.583) and (b < -7.083) then 0 else if (x < 9.583) and (b < -6.25) then 0 else if (x < 9.583) and (b < -5.417) then 0 else if (x < 9.583) and (b < -4.583) then 0 else if (x < 9.583) and (b < -3.75) then 0 else if (x < 9.583) and (b < -2.917) then 0 else if (x < 9.583) and (b < -2.083) then 0 else if (x < 9.583) and (b < -1.25) then 0 else if (x < 9.583) and (b < -0.417) then 0 else if (x < 9.583) and (b < 0.417) then 0.001 else if (x < 9.583) and (b < 1.25) then 0.002 else if (x < 9.583) and (b < 2.083) then 0.005 else if (x < 9.583) and (b < 2.917) then 0.01 else if (x < 9.583) and (b < 3.75) then 0.017 else if (x < 9.583) and (b < 4.583) then 0.03 else if (x < 9.583) and (b < 5.417) then 0.045 else if (x < 9.583) and (b < 6.25) then 0.065 else if (x < 9.583) and (b < 7.083) then 0.09 else if (x < 9.583) and (b < 7.917) then 0.119 else if (x < 9.583) and (b < 8.75) then 0.149 else if (x < 9.583) and (b < 9.583) then 0.182 else if (x < 9.583) and (b < 10) then 0.212 else if (x < 10) and (b < -9.583) then 0 else if (x < 10) and (b < -8.75) then 0 else if (x < 10) and (b < -7.917) then 0 else if (x < 10) and (b < -7.083) then 0 else if (x < 10) and (b < -6.25) then 0 else if (x < 10) and (b < -5.417) then 0 else if (x < 10) and (b < -4.583) then 0 else if (x < 10) and (b < -3.75) then 0 else if (x < 10) and (b < -2.917) then 0 else if (x < 10) and (b < -2.083) then 0 else if (x < 10) and (b < -1.25) then 0 else if (x < 10) and (b < -0.417) then 0 else if (x < 10) and (b < 0.417) then 0 else if (x < 10) and (b < 1.25) then 0.001 else if (x < 10) and (b < 2.083) then 0.002 else if (x < 10) and (b < 2.917) then 0.003 else if (x < 10) and (b < 3.75) then 0.005 else if (x < 10) and (b < 4.583) then 0.01 else if (x < 10) and (b < 5.417) then 0.016 else if (x < 10) and (b < 6.25) then 0.025 else if (x < 10) and (b < 7.083) then 0.038 else if (x < 10) and (b < 7.917) then 0.052 else if (x < 10) and (b < 8.75) then 0.069 else if (x < 10) and (b < 9.583) then 0.088 else 0.111");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, quantitativeTests);
	}

	@Test 
	public void queryWithTwoVariablesAndSumSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"constant b : [-10;10];" +
				"x = Normal(b + 2, 3.0);" +
				"";

		String query = "x";
		boolean quantitativeTests = false; // too large to parse due to a bug (not to generate)
		Expression expected = null;
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, quantitativeTests);
	}

	@Test  
	public void queryWithThreeVariablesSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"constant b : [-10;10];" +
				"constant c : [-10;10];" +
				"x = Normal(b + c, 3.0);" +
				"";

		String query = "x";
		boolean quantitativeTests = false; // too large to parse due to a bug (not to generate)
		Expression expected = null;
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, quantitativeTests);
	}

	@Test
	public void normalAndEqualitySamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"y = Normal(0.0, 3);" +
				"x = y;"
				;

		String query = "x";
		Expression expected = parse("if x < -9.583 then 0 else if x < -8.75 then 0.001 else if x < -7.917 then 0.002 else if x < -7.083 then 0.005 else if x < -6.25 then 0.01 else if x < -5.417 then 0.017 else if x < -4.583 then 0.028 else if x < -3.75 then 0.042 else if x < -2.917 then 0.06 else if x < -2.083 then 0.078 else if x < -1.25 then 0.094 else if x < -0.417 then 0.106 else if x < 0.417 then 0.111 else if x < 1.25 then 0.106 else if x < 2.083 then 0.095 else if x < 2.917 then 0.078 else if x < 3.75 then 0.06 else if x < 4.583 then 0.042 else if x < 5.417 then 0.028 else if x < 6.25 then 0.017 else if x < 7.083 then 0.009 else if x < 7.917 then 0.005 else if x < 8.75 then 0.002 else if x < 9.583 then 0.001 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void normalWithVariableMeanSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"y = Normal(0.0, 1);" +
				"x = Normal(y, 3);"
				;

		String query = "x";
		Expression expected = parse("if x < -9.583 then 0 else if x < -8.75 then 0.002 else if x < -7.917 then 0.003 else if x < -7.083 then 0.006 else if x < -6.25 then 0.012 else if x < -5.417 then 0.019 else if x < -4.583 then 0.03 else if x < -3.75 then 0.044 else if x < -2.917 then 0.06 else if x < -2.083 then 0.077 else if x < -1.25 then 0.091 else if x < -0.417 then 0.101 else if x < 0.417 then 0.105 else if x < 1.25 then 0.101 else if x < 2.083 then 0.092 else if x < 2.917 then 0.077 else if x < 3.75 then 0.06 else if x < 4.583 then 0.044 else if x < 5.417 then 0.03 else if x < 6.25 then 0.019 else if x < 7.083 then 0.012 else if x < 7.917 then 0.006 else if x < 8.75 then 0.003 else if x < 9.583 then 0.002 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	// @Test
	public void normalWithVariableMeanAndConditioningSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"y = Normal(0.0, 1);" +
				"x = Normal(y, 3);"
				;

		String query = "x";
		Sample conditioningSample = DefaultSample.makeFreshSample();
		conditioningSample.getAssignment().set(DefaultExpressionVariable.expressionVariable(parse("y")), 4.0);
		Expression expected = parse("if x < -9.583 then 0 else if x < -8.75 then 0 else if x < -7.917 then 0 else if x < -7.083 then 0 else if x < -6.25 then 0 else if x < -5.417 then 0 else if x < -4.583 then 0.001 else if x < -3.75 then 0.003 else if x < -2.917 then 0.006 else if x < -2.083 then 0.011 else if x < -1.25 then 0.019 else if x < -0.417 then 0.031 else if x < 0.417 then 0.046 else if x < 1.25 then 0.066 else if x < 2.083 then 0.083 else if x < 2.917 then 0.099 else if x < 3.75 then 0.112 else if x < 4.583 then 0.114 else if x < 5.417 then 0.106 else if x < 6.25 then 0.093 else if x < 7.083 then 0.077 else if x < 7.917 then 0.057 else if x < 8.75 then 0.04 else if x < 9.583 then 0.027 else 0.009");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, conditioningSample);
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
		Expression expected = parse("if x < -9.583 then 0.147 else if x < -8.75 then 0.378 else if x < -7.917 then 0.312 else if x < -7.083 then 0.13 else if x < -6.25 then 0.029 else if x < -5.417 then 0.004 else if x < -4.583 then 0 else if x < -3.75 then 0 else if x < -2.917 then 0 else if x < -2.083 then 0 else if x < -1.25 then 0 else if x < -0.417 then 0 else if x < 0.417 then 0 else if x < 1.25 then 0 else if x < 2.083 then 0 else if x < 2.917 then 0 else if x < 3.75 then 0 else if x < 4.583 then 0 else if x < 5.417 then 0 else if x < 6.25 then 0 else if x < 7.083 then 0 else if x < 7.917 then 0 else if x < 8.75 then 0 else if x < 9.583 then 0 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void equalitySamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"x = y;" +
				"y = 1.0;" +
				"";

		String query = "x";
		Expression expected = parse("if x < -9.5 then 0 else if x < -8.5 then 0 else if x < -7.5 then 0 else if x < -6.5 then 0 else if x < -5.5 then 0 else if x < -4.5 then 0 else if x < -3.5 then 0 else if x < -2.5 then 0 else if x < -1.5 then 0 else if x < -0.5 then 0 else if x < 0.5 then 0 else if x < 1.5 then 0.991 else if x < 2.5 then 0 else if x < 3.5 then 0 else if x < 4.5 then 0 else if x < 5.5 then 0 else if x < 6.5 then 0 else if x < 7.5 then 0 else if x < 8.5 then 0 else if x < 9.5 then 0 else 0");
//		Expression expected = parse("if x = 1.0 then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void equalityFunctionSamplingTest() {

		String model = "" +
				"random equal : Boolean;" +
				"random x : 0..5;" +
				"random y : 0..5;" +
				"equal = (x = y);" +
				"x = 1;" +
				"y = 1;" +
				"";

		String query = "equal";
		Expression expected = parse("if equal then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void equalityFunctionApplicableInverseSamplingTest() {

		String model = "" +
				"random equal : Boolean;" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"equal = (x = y);" +
				"equal;" +
				"x = 1.0;" +
				"y = Normal(2.0, 2.0);" +
				"";

		String query = "y";
		Expression expected = parse("if y < -9.5 then 0 else if y < -8.5 then 0 else if y < -7.5 then 0 else if y < -6.5 then 0 else if y < -5.5 then 0 else if y < -4.5 then 0 else if y < -3.5 then 0 else if y < -2.5 then 0 else if y < -1.5 then 0 else if y < -0.5 then 0 else if y < 0.5 then 0 else if y < 1.5 then 1 else if y < 2.5 then 0 else if y < 3.5 then 0 else if y < 4.5 then 0 else if y < 5.5 then 0 else if y < 6.5 then 0 else if y < 7.5 then 0 else if y < 8.5 then 0 else if y < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void equalityFunctionNotApplicableInverseSamplingTest() {

		String model = "" +
				"random equal : Boolean;" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"equal = (x = y);" +
				"not equal;" +
				"x = 1.0;" +
				"y = Normal(2.0, 2.0);" +
				"";

		String query = "y";
		Expression expected = parse("if y < -9.5 then 0 else if y < -8.5 then 0 else if y < -7.5 then 0 else if y < -6.5 then 0 else if y < -5.5 then 0 else if y < -4.5 then 0 else if y < -3.5 then 0.002 else if y < -2.5 then 0.009 else if y < -1.5 then 0.028 else if y < -0.5 then 0.067 else if y < 0.5 then 0.121 else if y < 1.5 then 0.174 else if y < 2.5 then 0.198 else if y < 3.5 then 0.174 else if y < 4.5 then 0.121 else if y < 5.5 then 0.065 else if y < 6.5 then 0.028 else if y < 7.5 then 0.009 else if y < 8.5 then 0.003 else if y < 9.5 then 0.001 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void lessThanSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"x = Normal(2.0, 2.0);" +
				"x < 0.0;" +
				"";

		String query = "x";
		Expression expected = parse("if x < -9.583 then 0 else if x < -8.75 then 0 else if x < -7.917 then 0 else if x < -7.083 then 0 else if x < -6.25 then 0 else if x < -5.417 then 0.001 else if x < -4.583 then 0.002 else if x < -3.75 then 0.009 else if x < -2.917 then 0.032 else if x < -2.083 then 0.087 else if x < -1.25 then 0.199 else if x < -0.417 then 0.386 else if x < 0.417 then 0.284 else if x < 1.25 then 0 else if x < 2.083 then 0 else if x < 2.917 then 0 else if x < 3.75 then 0 else if x < 4.583 then 0 else if x < 5.417 then 0 else if x < 6.25 then 0 else if x < 7.083 then 0 else if x < 7.917 then 0 else if x < 8.75 then 0 else if x < 9.583 then 0 else 0");
		int initialNumberOfSamples = 100000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void greaterThanSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"x = Normal(1.0, 2.0);" +
				"x > 0.0;" +
				"";

		String query = "x";
		Expression expected = parse("if x < -9.583 then 0 else if x < -8.75 then 0 else if x < -7.917 then 0 else if x < -7.083 then 0 else if x < -6.25 then 0 else if x < -5.417 then 0 else if x < -4.583 then 0 else if x < -3.75 then 0 else if x < -2.917 then 0 else if x < -2.083 then 0 else if x < -1.25 then 0 else if x < -0.417 then 0 else if x < 0.417 then 0.111 else if x < 1.25 then 0.236 else if x < 2.083 then 0.228 else if x < 2.917 then 0.179 else if x < 3.75 then 0.121 else if x < 4.583 then 0.071 else if x < 5.417 then 0.034 else if x < 6.25 then 0.014 else if x < 7.083 then 0.004 else if x < 7.917 then 0.002 else if x < 8.75 then 0 else if x < 9.583 then 0 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void lessThanOrEqualToSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"x = Normal(2.0, 2.0);" +
				"x <= 0.0;" +
				"";

		String query = "x";
		Expression expected = parse("if x < -9.583 then 0 else if x < -8.75 then 0 else if x < -7.917 then 0 else if x < -7.083 then 0 else if x < -6.25 then 0 else if x < -5.417 then 0.001 else if x < -4.583 then 0.002 else if x < -3.75 then 0.009 else if x < -2.917 then 0.032 else if x < -2.083 then 0.087 else if x < -1.25 then 0.199 else if x < -0.417 then 0.386 else if x < 0.417 then 0.284 else if x < 1.25 then 0 else if x < 2.083 then 0 else if x < 2.917 then 0 else if x < 3.75 then 0 else if x < 4.583 then 0 else if x < 5.417 then 0 else if x < 6.25 then 0 else if x < 7.083 then 0 else if x < 7.917 then 0 else if x < 8.75 then 0 else if x < 9.583 then 0 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void greaterThanOrEqualToSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"x = Normal(1.0, 2.0);" +
				"x >= 0.0;" +
				"";

		String query = "x";
		Expression expected = parse("if x < -9.583 then 0 else if x < -8.75 then 0 else if x < -7.917 then 0 else if x < -7.083 then 0 else if x < -6.25 then 0 else if x < -5.417 then 0 else if x < -4.583 then 0 else if x < -3.75 then 0 else if x < -2.917 then 0 else if x < -2.083 then 0 else if x < -1.25 then 0 else if x < -0.417 then 0 else if x < 0.417 then 0.111 else if x < 1.25 then 0.236 else if x < 2.083 then 0.228 else if x < 2.917 then 0.179 else if x < 3.75 then 0.121 else if x < 4.583 then 0.071 else if x < 5.417 then 0.034 else if x < 6.25 then 0.014 else if x < 7.083 then 0.004 else if x < 7.917 then 0.002 else if x < 8.75 then 0 else if x < 9.583 then 0 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void orFactorSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"x = Normal(2.0, 4.0);" +
				"x < 1.0 or x > 3.0;" + // same as notFactorSamplingTest
				"";

		String query = "x";
		Expression expected = parse("if x < -9.796 then 0 else if x < -9.388 then 0.001 else if x < -8.98 then 0.001 else if x < -8.571 then 0.002 else if x < -8.163 then 0.002 else if x < -7.755 then 0.002 else if x < -7.347 then 0.004 else if x < -6.939 then 0.004 else if x < -6.531 then 0.005 else if x < -6.122 then 0.006 else if x < -5.714 then 0.007 else if x < -5.306 then 0.009 else if x < -4.898 then 0.011 else if x < -4.49 then 0.012 else if x < -4.082 then 0.015 else if x < -3.673 then 0.018 else if x < -3.265 then 0.021 else if x < -2.857 then 0.024 else if x < -2.449 then 0.027 else if x < -2.041 then 0.03 else if x < -1.633 then 0.032 else if x < -1.224 then 0.035 else if x < -0.816 then 0.04 else if x < -0.408 then 0.043 else if x < -1.2E-15 then 0.046 else if x < 0.408 then 0.046 else if x < 0.816 then 0.05 else if x < 1.224 then 0.023 else if x < 1.633 then 0 else if x < 2.041 then 0 else if x < 2.449 then 0 else if x < 2.857 then 0 else if x < 3.265 then 0.032 else if x < 3.673 then 0.048 else if x < 4.082 then 0.047 else if x < 4.49 then 0.044 else if x < 4.898 then 0.043 else if x < 5.306 then 0.039 else if x < 5.714 then 0.035 else if x < 6.122 then 0.031 else if x < 6.531 then 0.029 else if x < 6.939 then 0.026 else if x < 7.347 then 0.023 else if x < 7.755 then 0.02 else if x < 8.163 then 0.019 else if x < 8.571 then 0.014 else if x < 8.98 then 0.013 else if x < 9.388 then 0.011 else if x < 9.796 then 0.009 else 0.004");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 50;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void andFactorSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"x = Normal(2.0, 4.0);" +
				"x > 0.0 and x < 4.0;" +
				"";

		String query = "x";
		Expression expected = parse("if x < -9.583 then 0 else if x < -8.75 then 0 else if x < -7.917 then 0 else if x < -7.083 then 0 else if x < -6.25 then 0 else if x < -5.417 then 0 else if x < -4.583 then 0 else if x < -3.75 then 0 else if x < -2.917 then 0 else if x < -2.083 then 0 else if x < -1.25 then 0 else if x < -0.417 then 0 else if x < 0.417 then 0.1 else if x < 1.25 then 0.204 else if x < 2.083 then 0.219 else if x < 2.917 then 0.212 else if x < 3.75 then 0.203 else if x < 4.583 then 0.06 else if x < 5.417 then 0 else if x < 6.25 then 0 else if x < 7.083 then 0 else if x < 7.917 then 0 else if x < 8.75 then 0 else if x < 9.583 then 0 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void notFactorSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"x = Normal(2.0, 4.0);" +
				"not(x > 1.0 and x < 3.0);" + // same as orFactorSamplingTest
				"";

		String query = "x";
		Expression expected = parse("if x < -9.796 then 0 else if x < -9.388 then 0.001 else if x < -8.98 then 0.001 else if x < -8.571 then 0.001 else if x < -8.163 then 0.002 else if x < -7.755 then 0.002 else if x < -7.347 then 0.003 else if x < -6.939 then 0.004 else if x < -6.531 then 0.005 else if x < -6.122 then 0.006 else if x < -5.714 then 0.007 else if x < -5.306 then 0.009 else if x < -4.898 then 0.011 else if x < -4.49 then 0.013 else if x < -4.082 then 0.015 else if x < -3.673 then 0.018 else if x < -3.265 then 0.021 else if x < -2.857 then 0.023 else if x < -2.449 then 0.027 else if x < -2.041 then 0.03 else if x < -1.633 then 0.033 else if x < -1.224 then 0.036 else if x < -0.816 then 0.039 else if x < -0.408 then 0.042 else if x < -1.2E-15 then 0.045 else if x < 0.408 then 0.047 else if x < 0.816 then 0.049 else if x < 1.224 then 0.023 else if x < 1.633 then 0 else if x < 2.041 then 0 else if x < 2.449 then 0 else if x < 2.857 then 0 else if x < 3.265 then 0.033 else if x < 3.673 then 0.049 else if x < 4.082 then 0.047 else if x < 4.49 then 0.044 else if x < 4.898 then 0.042 else if x < 5.306 then 0.038 else if x < 5.714 then 0.036 else if x < 6.122 then 0.032 else if x < 6.531 then 0.029 else if x < 6.939 then 0.026 else if x < 7.347 then 0.023 else if x < 7.755 then 0.02 else if x < 8.163 then 0.017 else if x < 8.571 then 0.015 else if x < 8.98 then 0.012 else if x < 9.388 then 0.01 else if x < 9.796 then 0.009 else 0.004");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 50;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void longEqualitySamplingTest() {

		String model = "" +
				"random a : [-10;10];" +
				"random b : [-10;10];" +
				"random c : [-10;10];" +
				"random d : [-10;10];" +
				"random e : [-10;10];" +
				"random f : [-10;10];" +
				"a = b;" +
				"b = c;" +
				"c = d;" +
				"d = e;" +
				"e = f;" +
				"f = 1.0;" +
				"";

		String query = "a";
		Expression expected = parse("if a < -9.5 then 0 else if a < -8.5 then 0 else if a < -7.5 then 0 else if a < -6.5 then 0 else if a < -5.5 then 0 else if a < -4.5 then 0 else if a < -3.5 then 0 else if a < -2.5 then 0 else if a < -1.5 then 0 else if a < -0.5 then 0 else if a < 0.5 then 0 else if a < 1.5 then 0.991 else if a < 2.5 then 0 else if a < 3.5 then 0 else if a < 4.5 then 0 else if a < 5.5 then 0 else if a < 6.5 then 0 else if a < 7.5 then 0 else if a < 8.5 then 0 else if a < 9.5 then 0 else 0");
//		Expression expected = parse("if a = 1.0 then 1 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void sumSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 2;" +
				"y = 3;" +
				"z = x + y;" +
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0 else if z < 5.5 then 0.991 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void sumInverseSamplingTest() {
	
		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 5;" +
				"z = 1;" +
				"z + 2.0 = x + y + 1.0;" +
				"";
	
		String query = "y";
		Expression expected = parse("if y < -9.5 then 0 else if y < -8.5 then 0 else if y < -7.5 then 0 else if y < -6.5 then 0 else if y < -5.5 then 0 else if y < -4.5 then 0 else if y < -3.5 then 0 else if y < -2.5 then 0.991 else if y < -1.5 then 0 else if y < -0.5 then 0 else if y < 0.5 then 0 else if y < 1.5 then 0 else if y < 2.5 then 0 else if y < 3.5 then 0 else if y < 4.5 then 0 else if y < 5.5 then 0 else if y < 6.5 then 0 else if y < 7.5 then 0 else if y < 8.5 then 0 else if y < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void sumOfNormalsSamplingTest() {
	
		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = Normal(5,2);" +
				"y = Normal(-4,2);" +
				"z = x + y;" +
				"";
	
		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0.001 else if z < -7.5 then 0.001 else if z < -6.5 then 0.003 else if z < -5.5 then 0.007 else if z < -4.5 then 0.015 else if z < -3.5 then 0.031 else if z < -2.5 then 0.052 else if z < -1.5 then 0.081 else if z < -0.5 then 0.108 else if z < 0.5 then 0.132 else if z < 1.5 then 0.139 else if z < 2.5 then 0.131 else if z < 3.5 then 0.108 else if z < 4.5 then 0.081 else if z < 5.5 then 0.052 else if z < 6.5 then 0.029 else if z < 7.5 then 0.016 else if z < 8.5 then 0.007 else if z < 9.5 then 0.003 else 0.001");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void subtractionSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 5;" +
				"y = 2;" +
				"z = x - y;" +
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0.991 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void subtractionInverseSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 4;" +
				"y = 2;" +
				"x = z - y;" +
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0.991 else if z < 7.5 then 0 else if z < 8.5 then 0 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void multiplicationSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 2;" +
				"y = 3;" +
				"z = x * y;" +
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0.991 else if z < 7.5 then 0 else if z < 8.5 then 0 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void multiplicationInverseSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
//				"random y : [-10;10];" +
//				"random z : [-10;10];" +
				"x = Normal(4, 2);" +
//				"y = 2;" +
//				"z = 6;" +
				"6 = x * 2;" +
				"";

		// x must be 3 with probability 1 in spite of its normal prior because x = z/y = 3/2.
		String query = "x";
		Expression expected = parse("if x < -9.5 then 0 else if x < -8.5 then 0 else if x < -7.5 then 0 else if x < -6.5 then 0 else if x < -5.5 then 0 else if x < -4.5 then 0 else if x < -3.5 then 0 else if x < -2.5 then 0 else if x < -1.5 then 0 else if x < -0.5 then 0 else if x < 0.5 then 0 else if x < 1.5 then 0 else if x < 2.5 then 0 else if x < 3.5 then 1 else if x < 4.5 then 0 else if x < 5.5 then 0 else if x < 6.5 then 0 else if x < 7.5 then 0 else if x < 8.5 then 0 else if x < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void multiplicationInverseNotKickingInWhenOtherArgumentsIsZeroSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"random w : [-10;10];" +
				"x = Normal(4, 2);" +
				"y = 0;" +
				"z = Normal(6, 2);" +
				"w = Normal(-4, 2);" + // making w negative so that y*w turns into -0.0 which is different from 0.0 but must be detected as absorbing element
				"z = x * y * w;" + // y is zero so z will also be zero and we cannot tell anything about the value of x, which will follow its prior.
				"";

		String query = "x";
		Expression expected = parse("if x < -9.5 then 0 else if x < -8.5 then 0 else if x < -7.5 then 0 else if x < -6.5 then 0 else if x < -5.5 then 0 else if x < -4.5 then 0 else if x < -3.5 then 0 else if x < -2.5 then 0 else if x < -1.5 then 0.003 else if x < -0.5 then 0.009 else if x < 0.5 then 0.028 else if x < 1.5 then 0.065 else if x < 2.5 then 0.122 else if x < 3.5 then 0.174 else if x < 4.5 then 0.196 else if x < 5.5 then 0.176 else if x < 6.5 then 0.121 else if x < 7.5 then 0.067 else if x < 8.5 then 0.028 else if x < 9.5 then 0.01 else 0.002");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 21;
		
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void multiplicationResultIsZeroIfJustOneArgumentIs0Test() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"random w : [-10;10];" +
				"x = Normal(4, 2);" +
				"y = 0;" +
				"z = Normal(6, 2);" +
				"w = Normal(6, 2);" +
				"z = x * y * w;" + // y is zero so z will also be zero
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 1 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
		
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void divisionSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 8;" +
				"y = 2;" +
				"z = x / y;" +
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0.991 else if z < 5.5 then 0 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void divisionInverseSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 4;" +
				"y = 2;" +
				"x = z / y;" +
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0.991 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void divisionByZeroSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 4;" +
				"y = -4;" +
				"z = x / (x + y);" +
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0.991 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;

		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
		}
		catch (Error e) {
			println(e.getMessage());
			if (!Util.containsAllCaseInsensitive(e.getMessage(), "illegal", "arguments")) {
				throw new AssertionError("Should have thrown 'illegal arguments' error, but threw " + e.getMessage(), e);
			}
		}
	}
	
	
	@Test
	public void exponentiationSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 2;" +
				"y = 3;" +
				"z = x ^ y;" +
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0.991 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void exponentiationInverseSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 9;" +
				"y = 2;" +
				"x = z ^ y;" +
				"";

		String query = "z";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0.991 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void exponentiationIllegalSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 0.5;" +
				"y = (-4)^x;" + // illegal square root
				"";

		String query = "y";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0.991 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;

		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
		}
		catch (Error e) {
			println(e.getMessage());
			if (!Util.containsAllCaseInsensitive(e.getMessage(), "error", "solving")) {
				throw new AssertionError("Should have thrown 'Error solving...' error, but threw " + e.getMessage(), e);
			}
		}
	}

	@Test
	public void exponentiationIllegalInverseSamplingTest() {

		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"-4 = x^2;" + // inverse sampling tries (-4)^0.5 = x, which is an illegal square root.
				"";

		String query = "x";
		Expression expected = parse("if z < -9.5 then 0 else if z < -8.5 then 0 else if z < -7.5 then 0 else if z < -6.5 then 0 else if z < -5.5 then 0 else if z < -4.5 then 0 else if z < -3.5 then 0 else if z < -2.5 then 0 else if z < -1.5 then 0 else if z < -0.5 then 0 else if z < 0.5 then 0 else if z < 1.5 then 0 else if z < 2.5 then 0 else if z < 3.5 then 0 else if z < 4.5 then 0 else if z < 5.5 then 0 else if z < 6.5 then 0 else if z < 7.5 then 0 else if z < 8.5 then 0.991 else if z < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;

		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
		}
		catch (Error e) {
			println(e.getMessage());
			if (!Util.containsAllCaseInsensitive(e.getMessage(), "error", "solving")) {
				throw new AssertionError("Should have thrown 'Error solving...' error, but threw " + e.getMessage(), e);
			}
		}
	}
	
	@Test
	public void unaryMinusSamplingTest() {
	
		String model = "" +
				"random x : [-10;10];" +
				"random y : [-10;10];" +
				"random z : [-10;10];" +
				"x = 2;" +
				"y = -x;" +
				"";
	
		String query = "y";
		Expression expected = parse("if y < -9.5 then 0 else if y < -8.5 then 0 else if y < -7.5 then 0 else if y < -6.5 then 0 else if y < -5.5 then 0 else if y < -4.5 then 0 else if y < -3.5 then 0 else if y < -2.5 then 0 else if y < -1.5 then 0.991 else if y < -0.5 then 0 else if y < 0.5 then 0 else if y < 1.5 then 0 else if y < 2.5 then 0 else if y < 3.5 then 0 else if y < 4.5 then 0 else if y < 5.5 then 0 else if y < 6.5 then 0 else if y < 7.5 then 0 else if y < 8.5 then 0 else if y < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void unaryMinusInverseSamplingTest() {
	
		String model = "" +
				"random x : [-10;10];" +
				"-2 = -x;" +
				"";
	
		String query = "x";
		Expression expected = parse("if x < -9.5 then 0 else if x < -8.5 then 0 else if x < -7.5 then 0 else if x < -6.5 then 0 else if x < -5.5 then 0 else if x < -4.5 then 0 else if x < -3.5 then 0 else if x < -2.5 then 0 else if x < -1.5 then 0 else if x < -0.5 then 0 else if x < 0.5 then 0 else if x < 1.5 then 0 else if x < 2.5 then 0.991 else if x < 3.5 then 0 else if x < 4.5 then 0 else if x < 5.5 then 0 else if x < 6.5 then 0 else if x < 7.5 then 0 else if x < 8.5 then 0 else if x < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void booleanSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"p;" +
				"";
	
		String query = "p";
		Expression expected = parse("if p then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
	}
	
	@Test
	public void booleanFalseSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"not p;" +
				"";
	
		String query = "p";
		Expression expected = parse("if p then 0 else 1");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
	}
	
	@Test
	public void conjunctionSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"random q : Boolean;" +
				"random r : Boolean;" +
				"r = (p and q);" + // TODO: replace by equivalence, as = has a parsing issue that = has greater precedence than and
				"p;" +
				"not q;" +
				"";
		
		String query = "r";
		Expression expected = parse("if r then 0 else 1");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "p";
		expected = parse("if p then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "q";
		expected = parse("if q then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
	}
	
	@Test
	public void conjunctionInverseSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"random q : Boolean;" +
				"random r : Boolean;" +
				"r = (p and q);" + // TODO: replace by equivalence, as = has a parsing issue that = has greater precedence than and
				"r;" +
				"q;" +
				"";
		// p should be true by inversion in r = (p and q) when r and q are true
		
		String query = "r";
		Expression expected = parse("if r then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "p";
		expected = parse("if p then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "q";
		expected = parse("if q then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
	}

	@Test
	public void conjunctionResultIsFalseIfOnlyOneArgumentIsFalseSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"random q : Boolean;" +
				"random r : Boolean;" +
				"r = (p and q);" + // TODO: replace by equivalence, as = has a parsing issue that = has greater precedence than and
				"not q;" +
				"";
		// note that p has no specified distribution
		// this is on purpose so that it does not get instantiated and allows r to be determined in absence of its information
		// However, this model will raise an error if p is to be sampled, as tested below.
		
		String query = "r";
		Expression expected = parse("if r then 0 else 1");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "q";
		expected = parse("if q then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);

		query = "p";
		expected = null; // no expected expression, because sampling should cause error before that's needed
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		}
		catch (Throwable e) {
			println(e.getMessage());
			if (!Util.containsAllCaseInsensitive(e.getMessage(), "could not sample p")) {
				throw new AssertionError("Should have thrown \"Could not sample p\" error, but threw " + e.getMessage(), e);
			}
		}
	}

	@Test
	public void disjunctionSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"random q : Boolean;" +
				"random r : Boolean;" +
				"r = (p or q);" + // TODO: replace by equivalence, as = has a parsing issue that = has greater precedence than and
				"p and not q;" +
				"";
		
		String query = "r";
		Expression expected = parse("if r then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "p";
		expected = parse("if p then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "q";
		expected = parse("if q then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
	}

	@Test
	public void disjunctionInverseSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"random q : Boolean;" +
				"random r : Boolean;" +
				"r = (p or q);" + // TODO: replace by equivalence, as = has a parsing issue that = has greater precedence than and
				"r and not q;" +
				"";
		// p should be true by inversion in r = (p or q) when r is true and q is false
		
		String query = "r";
		Expression expected = parse("if r then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "p";
		expected = parse("if p then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "q";
		expected = parse("if q then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
	}

	@Test
	public void disjunctionResultIsFalseIfOnlyOneArgumentIsFalseSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"random q : Boolean;" +
				"random r : Boolean;" +
				"r = (p or q);" + // TODO: replace by equivalence, as = has a parsing issue that = has greater precedence than and
				"q;" +
				"";
		// note that p has no specified distribution
		// this is on purpose so that it does not get instantiated and allows r to be determined in absence of its information
		// However, this model will raise an error if p is to be sampled, as tested below.
		
		String query = "r";
		Expression expected = parse("if r then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		
		query = "q";
		expected = parse("if q then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
	
		query = "p";
		expected = null; // should cause error
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, true);
		}
		catch (Throwable e) {
			println(e.getMessage());
			if (!Util.containsAllCaseInsensitive(e.getMessage(), "could not sample p")) {
				throw new AssertionError("Should have thrown \"Could not sample p\" error, but threw " + e.getMessage(), e);
			}
		}
	}

	@Test
	public void orAndCompoundInverseSamplingTest() {

		String model = "" +
				"random p : Boolean;" +
				"random q : Boolean;" +
				"random r : Boolean;" +
				"(p and q) or (not p and r);" +
				"p;" +
				"";

		String query = "q";
		Expression expected = parse("if q then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 50;

		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void negationSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"random q : Boolean;" +
				"p = (not true);" +
				"q = (not not p);" +
				"";
	
		String query = "q";
		Expression expected = parse("if q then 0 else 1");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);

		query = "p";
		expected = parse("if p then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void negationInverseSamplingTest() {
	
		String model = "" +
				"random p : Boolean;" +
				"random q : Boolean;" +
				"q = (not not not p);" +
				"not p;" +
				"";
		
		String query = "q";
		Expression expected = parse("if q then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
		
		query = "p";
		expected = parse("if p then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void formulaPropagationSamplingTest() {
	
		String model = "" +
				"random playTennis : Boolean;" +
				"random raining : Boolean;" +
				"random partnerIsAvailable : Boolean;" +
				"random playByMyself : Boolean;" +
				"playTennis = (((partnerIsAvailable and not raining)) or playByMyself);" +
				"raining and not playByMyself;" +
				"";
		
		String query = "playTennis";
		Expression expected = parse("if playTennis then 0 else 1");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void countiesSamplingTest() {
	
		String model = "" +
				"sort Counties: 4, Abiemnhom, Abyei, Akobo, AweilCentre;" + 
				"random capital: Counties;" + 
				"capital = Abiemnhom;" + 
				"";
		
		String query = "capital";
		Expression expected = parse("if capital = Abiemnhom then 1 else if capital = Abyei then 0 else if capital = Akobo then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void countiesSamplingTest2() {
	
		String model = "" +
				"sort Counties: 3, Abiemnhom, Abyei, Akobo;" + 
				"random capital: Counties;" + 
				"random x : [-10;10];" + 
				"x = Normal(0,1);" + 
				"capital = if x > -1 then (if x > 0 then Abyei else Abiemnhom) else Akobo;" +
				"";
		
		String query = "capital";
		Expression expected = parse("if capital = Abiemnhom then 0.35 else if capital = Abyei then 0.488 else 0.162");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void countiesSamplingTest3() {
	
		String model = "" +
				"sort Counties: 3, Abiemnhom', Abyei', Akobo';" + // the quotes makes them not be region names and makes plots be recorded, not maps. 
				"random capital: Counties;" + 
				"constant x : [-10;10];" + 
				"capital = if x > 0 then Abyei' else Akobo';";
		
		String query = "capital";
		Expression expected = parse("if (x < -9.5) and (capital = Abiemnhom') then 0 else if (x < -9.5) and (capital = Abyei') then 0 else if (x < -9.5) and (capital = Akobo') then 1 else if (x < -8.5) and (capital = Abiemnhom') then 0 else if (x < -8.5) and (capital = Abyei') then 0 else if (x < -8.5) and (capital = Akobo') then 1 else if (x < -7.5) and (capital = Abiemnhom') then 0 else if (x < -7.5) and (capital = Abyei') then 0 else if (x < -7.5) and (capital = Akobo') then 1 else if (x < -6.5) and (capital = Abiemnhom') then 0 else if (x < -6.5) and (capital = Abyei') then 0 else if (x < -6.5) and (capital = Akobo') then 1 else if (x < -5.5) and (capital = Abiemnhom') then 0 else if (x < -5.5) and (capital = Abyei') then 0 else if (x < -5.5) and (capital = Akobo') then 1 else if (x < -4.5) and (capital = Abiemnhom') then 0 else if (x < -4.5) and (capital = Abyei') then 0 else if (x < -4.5) and (capital = Akobo') then 1 else if (x < -3.5) and (capital = Abiemnhom') then 0 else if (x < -3.5) and (capital = Abyei') then 0 else if (x < -3.5) and (capital = Akobo') then 1 else if (x < -2.5) and (capital = Abiemnhom') then 0 else if (x < -2.5) and (capital = Abyei') then 0 else if (x < -2.5) and (capital = Akobo') then 1 else if (x < -1.5) and (capital = Abiemnhom') then 0 else if (x < -1.5) and (capital = Abyei') then 0 else if (x < -1.5) and (capital = Akobo') then 1 else if (x < -0.5) and (capital = Abiemnhom') then 0 else if (x < -0.5) and (capital = Abyei') then 0 else if (x < -0.5) and (capital = Akobo') then 1 else if (x < 0.5) and (capital = Abiemnhom') then 0 else if (x < 0.5) and (capital = Abyei') then 0 else if (x < 0.5) and (capital = Akobo') then 1 else if (x < 1.5) and (capital = Abiemnhom') then 0 else if (x < 1.5) and (capital = Abyei') then 1 else if (x < 1.5) and (capital = Akobo') then 0 else if (x < 2.5) and (capital = Abiemnhom') then 0 else if (x < 2.5) and (capital = Abyei') then 1 else if (x < 2.5) and (capital = Akobo') then 0 else if (x < 3.5) and (capital = Abiemnhom') then 0 else if (x < 3.5) and (capital = Abyei') then 1 else if (x < 3.5) and (capital = Akobo') then 0 else if (x < 4.5) and (capital = Abiemnhom') then 0 else if (x < 4.5) and (capital = Abyei') then 1 else if (x < 4.5) and (capital = Akobo') then 0 else if (x < 5.5) and (capital = Abiemnhom') then 0 else if (x < 5.5) and (capital = Abyei') then 1 else if (x < 5.5) and (capital = Akobo') then 0 else if (x < 6.5) and (capital = Abiemnhom') then 0 else if (x < 6.5) and (capital = Abyei') then 1 else if (x < 6.5) and (capital = Akobo') then 0 else if (x < 7.5) and (capital = Abiemnhom') then 0 else if (x < 7.5) and (capital = Abyei') then 1 else if (x < 7.5) and (capital = Akobo') then 0 else if (x < 8.5) and (capital = Abiemnhom') then 0 else if (x < 8.5) and (capital = Abyei') then 1 else if (x < 8.5) and (capital = Akobo') then 0 else if (x < 9.5) and (capital = Abiemnhom') then 0 else if (x < 9.5) and (capital = Abyei') then 1 else if (x < 9.5) and (capital = Akobo') then 0 else if (x < 10) and (capital = Abiemnhom') then 0 else if (x < 10) and (capital = Abyei') then 1 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void countiesWithBooleanExternalVariableSamplingTest() {
	
		String model = "" +
				"sort Counties: 2, Abyei', Akobo';" + // the quotes makes them not be region names and makes plots be recorded, not maps. 
				"random capital: Counties;" + 
				"constant year : Boolean;" +
				"capital = if year then Abyei' else Akobo';";
		
		String query = "capital";
		Expression expected = parse("if (capital = Abyei') and year then 1 else if (capital = Abyei') and not year then 0 else if (capital = Akobo') and year then 0 else 1");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void countiesWithIntegerExternalVariableSamplingTest() {
	
		String model = "" +
				"sort Counties: 3, Abiemnhom', Abyei', Akobo';" + // the quotes makes them not be region names and makes plots be recorded, not maps. 
				"random capital: Counties;" + 
				"constant month : 0..2;" +
				"constant year : 0..2;" +
				"capital = "
				+ "if year < 1 "
				+ "then Abyei' "
				+ "else if year = 1"
				+ "        then if month = 0 then Abyei' "
				+ "        else if month = 1 then Akobo' "
				+ "        else Abiemnhom'"
				+ "else Abiemnhom';";
		
		String query = "capital";
		Expression expected = parse("if (year = 0) and (capital = Abiemnhom') and (month = 0) then 0 else if (year = 0) and (capital = Abiemnhom') and (month = 1) then 0 else if (year = 0) and (capital = Abiemnhom') and (month = 2) then 0 else if (year = 0) and (capital = Abyei') and (month = 0) then 1 else if (year = 0) and (capital = Abyei') and (month = 1) then 1 else if (year = 0) and (capital = Abyei') and (month = 2) then 1 else if (year = 0) and (capital = Akobo') and (month = 0) then 0 else if (year = 0) and (capital = Akobo') and (month = 1) then 0 else if (year = 0) and (capital = Akobo') and (month = 2) then 0 else if (year = 1) and (capital = Abiemnhom') and (month = 0) then 0 else if (year = 1) and (capital = Abiemnhom') and (month = 1) then 0 else if (year = 1) and (capital = Abiemnhom') and (month = 2) then 1 else if (year = 1) and (capital = Abyei') and (month = 0) then 1 else if (year = 1) and (capital = Abyei') and (month = 1) then 0 else if (year = 1) and (capital = Abyei') and (month = 2) then 0 else if (year = 1) and (capital = Akobo') and (month = 0) then 0 else if (year = 1) and (capital = Akobo') and (month = 1) then 1 else if (year = 1) and (capital = Akobo') and (month = 2) then 0 else if (year = 2) and (capital = Abiemnhom') and (month = 0) then 1 else if (year = 2) and (capital = Abiemnhom') and (month = 1) then 1 else if (year = 2) and (capital = Abiemnhom') and (month = 2) then 1 else if (year = 2) and (capital = Abyei') and (month = 0) then 0 else if (year = 2) and (capital = Abyei') and (month = 1) then 0 else if (year = 2) and (capital = Abyei') and (month = 2) then 0 else if (year = 2) and (capital = Akobo') and (month = 0) then 0 else if (year = 2) and (capital = Akobo') and (month = 1) then 0 else 0");
		int initialNumberOfSamples = 1;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void countiesSamplingConditioningTest() {
	
		String model = "" +
				"sort Counties: 3, Abiemnhom, Abyei, Akobo;" + 
				"random capital: Counties;" + 
				"random x : [-10;10];" + 
				"x = Normal(0,1);" + 
				"capital = if x > -1 then (if x > 0 then Abyei else Abiemnhom) else Akobo;" +
				"x = 0;" +
				"";
		
		String query = "capital";
		Expression expected = parse("if capital = Abiemnhom then 1 else if capital = Abyei then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void countiesSamplingConditioningTest2() {
	
		String model = "" +
				"sort Counties: 3, Abiemnhom, Abyei, Akobo;" + 
				"random capital: Counties;" + 
				"random x : [-10;10];" + 
				"x = Normal(0,1);" + 
				"capital = if x > -1 then (if x > 0 then Abyei else Abiemnhom) else Akobo;" +
				"capital = Abyei;" +
				"";
		
		String query = "x";
		Expression expected = parse("if x < -9.5 then 0 else if x < -8.5 then 0 else if x < -7.5 then 0 else if x < -6.5 then 0 else if x < -5.5 then 0 else if x < -4.5 then 0 else if x < -3.5 then 0 else if x < -2.5 then 0 else if x < -1.5 then 0 else if x < -0.5 then 0 else if x < 0.5 then 0.381 else if x < 1.5 then 0.487 else if x < 2.5 then 0.12 else if x < 3.5 then 0.012 else if x < 4.5 then 0 else if x < 5.5 then 0 else if x < 6.5 then 0 else if x < 7.5 then 0 else if x < 8.5 then 0 else if x < 9.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void simpleRelationalSamplingTest() {
	
		String model = "" +
				"random x: 0..3 -> [-10;10];" +
				"for all I in 0..3 : x(I) = Normal(0.0, 3.0);" +
				"";
		
		String query = "x(0)";
		Expression expected = parse("if x(0) < -9.5 then 0 else if x(0) < -8.5 then 0.002 else if x(0) < -7.5 then 0.004 else if x(0) < -6.5 then 0.009 else if x(0) < -5.5 then 0.018 else if x(0) < -4.5 then 0.033 else if x(0) < -3.5 then 0.056 else if x(0) < -2.5 then 0.08 else if x(0) < -1.5 then 0.107 else if x(0) < -0.5 then 0.124 else if x(0) < 0.5 then 0.134 else if x(0) < 1.5 then 0.125 else if x(0) < 2.5 then 0.106 else if x(0) < 3.5 then 0.081 else if x(0) < 4.5 then 0.055 else if x(0) < 5.5 then 0.035 else if x(0) < 6.5 then 0.018 else if x(0) < 7.5 then 0.009 else if x(0) < 8.5 then 0.004 else if x(0) < 9.5 then 0.001 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void liftedQuerySamplingTest() {
	
		String model = "" +
				"random x: 0..3 x Boolean -> [-10;10];" +
				"for all I in 0..3 : for all P in Boolean : x(I, P) = Normal(2*I, 3.0);" +
				"";
		
		String query = "for all J in 0..3 : for all Q in Boolean : x(J, Q)";
		Expression expected = parse("if (J = 0) and (Q = true) then if x(J, Q) < -9.5 then 0 else if x(J, Q) < -8.5 then 0.001 else if x(J, Q) < -7.5 then 0.004 else if x(J, Q) < -6.5 then 0.01 else if x(J, Q) < -5.5 then 0.018 else if x(J, Q) < -4.5 then 0.032 else if x(J, Q) < -3.5 then 0.054 else if x(J, Q) < -2.5 then 0.08 else if x(J, Q) < -1.5 then 0.107 else if x(J, Q) < -0.5 then 0.125 else if x(J, Q) < 0.5 then 0.136 else if x(J, Q) < 1.5 then 0.123 else if x(J, Q) < 2.5 then 0.107 else if x(J, Q) < 3.5 then 0.083 else if x(J, Q) < 4.5 then 0.053 else if x(J, Q) < 5.5 then 0.033 else if x(J, Q) < 6.5 then 0.018 else if x(J, Q) < 7.5 then 0.01 else if x(J, Q) < 8.5 then 0.004 else if x(J, Q) < 9.5 then 0.002 else 0 else if (J = 1) and (Q = true) then if x(J, Q) < -9.5 then 0 else if x(J, Q) < -8.5 then 0 else if x(J, Q) < -7.5 then 0.001 else if x(J, Q) < -6.5 then 0.002 else if x(J, Q) < -5.5 then 0.004 else if x(J, Q) < -4.5 then 0.01 else if x(J, Q) < -3.5 then 0.019 else if x(J, Q) < -2.5 then 0.032 else if x(J, Q) < -1.5 then 0.055 else if x(J, Q) < -0.5 then 0.084 else if x(J, Q) < 0.5 then 0.104 else if x(J, Q) < 1.5 then 0.125 else if x(J, Q) < 2.5 then 0.132 else if x(J, Q) < 3.5 then 0.127 else if x(J, Q) < 4.5 then 0.108 else if x(J, Q) < 5.5 then 0.08 else if x(J, Q) < 6.5 then 0.055 else if x(J, Q) < 7.5 then 0.034 else if x(J, Q) < 8.5 then 0.017 else if x(J, Q) < 9.5 then 0.009 else 0.002 else if (J = 2) and (Q = true) then if x(J, Q) < -9.5 then 1.981E-6 else if x(J, Q) < -8.5 then 0 else if x(J, Q) < -7.5 then 0 else if x(J, Q) < -6.5 then 0 else if x(J, Q) < -5.5 then 0 else if x(J, Q) < -4.5 then 0.002 else if x(J, Q) < -3.5 then 0.004 else if x(J, Q) < -2.5 then 0.009 else if x(J, Q) < -1.5 then 0.019 else if x(J, Q) < -0.5 then 0.034 else if x(J, Q) < 0.5 then 0.056 else if x(J, Q) < 1.5 then 0.084 else if x(J, Q) < 2.5 then 0.107 else if x(J, Q) < 3.5 then 0.129 else if x(J, Q) < 4.5 then 0.136 else if x(J, Q) < 5.5 then 0.127 else if x(J, Q) < 6.5 then 0.109 else if x(J, Q) < 7.5 then 0.081 else if x(J, Q) < 8.5 then 0.056 else if x(J, Q) < 9.5 then 0.036 else 0.012 else if (J = 3) and (Q = true) then if x(J, Q) < -9.5 then 0 else if x(J, Q) < -8.5 then 0 else if x(J, Q) < -7.5 then 0 else if x(J, Q) < -6.5 then 0 else if x(J, Q) < -5.5 then 0 else if x(J, Q) < -4.5 then 0 else if x(J, Q) < -3.5 then 0.001 else if x(J, Q) < -2.5 then 0.002 else if x(J, Q) < -1.5 then 0.004 else if x(J, Q) < -0.5 then 0.01 else if x(J, Q) < 0.5 then 0.02 else if x(J, Q) < 1.5 then 0.037 else if x(J, Q) < 2.5 then 0.061 else if x(J, Q) < 3.5 then 0.086 else if x(J, Q) < 4.5 then 0.115 else if x(J, Q) < 5.5 then 0.136 else if x(J, Q) < 6.5 then 0.145 else if x(J, Q) < 7.5 then 0.14 else if x(J, Q) < 8.5 then 0.12 else if x(J, Q) < 9.5 then 0.09 else 0.033 else if (J = 0) and (Q = false) then if x(J, Q) < -9.5 then 0 else if x(J, Q) < -8.5 then 0.002 else if x(J, Q) < -7.5 then 0.004 else if x(J, Q) < -6.5 then 0.009 else if x(J, Q) < -5.5 then 0.019 else if x(J, Q) < -4.5 then 0.034 else if x(J, Q) < -3.5 then 0.056 else if x(J, Q) < -2.5 then 0.081 else if x(J, Q) < -1.5 then 0.106 else if x(J, Q) < -0.5 then 0.126 else if x(J, Q) < 0.5 then 0.131 else if x(J, Q) < 1.5 then 0.127 else if x(J, Q) < 2.5 then 0.105 else if x(J, Q) < 3.5 then 0.078 else if x(J, Q) < 4.5 then 0.054 else if x(J, Q) < 5.5 then 0.034 else if x(J, Q) < 6.5 then 0.019 else if x(J, Q) < 7.5 then 0.008 else if x(J, Q) < 8.5 then 0.004 else if x(J, Q) < 9.5 then 0.002 else 0 else if (J = 1) and (Q = false) then if x(J, Q) < -9.5 then 0 else if x(J, Q) < -8.5 then 0 else if x(J, Q) < -7.5 then 0.001 else if x(J, Q) < -6.5 then 0.001 else if x(J, Q) < -5.5 then 0.004 else if x(J, Q) < -4.5 then 0.008 else if x(J, Q) < -3.5 then 0.019 else if x(J, Q) < -2.5 then 0.036 else if x(J, Q) < -1.5 then 0.057 else if x(J, Q) < -0.5 then 0.081 else if x(J, Q) < 0.5 then 0.108 else if x(J, Q) < 1.5 then 0.125 else if x(J, Q) < 2.5 then 0.131 else if x(J, Q) < 3.5 then 0.126 else if x(J, Q) < 4.5 then 0.106 else if x(J, Q) < 5.5 then 0.079 else if x(J, Q) < 6.5 then 0.054 else if x(J, Q) < 7.5 then 0.034 else if x(J, Q) < 8.5 then 0.019 else if x(J, Q) < 9.5 then 0.008 else 0.002 else if (J = 2) and (Q = false) then if x(J, Q) < -9.5 then 5.973E-6 else if x(J, Q) < -8.5 then 0 else if x(J, Q) < -7.5 then 0 else if x(J, Q) < -6.5 then 0 else if x(J, Q) < -5.5 then 0 else if x(J, Q) < -4.5 then 0.002 else if x(J, Q) < -3.5 then 0.004 else if x(J, Q) < -2.5 then 0.009 else if x(J, Q) < -1.5 then 0.019 else if x(J, Q) < -0.5 then 0.033 else if x(J, Q) < 0.5 then 0.058 else if x(J, Q) < 1.5 then 0.083 else if x(J, Q) < 2.5 then 0.109 else if x(J, Q) < 3.5 then 0.124 else if x(J, Q) < 4.5 then 0.136 else if x(J, Q) < 5.5 then 0.129 else if x(J, Q) < 6.5 then 0.113 else if x(J, Q) < 7.5 then 0.082 else if x(J, Q) < 8.5 then 0.054 else if x(J, Q) < 9.5 then 0.035 else 0.01 else if x(J, Q) < -9.5 then 0 else if x(J, Q) < -8.5 then 0 else if x(J, Q) < -7.5 then 0 else if x(J, Q) < -6.5 then 8.144E-6 else if x(J, Q) < -5.5 then 0 else if x(J, Q) < -4.5 then 0 else if x(J, Q) < -3.5 then 0.001 else if x(J, Q) < -2.5 then 0.001 else if x(J, Q) < -1.5 then 0.004 else if x(J, Q) < -0.5 then 0.01 else if x(J, Q) < 0.5 then 0.021 else if x(J, Q) < 1.5 then 0.039 else if x(J, Q) < 2.5 then 0.058 else if x(J, Q) < 3.5 then 0.089 else if x(J, Q) < 4.5 then 0.119 else if x(J, Q) < 5.5 then 0.138 else if x(J, Q) < 6.5 then 0.143 else if x(J, Q) < 7.5 then 0.138 else if x(J, Q) < 8.5 then 0.115 else if x(J, Q) < 9.5 then 0.09 else 0.033");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void liftedSeriesSamplingTest() {
	
		String model = "" +
				"random temp: 0..5 -> [0;60];\n" + 
				"\n" + 
				"for all Month in 0..5 : if Month = 0 then temp(Month) = Normal(20,5) else temp(Month) = Normal(temp(Month - 1), 2);";
		
		String query = "temp(0)";
		Expression expected = parse("if temp(0) < 1.5 then 0 else if temp(0) < 4.5 then 0.001 else if temp(0) < 7.5 then 0.005 else if temp(0) < 10.5 then 0.022 else if temp(0) < 13.5 then 0.069 else if temp(0) < 16.5 then 0.146 else if temp(0) < 19.5 then 0.218 else if temp(0) < 22.5 then 0.231 else if temp(0) < 25.5 then 0.172 else if temp(0) < 28.5 then 0.09 else if temp(0) < 31.5 then 0.034 else if temp(0) < 34.5 then 0.009 else if temp(0) < 37.5 then 0.002 else if temp(0) < 40.5 then 0 else if temp(0) < 43.5 then 0 else if temp(0) < 46.5 then 0 else if temp(0) < 49.5 then 0 else if temp(0) < 52.5 then 0 else if temp(0) < 55.5 then 0 else if temp(0) < 58.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void liftedSeriesLiftedQuerySamplingTest() {
	
		String model = "" +
				"random temp: 0..5 -> [0;60];\n" + 
				"\n" + 
				"for all Month in 0..5 : if Month = 0 then temp(Month) = Normal(20,5) else temp(Month) = Normal(temp(Month - 1), 2);";
		
		String query = "for all M in 0..5: temp(M)";
		Expression expected = parse("if M = 0 then if temp(M) < 1.5 then 0 else if temp(M) < 4.5 then 0.001 else if temp(M) < 7.5 then 0.005 else if temp(M) < 10.5 then 0.022 else if temp(M) < 13.5 then 0.068 else if temp(M) < 16.5 then 0.146 else if temp(M) < 19.5 then 0.216 else if temp(M) < 22.5 then 0.229 else if temp(M) < 25.5 then 0.176 else if temp(M) < 28.5 then 0.093 else if temp(M) < 31.5 then 0.034 else if temp(M) < 34.5 then 0.009 else if temp(M) < 37.5 then 0.002 else if temp(M) < 40.5 then 0 else if temp(M) < 43.5 then 0 else if temp(M) < 46.5 then 0 else if temp(M) < 49.5 then 0 else if temp(M) < 52.5 then 0 else if temp(M) < 55.5 then 0 else if temp(M) < 58.5 then 0 else 0 else if M = 1 then if temp(M) < 1.5 then 0 else if temp(M) < 4.5 then 0.001 else if temp(M) < 7.5 then 0.008 else if temp(M) < 10.5 then 0.029 else if temp(M) < 13.5 then 0.078 else if temp(M) < 16.5 then 0.143 else if temp(M) < 19.5 then 0.205 else if temp(M) < 22.5 then 0.216 else if temp(M) < 25.5 then 0.167 else if temp(M) < 28.5 then 0.095 else if temp(M) < 31.5 then 0.041 else if temp(M) < 34.5 then 0.013 else if temp(M) < 37.5 then 0.003 else if temp(M) < 40.5 then 0.001 else if temp(M) < 43.5 then 0 else if temp(M) < 46.5 then 0 else if temp(M) < 49.5 then 0 else if temp(M) < 52.5 then 0 else if temp(M) < 55.5 then 0 else if temp(M) < 58.5 then 0 else 0 else if M = 2 then if temp(M) < 1.5 then 0 else if temp(M) < 4.5 then 0.003 else if temp(M) < 7.5 then 0.011 else if temp(M) < 10.5 then 0.034 else if temp(M) < 13.5 then 0.079 else if temp(M) < 16.5 then 0.143 else if temp(M) < 19.5 then 0.194 else if temp(M) < 22.5 then 0.202 else if temp(M) < 25.5 then 0.164 else if temp(M) < 28.5 then 0.1 else if temp(M) < 31.5 then 0.046 else if temp(M) < 34.5 then 0.017 else if temp(M) < 37.5 then 0.005 else if temp(M) < 40.5 then 0.001 else if temp(M) < 43.5 then 0 else if temp(M) < 46.5 then 0 else if temp(M) < 49.5 then 0 else if temp(M) < 52.5 then 0 else if temp(M) < 55.5 then 0 else if temp(M) < 58.5 then 0 else 0 else if M = 3 then if temp(M) < 1.5 then 0.001 else if temp(M) < 4.5 then 0.005 else if temp(M) < 7.5 then 0.014 else if temp(M) < 10.5 then 0.04 else if temp(M) < 13.5 then 0.084 else if temp(M) < 16.5 then 0.138 else if temp(M) < 19.5 then 0.185 else if temp(M) < 22.5 then 0.193 else if temp(M) < 25.5 then 0.157 else if temp(M) < 28.5 then 0.103 else if temp(M) < 31.5 then 0.051 else if temp(M) < 34.5 then 0.021 else if temp(M) < 37.5 then 0.007 else if temp(M) < 40.5 then 0.002 else if temp(M) < 43.5 then 0 else if temp(M) < 46.5 then 0 else if temp(M) < 49.5 then 0 else if temp(M) < 52.5 then 0 else if temp(M) < 55.5 then 0 else if temp(M) < 58.5 then 0 else 0 else if M = 4 then if temp(M) < 1.5 then 0.001 else if temp(M) < 4.5 then 0.006 else if temp(M) < 7.5 then 0.019 else if temp(M) < 10.5 then 0.043 else if temp(M) < 13.5 then 0.086 else if temp(M) < 16.5 then 0.139 else if temp(M) < 19.5 then 0.175 else if temp(M) < 22.5 then 0.184 else if temp(M) < 25.5 then 0.152 else if temp(M) < 28.5 then 0.104 else if temp(M) < 31.5 then 0.056 else if temp(M) < 34.5 then 0.024 else if temp(M) < 37.5 then 0.008 else if temp(M) < 40.5 then 0.002 else if temp(M) < 43.5 then 0.001 else if temp(M) < 46.5 then 0 else if temp(M) < 49.5 then 0 else if temp(M) < 52.5 then 0 else if temp(M) < 55.5 then 0 else if temp(M) < 58.5 then 0 else 0 else if temp(M) < 1.5 then 0.002 else if temp(M) < 4.5 then 0.007 else if temp(M) < 7.5 then 0.021 else if temp(M) < 10.5 then 0.046 else if temp(M) < 13.5 then 0.088 else if temp(M) < 16.5 then 0.137 else if temp(M) < 19.5 then 0.168 else if temp(M) < 22.5 then 0.176 else if temp(M) < 25.5 then 0.15 else if temp(M) < 28.5 then 0.104 else if temp(M) < 31.5 then 0.059 else if temp(M) < 34.5 then 0.028 else if temp(M) < 37.5 then 0.011 else if temp(M) < 40.5 then 0.003 else if temp(M) < 43.5 then 0.001 else if temp(M) < 46.5 then 0 else if temp(M) < 49.5 then 0 else if temp(M) < 52.5 then 0 else if temp(M) < 55.5 then 0 else if temp(M) < 58.5 then 0 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void liftedSeriesWithGroundStatementSamplingTest() {
	
		String model = "" +
				"random temp: 0..5 -> [0;60];\n" + 
				"\n" + 
				"for all Month in 0..5 : if Month = 0 then temp(Month) = Normal(20,5) else temp(Month) = Normal(temp(Month - 1), 2);" +
				"temp(0) = 35;" + 
				"";
		
		String query = "temp(0)";
		Expression expected = parse("if temp(0) < 1.5 then 0 else if temp(0) < 4.5 then 0 else if temp(0) < 7.5 then 0 else if temp(0) < 10.5 then 0 else if temp(0) < 13.5 then 0 else if temp(0) < 16.5 then 0 else if temp(0) < 19.5 then 0 else if temp(0) < 22.5 then 0 else if temp(0) < 25.5 then 0 else if temp(0) < 28.5 then 0 else if temp(0) < 31.5 then 0 else if temp(0) < 34.5 then 0 else if temp(0) < 37.5 then 1 else if temp(0) < 40.5 then 0 else if temp(0) < 43.5 then 0 else if temp(0) < 46.5 then 0 else if temp(0) < 49.5 then 0 else if temp(0) < 52.5 then 0 else if temp(0) < 55.5 then 0 else if temp(0) < 58.5 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void nonBooleanFactorErrorTest() {
	
		String model = "" +
				"random temp: 0..5 -> [0;60];\n" + 
				"\n" + 
				"for all Month in 0..5 : if Month = 0 then temp(Month) = Normal(20,5) else temp(Month) = Normal(temp(Month - 1), 2);" +
				"for all Month in 0..5 : temp(Month);" + // incorrect because temp(Month) is not boolean 
				"";
		
		String query = "temp(0)";
		Expression expected = null;
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		boolean error = false;
		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
		}
		catch (NonBooleanFactorError e) {
			println(e.getMessage());
			error = true;
		}
		assertTrue(error);
		
	}

	@Test
	public void nonBooleanFactorErrorTest2() {
	
		String model = "" +
				"random temp: 0..5 -> [0;60];\n" + 
				"\n" + 
				"for all Month in 0..5 : if Month = 0 then temp(Month) = Normal(20,5) else temp(Month) = Normal(temp(Month - 1), 2);" +
				"temp(0) 0.6;" + // incorrect because temp(Month) is not boolean 
				"";
		
		String query = "temp(0)";
		Expression expected = null;
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		boolean error = false;
		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
		}
		catch (AssertionError e) {
			println(e.getMessage());
			error = true;
		}
		assertTrue(error);
		
	}

	@Test
	public void nonBooleanFactorErrorTest3() {
	
		String model = "" +
				"random temp: 0..5 -> [0;60];\n" + 
				"\n" + 
				"for all Month in 0..5 : if Month = 0 then temp(Month) = Normal(20,5) else temp(Month) = Normal(temp(Month - 1), 2);" +
				"if temp(0) then 0.4 else 0.6;" + // incorrect because temp(Month) is not boolean 
				"";
		
		String query = "temp(0)";
		Expression expected = null;
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		boolean error = false;
		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
		}
		catch (AssertionError e) {
			println(e.getMessage());
			error = true;
		}
		assertTrue(error);
		
	}

	@Test
	public void histogramSamplingTest() {
	
		String model = "" +
				"random beta: [0;60];\n" + 
				"if beta < 5 then 0 else if beta < 10 then 0.2 else if beta < 15 then 0.8 else 0;" + 
				"";
		
		String query = "beta";
		Expression expected = parse("if beta < 1.5 then 0 else if beta < 4.5 then 0 else if beta < 7.5 then 0 else if beta < 10.5 then 0.194 else if beta < 13.5 then 0 else if beta < 16.5 then 0.806 else if beta < 19.5 then 0 else if beta < 22.5 then 0 else if beta < 25.5 then 0 else if beta < 28.5 then 0 else if beta < 31.5 then 0 else if beta < 34.5 then 0 else if beta < 37.5 then 0 else if beta < 40.5 then 0 else if beta < 43.5 then 0 else if beta < 46.5 then 0 else if beta < 49.5 then 0 else if beta < 52.5 then 0 else if beta < 55.5 then 0 else if beta < 58.5 then 0 else 0");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void barChartSamplingTest() {
	
		String model = "" +
				"sort Person: 4, bob, mary;" + 
				"random neighbor : 1990..1995 -> Person;" + 
				"for all Y in 1990..1995 : "
				+ "if Y > 1992 "
				+ "then neighbor(Y) = bob "
				+ "else if Normal(0,1) < 0 then neighbor(Y) = bob else neighbor(Y) = mary;" + 
				"";
		
		String query = "for all Y in 1990..1995: neighbor(Y)";
		Expression expected = parse("if Y = 1990 then if neighbor(Y) = bob then 0.506 else if neighbor(Y) = mary then 0.494 else if neighbor(Y) = person3 then 0 else 0 else if Y = 1991 then if neighbor(Y) = bob then 0.5 else if neighbor(Y) = mary then 0.5 else if neighbor(Y) = person3 then 0 else 0 else if Y = 1992 then if neighbor(Y) = bob then 0.489 else if neighbor(Y) = mary then 0.511 else if neighbor(Y) = person3 then 0 else 0 else if Y = 1993 then if neighbor(Y) = bob then 1 else if neighbor(Y) = mary then 0 else if neighbor(Y) = person3 then 0 else 0 else if Y = 1994 then if neighbor(Y) = bob then 1 else if neighbor(Y) = mary then 0 else if neighbor(Y) = person3 then 0 else 0 else if neighbor(Y) = bob then 1 else if neighbor(Y) = mary then 0 else if neighbor(Y) = person3 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void barChartExtraDimensionSamplingTest() {
	
		String model = "" +
				"sort Person: 4, bob, mary;\r\n" + 
				"random neighbor : Person x 1990..2000 -> Person;\r\n" + 
				"random coin : Person x 1990..2000 -> [-10;10];\r\n" +
				
				"for all X in Person : for all Y in 1990..2000 : coin(X, Y) = Normal(0,1);" +
				
				"for all X in Person : for all Y in 1990..2000 : " +
				"if Y > 1995 "
				+ " then neighbor(X, Y) = bob"
				+ " else if coin(X,Y) < 0 then neighbor(X, Y) = bob else neighbor(X, Y) = mary;";
		
		String query = "for all X in Person : for all t in 1990..2000 : neighbor(X, t)";
		
		Expression expected = parse("if (X = bob) and (t = 1990) then if neighbor(X, t) = bob then 0.498 else if neighbor(X, t) = mary then 0.502 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1990) then if neighbor(X, t) = bob then 0.499 else if neighbor(X, t) = mary then 0.501 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1990) then if neighbor(X, t) = bob then 0.513 else if neighbor(X, t) = mary then 0.487 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1990) then if neighbor(X, t) = bob then 0.491 else if neighbor(X, t) = mary then 0.509 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 1991) then if neighbor(X, t) = bob then 0.487 else if neighbor(X, t) = mary then 0.513 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1991) then if neighbor(X, t) = bob then 0.492 else if neighbor(X, t) = mary then 0.508 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1991) then if neighbor(X, t) = bob then 0.495 else if neighbor(X, t) = mary then 0.505 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1991) then if neighbor(X, t) = bob then 0.532 else if neighbor(X, t) = mary then 0.468 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 1992) then if neighbor(X, t) = bob then 0.478 else if neighbor(X, t) = mary then 0.522 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1992) then if neighbor(X, t) = bob then 0.486 else if neighbor(X, t) = mary then 0.514 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1992) then if neighbor(X, t) = bob then 0.504 else if neighbor(X, t) = mary then 0.496 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1992) then if neighbor(X, t) = bob then 0.492 else if neighbor(X, t) = mary then 0.508 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 1993) then if neighbor(X, t) = bob then 0.49 else if neighbor(X, t) = mary then 0.51 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1993) then if neighbor(X, t) = bob then 0.503 else if neighbor(X, t) = mary then 0.497 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1993) then if neighbor(X, t) = bob then 0.523 else if neighbor(X, t) = mary then 0.477 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1993) then if neighbor(X, t) = bob then 0.5 else if neighbor(X, t) = mary then 0.5 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 1994) then if neighbor(X, t) = bob then 0.482 else if neighbor(X, t) = mary then 0.518 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1994) then if neighbor(X, t) = bob then 0.48 else if neighbor(X, t) = mary then 0.52 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1994) then if neighbor(X, t) = bob then 0.519 else if neighbor(X, t) = mary then 0.481 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1994) then if neighbor(X, t) = bob then 0.506 else if neighbor(X, t) = mary then 0.494 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 1995) then if neighbor(X, t) = bob then 0.474 else if neighbor(X, t) = mary then 0.526 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1995) then if neighbor(X, t) = bob then 0.481 else if neighbor(X, t) = mary then 0.519 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1995) then if neighbor(X, t) = bob then 0.516 else if neighbor(X, t) = mary then 0.484 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1995) then if neighbor(X, t) = bob then 0.503 else if neighbor(X, t) = mary then 0.497 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 1996) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1996) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1996) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1996) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 1997) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1997) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1997) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1997) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 1998) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1998) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1998) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1998) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 1999) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 1999) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 1999) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person4) and (t = 1999) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = bob) and (t = 2000) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = mary) and (t = 2000) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if (X = person3) and (t = 2000) then if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0 else if neighbor(X, t) = bob then 1 else if neighbor(X, t) = mary then 0 else if neighbor(X, t) = person3 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 5;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	@Test
	public void simpleFoodSecurityTest() {

		String model = "" +
				"random food_availability : 0..6 -> [-4000;4000];\r\n" + 
				"random food_availabilityDif : 0..6 -> [-4000;4000];\r\n" + 
				"\r\n" + 
				"for all t in 0..6 : \r\n" + 
				"	if t = 0\r\n" + 
				"	   then\r\n" + 
				"	     food_availability(t) = 0\r\n" + 
				"	   else\r\n" + 
				"	     food_availability(t) = Normal(food_availability(t-1) \r\n" + 
				"		 + food_availabilityDif(t-1), 10);\r\n" + 
				"\r\n" + 
				"for all t in 0..6 : \r\n" + 
				"	if t = 0\r\n" + 
				"	   then\r\n" + 
				"	     food_availabilityDif(t) = 100\r\n" + 
				"	   else\r\n" + 
				"	     food_availabilityDif(t) = food_availability(t) - food_availability(t-1);";
		
		String query = "for all t in 0..6 : food_availability(t)";
		Expression expected = parse("if t = 0 then if food_availability(t) < -3833.333 then 0 else if food_availability(t) < -3500 then 0 else if food_availability(t) < -3166.667 then 0 else if food_availability(t) < -2833.333 then 0 else if food_availability(t) < -2500 then 0 else if food_availability(t) < -2166.667 then 0 else if food_availability(t) < -1833.333 then 0 else if food_availability(t) < -1500 then 0 else if food_availability(t) < -1166.667 then 0 else if food_availability(t) < -833.333 then 0 else if food_availability(t) < -500 then 0 else if food_availability(t) < -166.667 then 0 else if food_availability(t) < 166.667 then 1 else if food_availability(t) < 500 then 0 else if food_availability(t) < 833.333 then 0 else if food_availability(t) < 1166.667 then 0 else if food_availability(t) < 1500 then 0 else if food_availability(t) < 1833.333 then 0 else if food_availability(t) < 2166.667 then 0 else if food_availability(t) < 2500 then 0 else if food_availability(t) < 2833.333 then 0 else if food_availability(t) < 3166.667 then 0 else if food_availability(t) < 3500 then 0 else if food_availability(t) < 3833.333 then 0 else 0 else if t = 1 then if food_availability(t) < -3833.333 then 0 else if food_availability(t) < -3500 then 0 else if food_availability(t) < -3166.667 then 0 else if food_availability(t) < -2833.333 then 0 else if food_availability(t) < -2500 then 0 else if food_availability(t) < -2166.667 then 0 else if food_availability(t) < -1833.333 then 0 else if food_availability(t) < -1500 then 0 else if food_availability(t) < -1166.667 then 0 else if food_availability(t) < -833.333 then 0 else if food_availability(t) < -500 then 0 else if food_availability(t) < -166.667 then 0 else if food_availability(t) < 166.667 then 1 else if food_availability(t) < 500 then 0 else if food_availability(t) < 833.333 then 0 else if food_availability(t) < 1166.667 then 0 else if food_availability(t) < 1500 then 0 else if food_availability(t) < 1833.333 then 0 else if food_availability(t) < 2166.667 then 0 else if food_availability(t) < 2500 then 0 else if food_availability(t) < 2833.333 then 0 else if food_availability(t) < 3166.667 then 0 else if food_availability(t) < 3500 then 0 else if food_availability(t) < 3833.333 then 0 else 0 else if t = 2 then if food_availability(t) < -3833.333 then 0 else if food_availability(t) < -3500 then 0 else if food_availability(t) < -3166.667 then 0 else if food_availability(t) < -2833.333 then 0 else if food_availability(t) < -2500 then 0 else if food_availability(t) < -2166.667 then 0 else if food_availability(t) < -1833.333 then 0 else if food_availability(t) < -1500 then 0 else if food_availability(t) < -1166.667 then 0 else if food_availability(t) < -833.333 then 0 else if food_availability(t) < -500 then 0 else if food_availability(t) < -166.667 then 0 else if food_availability(t) < 166.667 then 0.085 else if food_availability(t) < 500 then 0.915 else if food_availability(t) < 833.333 then 0 else if food_availability(t) < 1166.667 then 0 else if food_availability(t) < 1500 then 0 else if food_availability(t) < 1833.333 then 0 else if food_availability(t) < 2166.667 then 0 else if food_availability(t) < 2500 then 0 else if food_availability(t) < 2833.333 then 0 else if food_availability(t) < 3166.667 then 0 else if food_availability(t) < 3500 then 0 else if food_availability(t) < 3833.333 then 0 else 0 else if t = 3 then if food_availability(t) < -3833.333 then 0 else if food_availability(t) < -3500 then 0 else if food_availability(t) < -3166.667 then 0 else if food_availability(t) < -2833.333 then 0 else if food_availability(t) < -2500 then 0 else if food_availability(t) < -2166.667 then 0 else if food_availability(t) < -1833.333 then 0 else if food_availability(t) < -1500 then 0 else if food_availability(t) < -1166.667 then 0 else if food_availability(t) < -833.333 then 0 else if food_availability(t) < -500 then 0 else if food_availability(t) < -166.667 then 0 else if food_availability(t) < 166.667 then 0 else if food_availability(t) < 500 then 1 else if food_availability(t) < 833.333 then 0 else if food_availability(t) < 1166.667 then 0 else if food_availability(t) < 1500 then 0 else if food_availability(t) < 1833.333 then 0 else if food_availability(t) < 2166.667 then 0 else if food_availability(t) < 2500 then 0 else if food_availability(t) < 2833.333 then 0 else if food_availability(t) < 3166.667 then 0 else if food_availability(t) < 3500 then 0 else if food_availability(t) < 3833.333 then 0 else 0 else if t = 4 then if food_availability(t) < -3833.333 then 0 else if food_availability(t) < -3500 then 0 else if food_availability(t) < -3166.667 then 0 else if food_availability(t) < -2833.333 then 0 else if food_availability(t) < -2500 then 0 else if food_availability(t) < -2166.667 then 0 else if food_availability(t) < -1833.333 then 0 else if food_availability(t) < -1500 then 0 else if food_availability(t) < -1166.667 then 0 else if food_availability(t) < -833.333 then 0 else if food_availability(t) < -500 then 0 else if food_availability(t) < -166.667 then 0 else if food_availability(t) < 166.667 then 0 else if food_availability(t) < 500 then 0.969 else if food_availability(t) < 833.333 then 0.031 else if food_availability(t) < 1166.667 then 0 else if food_availability(t) < 1500 then 0 else if food_availability(t) < 1833.333 then 0 else if food_availability(t) < 2166.667 then 0 else if food_availability(t) < 2500 then 0 else if food_availability(t) < 2833.333 then 0 else if food_availability(t) < 3166.667 then 0 else if food_availability(t) < 3500 then 0 else if food_availability(t) < 3833.333 then 0 else 0 else if t = 5 then if food_availability(t) < -3833.333 then 0 else if food_availability(t) < -3500 then 0 else if food_availability(t) < -3166.667 then 0 else if food_availability(t) < -2833.333 then 0 else if food_availability(t) < -2500 then 0 else if food_availability(t) < -2166.667 then 0 else if food_availability(t) < -1833.333 then 0 else if food_availability(t) < -1500 then 0 else if food_availability(t) < -1166.667 then 0 else if food_availability(t) < -833.333 then 0 else if food_availability(t) < -500 then 0 else if food_availability(t) < -166.667 then 0 else if food_availability(t) < 166.667 then 0 else if food_availability(t) < 500 then 0.507 else if food_availability(t) < 833.333 then 0.493 else if food_availability(t) < 1166.667 then 0 else if food_availability(t) < 1500 then 0 else if food_availability(t) < 1833.333 then 0 else if food_availability(t) < 2166.667 then 0 else if food_availability(t) < 2500 then 0 else if food_availability(t) < 2833.333 then 0 else if food_availability(t) < 3166.667 then 0 else if food_availability(t) < 3500 then 0 else if food_availability(t) < 3833.333 then 0 else 0 else if food_availability(t) < -3833.333 then 0 else if food_availability(t) < -3500 then 0 else if food_availability(t) < -3166.667 then 0 else if food_availability(t) < -2833.333 then 0 else if food_availability(t) < -2500 then 0 else if food_availability(t) < -2166.667 then 0 else if food_availability(t) < -1833.333 then 0 else if food_availability(t) < -1500 then 0 else if food_availability(t) < -1166.667 then 0 else if food_availability(t) < -833.333 then 0 else if food_availability(t) < -500 then 0 else if food_availability(t) < -166.667 then 0 else if food_availability(t) < 166.667 then 0 else if food_availability(t) < 500 then 0.154 else if food_availability(t) < 833.333 then 0.838 else if food_availability(t) < 1166.667 then 0.008 else if food_availability(t) < 1500 then 0 else if food_availability(t) < 1833.333 then 0 else if food_availability(t) < 2166.667 then 0 else if food_availability(t) < 2500 then 0 else if food_availability(t) < 2833.333 then 0 else if food_availability(t) < 3166.667 then 0 else if food_availability(t) < 3500 then 0 else if food_availability(t) < 3833.333 then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 25;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}
	
	@Test
	public void randomWalkSamplingTest() {

		String model = "" +
				"random food_availability : 0..10 -> [-50;50];\r\n" + 
				"\r\n" + 
				"for all t in 0..10 : \r\n" + 
				"	if t = 0\r\n" + 
				"	   then\r\n" + 
				"	     food_availability(t) = 0\r\n" + 
				"	   else\r\n" + 
				"	     food_availability(t) = Normal(food_availability(t-1), 10);";
		
		String query = "for all t in 0..10 : food_availability(t)";
		Expression expected = parse("if t = 0 then if food_availability(t) < -47.917 then 0 else if food_availability(t) < -43.75 then 0 else if food_availability(t) < -39.583 then 0 else if food_availability(t) < -35.417 then 0 else if food_availability(t) < -31.25 then 0 else if food_availability(t) < -27.083 then 0 else if food_availability(t) < -22.917 then 0 else if food_availability(t) < -18.75 then 0 else if food_availability(t) < -14.583 then 0 else if food_availability(t) < -10.417 then 0 else if food_availability(t) < -6.25 then 0 else if food_availability(t) < -2.083 then 0 else if food_availability(t) < 2.083 then 1 else if food_availability(t) < 6.25 then 0 else if food_availability(t) < 10.417 then 0 else if food_availability(t) < 14.583 then 0 else if food_availability(t) < 18.75 then 0 else if food_availability(t) < 22.917 then 0 else if food_availability(t) < 27.083 then 0 else if food_availability(t) < 31.25 then 0 else if food_availability(t) < 35.417 then 0 else if food_availability(t) < 39.583 then 0 else if food_availability(t) < 43.75 then 0 else if food_availability(t) < 47.917 then 0 else 0 else if t = 1 then if food_availability(t) < -47.917 then 0 else if food_availability(t) < -43.75 then 0 else if food_availability(t) < -39.583 then 0 else if food_availability(t) < -35.417 then 0 else if food_availability(t) < -31.25 then 0 else if food_availability(t) < -27.083 then 0.002 else if food_availability(t) < -22.917 then 0.007 else if food_availability(t) < -18.75 then 0.025 else if food_availability(t) < -14.583 then 0.048 else if food_availability(t) < -10.417 then 0.075 else if food_availability(t) < -6.25 then 0.114 else if food_availability(t) < -2.083 then 0.151 else if food_availability(t) < 2.083 then 0.153 else if food_availability(t) < 6.25 then 0.159 else if food_availability(t) < 10.417 then 0.109 else if food_availability(t) < 14.583 then 0.078 else if food_availability(t) < 18.75 then 0.045 else if food_availability(t) < 22.917 then 0.022 else if food_availability(t) < 27.083 then 0.007 else if food_availability(t) < 31.25 then 0.004 else if food_availability(t) < 35.417 then 0 else if food_availability(t) < 39.583 then 0.001 else if food_availability(t) < 43.75 then 0 else if food_availability(t) < 47.917 then 0 else 0 else if t = 2 then if food_availability(t) < -47.917 then 0 else if food_availability(t) < -43.75 then 0.001 else if food_availability(t) < -39.583 then 0.001 else if food_availability(t) < -35.417 then 0.003 else if food_availability(t) < -31.25 then 0.007 else if food_availability(t) < -27.083 then 0.013 else if food_availability(t) < -22.917 then 0.029 else if food_availability(t) < -18.75 then 0.038 else if food_availability(t) < -14.583 then 0.06 else if food_availability(t) < -10.417 then 0.082 else if food_availability(t) < -6.25 then 0.086 else if food_availability(t) < -2.083 then 0.114 else if food_availability(t) < 2.083 then 0.118 else if food_availability(t) < 6.25 then 0.109 else if food_availability(t) < 10.417 then 0.108 else if food_availability(t) < 14.583 then 0.079 else if food_availability(t) < 18.75 then 0.06 else if food_availability(t) < 22.917 then 0.04 else if food_availability(t) < 27.083 then 0.023 else if food_availability(t) < 31.25 then 0.018 else if food_availability(t) < 35.417 then 0.008 else if food_availability(t) < 39.583 then 0.003 else if food_availability(t) < 43.75 then 0.001 else if food_availability(t) < 47.917 then 9.257E-6 else 0 else if t = 3 then if food_availability(t) < -47.917 then 0.001 else if food_availability(t) < -43.75 then 0.004 else if food_availability(t) < -39.583 then 0.007 else if food_availability(t) < -35.417 then 0.012 else if food_availability(t) < -31.25 then 0.016 else if food_availability(t) < -27.083 then 0.024 else if food_availability(t) < -22.917 then 0.031 else if food_availability(t) < -18.75 then 0.041 else if food_availability(t) < -14.583 then 0.066 else if food_availability(t) < -10.417 then 0.077 else if food_availability(t) < -6.25 then 0.084 else if food_availability(t) < -2.083 then 0.093 else if food_availability(t) < 2.083 then 0.097 else if food_availability(t) < 6.25 then 0.09 else if food_availability(t) < 10.417 then 0.076 else if food_availability(t) < 14.583 then 0.076 else if food_availability(t) < 18.75 then 0.061 else if food_availability(t) < 22.917 then 0.048 else if food_availability(t) < 27.083 then 0.033 else if food_availability(t) < 31.25 then 0.029 else if food_availability(t) < 35.417 then 0.013 else if food_availability(t) < 39.583 then 0.01 else if food_availability(t) < 43.75 then 0.006 else if food_availability(t) < 47.917 then 0.003 else 0.001 else if t = 4 then if food_availability(t) < -47.917 then 0.004 else if food_availability(t) < -43.75 then 0.004 else if food_availability(t) < -39.583 then 0.011 else if food_availability(t) < -35.417 then 0.012 else if food_availability(t) < -31.25 then 0.02 else if food_availability(t) < -27.083 then 0.028 else if food_availability(t) < -22.917 then 0.041 else if food_availability(t) < -18.75 then 0.052 else if food_availability(t) < -14.583 then 0.054 else if food_availability(t) < -10.417 then 0.067 else if food_availability(t) < -6.25 then 0.076 else if food_availability(t) < -2.083 then 0.086 else if food_availability(t) < 2.083 then 0.088 else if food_availability(t) < 6.25 then 0.08 else if food_availability(t) < 10.417 then 0.079 else if food_availability(t) < 14.583 then 0.067 else if food_availability(t) < 18.75 then 0.057 else if food_availability(t) < 22.917 then 0.05 else if food_availability(t) < 27.083 then 0.042 else if food_availability(t) < 31.25 then 0.032 else if food_availability(t) < 35.417 then 0.019 else if food_availability(t) < 39.583 then 0.016 else if food_availability(t) < 43.75 then 0.009 else if food_availability(t) < 47.917 then 0.007 else 0.002 else if t = 5 then if food_availability(t) < -47.917 then 0.004 else if food_availability(t) < -43.75 then 0.013 else if food_availability(t) < -39.583 then 0.014 else if food_availability(t) < -35.417 then 0.019 else if food_availability(t) < -31.25 then 0.026 else if food_availability(t) < -27.083 then 0.035 else if food_availability(t) < -22.917 then 0.044 else if food_availability(t) < -18.75 then 0.048 else if food_availability(t) < -14.583 then 0.054 else if food_availability(t) < -10.417 then 0.061 else if food_availability(t) < -6.25 then 0.076 else if food_availability(t) < -2.083 then 0.079 else if food_availability(t) < 2.083 then 0.075 else if food_availability(t) < 6.25 then 0.08 else if food_availability(t) < 10.417 then 0.073 else if food_availability(t) < 14.583 then 0.061 else if food_availability(t) < 18.75 then 0.054 else if food_availability(t) < 22.917 then 0.047 else if food_availability(t) < 27.083 then 0.033 else if food_availability(t) < 31.25 then 0.034 else if food_availability(t) < 35.417 then 0.025 else if food_availability(t) < 39.583 then 0.02 else if food_availability(t) < 43.75 then 0.012 else if food_availability(t) < 47.917 then 0.008 else 0.004 else if t = 6 then if food_availability(t) < -47.917 then 0.006 else if food_availability(t) < -43.75 then 0.013 else if food_availability(t) < -39.583 then 0.015 else if food_availability(t) < -35.417 then 0.022 else if food_availability(t) < -31.25 then 0.027 else if food_availability(t) < -27.083 then 0.039 else if food_availability(t) < -22.917 then 0.039 else if food_availability(t) < -18.75 then 0.046 else if food_availability(t) < -14.583 then 0.055 else if food_availability(t) < -10.417 then 0.068 else if food_availability(t) < -6.25 then 0.072 else if food_availability(t) < -2.083 then 0.069 else if food_availability(t) < 2.083 then 0.071 else if food_availability(t) < 6.25 then 0.071 else if food_availability(t) < 10.417 then 0.059 else if food_availability(t) < 14.583 then 0.06 else if food_availability(t) < 18.75 then 0.056 else if food_availability(t) < 22.917 then 0.046 else if food_availability(t) < 27.083 then 0.045 else if food_availability(t) < 31.25 then 0.036 else if food_availability(t) < 35.417 then 0.033 else if food_availability(t) < 39.583 then 0.023 else if food_availability(t) < 43.75 then 0.018 else if food_availability(t) < 47.917 then 0.011 else 0.004 else if t = 7 then if food_availability(t) < -47.917 then 0.005 else if food_availability(t) < -43.75 then 0.016 else if food_availability(t) < -39.583 then 0.021 else if food_availability(t) < -35.417 then 0.02 else if food_availability(t) < -31.25 then 0.03 else if food_availability(t) < -27.083 then 0.038 else if food_availability(t) < -22.917 then 0.04 else if food_availability(t) < -18.75 then 0.049 else if food_availability(t) < -14.583 then 0.051 else if food_availability(t) < -10.417 then 0.063 else if food_availability(t) < -6.25 then 0.061 else if food_availability(t) < -2.083 then 0.072 else if food_availability(t) < 2.083 then 0.067 else if food_availability(t) < 6.25 then 0.064 else if food_availability(t) < 10.417 then 0.066 else if food_availability(t) < 14.583 then 0.06 else if food_availability(t) < 18.75 then 0.054 else if food_availability(t) < 22.917 then 0.047 else if food_availability(t) < 27.083 then 0.041 else if food_availability(t) < 31.25 then 0.037 else if food_availability(t) < 35.417 then 0.031 else if food_availability(t) < 39.583 then 0.023 else if food_availability(t) < 43.75 then 0.019 else if food_availability(t) < 47.917 then 0.018 else 0.006 else if t = 8 then if food_availability(t) < -47.917 then 0.007 else if food_availability(t) < -43.75 then 0.016 else if food_availability(t) < -39.583 then 0.02 else if food_availability(t) < -35.417 then 0.027 else if food_availability(t) < -31.25 then 0.032 else if food_availability(t) < -27.083 then 0.04 else if food_availability(t) < -22.917 then 0.043 else if food_availability(t) < -18.75 then 0.044 else if food_availability(t) < -14.583 then 0.055 else if food_availability(t) < -10.417 then 0.058 else if food_availability(t) < -6.25 then 0.059 else if food_availability(t) < -2.083 then 0.063 else if food_availability(t) < 2.083 then 0.065 else if food_availability(t) < 6.25 then 0.062 else if food_availability(t) < 10.417 then 0.059 else if food_availability(t) < 14.583 then 0.059 else if food_availability(t) < 18.75 then 0.055 else if food_availability(t) < 22.917 then 0.052 else if food_availability(t) < 27.083 then 0.043 else if food_availability(t) < 31.25 then 0.036 else if food_availability(t) < 35.417 then 0.034 else if food_availability(t) < 39.583 then 0.027 else if food_availability(t) < 43.75 then 0.02 else if food_availability(t) < 47.917 then 0.016 else 0.008 else if t = 9 then if food_availability(t) < -47.917 then 0.008 else if food_availability(t) < -43.75 then 0.019 else if food_availability(t) < -39.583 then 0.022 else if food_availability(t) < -35.417 then 0.028 else if food_availability(t) < -31.25 then 0.033 else if food_availability(t) < -27.083 then 0.038 else if food_availability(t) < -22.917 then 0.044 else if food_availability(t) < -18.75 then 0.049 else if food_availability(t) < -14.583 then 0.054 else if food_availability(t) < -10.417 then 0.057 else if food_availability(t) < -6.25 then 0.064 else if food_availability(t) < -2.083 then 0.058 else if food_availability(t) < 2.083 then 0.06 else if food_availability(t) < 6.25 then 0.059 else if food_availability(t) < 10.417 then 0.062 else if food_availability(t) < 14.583 then 0.053 else if food_availability(t) < 18.75 then 0.051 else if food_availability(t) < 22.917 then 0.047 else if food_availability(t) < 27.083 then 0.042 else if food_availability(t) < 31.25 then 0.039 else if food_availability(t) < 35.417 then 0.034 else if food_availability(t) < 39.583 then 0.025 else if food_availability(t) < 43.75 then 0.027 else if food_availability(t) < 47.917 then 0.021 else 0.006 else if food_availability(t) < -47.917 then 0.008 else if food_availability(t) < -43.75 then 0.023 else if food_availability(t) < -39.583 then 0.026 else if food_availability(t) < -35.417 then 0.027 else if food_availability(t) < -31.25 then 0.035 else if food_availability(t) < -27.083 then 0.038 else if food_availability(t) < -22.917 then 0.045 else if food_availability(t) < -18.75 then 0.048 else if food_availability(t) < -14.583 then 0.048 else if food_availability(t) < -10.417 then 0.057 else if food_availability(t) < -6.25 then 0.055 else if food_availability(t) < -2.083 then 0.057 else if food_availability(t) < 2.083 then 0.058 else if food_availability(t) < 6.25 then 0.061 else if food_availability(t) < 10.417 then 0.059 else if food_availability(t) < 14.583 then 0.058 else if food_availability(t) < 18.75 then 0.05 else if food_availability(t) < 22.917 then 0.048 else if food_availability(t) < 27.083 then 0.045 else if food_availability(t) < 31.25 then 0.041 else if food_availability(t) < 35.417 then 0.033 else if food_availability(t) < 39.583 then 0.029 else if food_availability(t) < 43.75 then 0.026 else if food_availability(t) < 47.917 then 0.02 else 0.01");
		int initialNumberOfSamples = 10000;
		int numberOfDiscreteValues = 25;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true);
	}

	
	///////////////////////////////

	private void runTest(String model, String query, Expression expected, int initialNumberOfSamples, int numberOfDiscreteValues, boolean quantitativeTests) {
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, quantitativeTests, /* generate graph */ true, DefaultSample.makeFreshSample());
	}

	private void runTest(String model, String query, Expression expected, int initialNumberOfSamples, int numberOfDiscreteValues, boolean quantitativeTests, Sample conditioningSample) {
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, quantitativeTests, /* generate graph */ true, conditioningSample);
	}
	
	private void runTest(String model, String query, Expression expected, int initialNumberOfSamples, int numberOfDiscreteValues, boolean quantitativeTests, boolean generateGraph) {
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, quantitativeTests, generateGraph, DefaultSample.makeFreshSample());
	}
	
	private void runTest(String model, String query, Expression expected, int initialNumberOfSamples, int numberOfDiscreteValues, boolean quantitativeTests, boolean generateGraph, Sample conditioningSample) {

		if (generateGraph) {
			generateGraph(model, query, initialNumberOfSamples, numberOfDiscreteValues, conditioningSample);
		}
		
		if (quantitativeTests) {
			runQuantitativeTests(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, conditioningSample);
		}
	}

	private void runQuantitativeTests(String model, String query, Expression expected, int initialNumberOfSamples,
			int numberOfDiscreteValues, Sample conditioningSample) {
		// if the number of samples is just one, the test problem should be deterministic
		// (the user is betting on getting the right answer in one shot)
		// but we still run the complete test several times to increase the changes of detecting randomness.
		boolean testProblemIsMeantToBeDeterministic = initialNumberOfSamples == 1;
		int numberOfCompletTests = testProblemIsMeantToBeDeterministic? 10 : 1;
		repeat(
				numberOfCompletTests, 
				testIndex -> runNumericalTestWithCorrectPrecision(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, testProblemIsMeantToBeDeterministic, conditioningSample, testIndex));
	}

	private void generateGraph(String model, String query, int initialNumberOfSamples, int numberOfDiscreteValues, Sample conditioningSample) {

		if (System.getProperty(PROPERTY_KEY_GENERATING_GRAPH_FILE) != null) {
			ExpressionWithProbabilityFunction conditioned = computeConditionedResult(model, query, initialNumberOfSamples, numberOfDiscreteValues, conditioningSample);
			Function conditionedFunction = conditioned.getDiscretizedConditionalProbabilityDistributionFunction();
//			int queryIndex = Util.getIndexOfFirstSatisfyingPredicateOrMinusOne(conditionedFunction.getSetOfInputVariables().getVariables(), v -> v.getName().equals(query));
			int queryIndex = conditioned.getDiscretizedConditionalProbabilityDistributionFunctionQueryIndex();
			Functions functions = Functions.functions(conditionedFunction);
			myAssert(queryIndex != -1, () -> "Query variable " + query + " not found in variables " + conditionedFunction.getSetOfInputVariables().getVariables());
			GraphSet.plot(functions, queryIndex, GRAPH_FILENAME_WITHOUT_EXTENSION);
		}
	}

	private void runNumericalTestWithCorrectPrecision(String model, String query, Expression expected, int initialNumberOfSamples, int numberOfDiscreteValues, boolean deterministic, Sample conditioningSample, int testIndex) {
		
		int oldPrecision = ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(3);
		try {
			
			runNumericalTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, deterministic, conditioningSample, testIndex);
			
		} catch (Throwable t) {
			throw t;
		}
		finally {
			ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(oldPrecision);
		}
	}

	private void runNumericalTest(String model, String query, Expression expected, int initialNumberOfSamples, int numberOfDiscreteValues, boolean deterministic, Sample conditioningSample, int testIndex) {
		ExpressionWithProbabilityFunction conditioned = computeConditionedResult(model, query, initialNumberOfSamples, numberOfDiscreteValues, conditioningSample);
		printAndCompare(query, conditioned, expected, deterministic, conditioningSample, testIndex);
	}

	private ExpressionWithProbabilityFunction computeConditionedResult(String model, String query, int initialNumberOfSamples, int numberOfDiscreteValues, Sample conditioningSample) {
		ExpressionWithProbabilityFunction solution = computeUnconditionedResult(model, query, initialNumberOfSamples, numberOfDiscreteValues);
//		ExpressionSamplingFactor conditioned = solution.condition(conditioningSample);
//		return conditioned;
		return solution;
	}

	private ExpressionWithProbabilityFunction computeUnconditionedResult(String model, String query, int initialNumberOfSamples, int numberOfDiscreteValues) {
		HOGMProblemResult hogmResult = computeResult(model, query, initialNumberOfSamples, numberOfDiscreteValues);
		assertNoErrors(hogmResult);
		Expression result = hogmResult.getResult();
		ExpressionWithProbabilityFunction solution = (ExpressionWithProbabilityFunction) result;
		return solution;
	}

	private HOGMProblemResult computeResult(String model, String query, int initialNumberOfSamples, int numberOfDiscreteValues) {
		HOGMMultiQuerySamplingProblemSolver solver = 
				new HOGMMultiQuerySamplingProblemSolver(
						model, 
						list(query),
						v -> numberOfDiscreteValues, 
						initialNumberOfSamples, 
						new Random());
		
		List<? extends HOGMProblemResult> results = solver.getResults();
		assertEquals(1, results.size());
		HOGMProblemResult result = getFirst(results);
		return result;
	}

	private void assertNoErrors(HOGMProblemResult result) {
		result.getErrors().stream().forEach(e -> println(e));
		assertFalse(result.hasErrors());
	}

	private void printAndCompare(String query, ExpressionWithProbabilityFunction conditioned, Expression expected, boolean deterministic, Sample conditioningSample, int testIndex) {
		
		println("query: " + query);
		println("expected: " + expected);
		println("actual  : " + conditioned);
		if (expected != null) {
			String reasonForDifference = areEqualUpToNumericProportion(expected, conditioned, 0.5, 0.01);
			if (reasonForDifference != "") {
				println("Failure: " + reasonForDifference);
			}
			assertEquals("", reasonForDifference);
		}
	}
}
