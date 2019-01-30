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
		Expression expected = null;
		int initialNumberOfSamples = 100000;
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
		conditioningSample.getAssignment().set(new DefaultExpressionVariable(parse("y")), 4.0);
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
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
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
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "p";
		expected = parse("if p then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "q";
		expected = parse("if q then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
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
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "p";
		expected = parse("if p then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "q";
		expected = parse("if q then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
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
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "q";
		expected = parse("if q then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);

		query = "p";
		expected = null; // no expected expression, because sampling should cause error before that's needed
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
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
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "p";
		expected = parse("if p then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "q";
		expected = parse("if q then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
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
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "p";
		expected = parse("if p then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "q";
		expected = parse("if q then 0 else 1");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
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
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
		
		query = "q";
		expected = parse("if q then 1 else 0");
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
	
		query = "p";
		expected = null; // should cause error
		initialNumberOfSamples = 1;
		numberOfDiscreteValues = 21;
	
		try {
			runTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, true, /* no graph for discrete values for now */ false);
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

	//@Test
	public void simpleRelationalSamplingTest() {
	
		String model = "" +
				"random x: 0..3 -> [-10;10];" +
				"for all I in 0..3 : x(I) = Normal(0.0, 3.0);" +
				"";
		
		String query = "x(0)";
		Expression expected = parse("if capital = Abiemnhom then 1 else if capital = Abyei then 0 else if capital = Akobo then 0 else 0");
		int initialNumberOfSamples = 1000;
		int numberOfDiscreteValues = 21;
	
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

		ExpressionWithProbabilityFunction conditioned = computeConditionedResult(model, query, initialNumberOfSamples, numberOfDiscreteValues, conditioningSample);
		if (System.getProperty(PROPERTY_KEY_GENERATING_GRAPH_FILE) != null) {
			Function conditionedFunction = conditioned.getDiscretizedConditionalProbabilityDistributionFunction();
			Functions functions = Functions.functions(conditionedFunction);
			int queryIndex = Util.getIndexOfFirstSatisfyingPredicateOrMinusOne(conditionedFunction.getSetOfInputVariables().getVariables(), v -> v.getName().equals(query));
			myAssert(queryIndex != -1, () -> "Query variable " + query + " not found in variables " + conditionedFunction.getSetOfInputVariables().getVariables());
			GraphSet.plot(functions, queryIndex, GRAPH_FILENAME_WITHOUT_EXTENSION);
		}
	}

	private void runNumericalTestWithCorrectPrecision(String model, String query, Expression expected, int initialNumberOfSamples, int numberOfDiscreteValues, boolean deterministic, Sample conditioningSample, int testIndex) {
		
		int oldPrecision = ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(3);
		try {
			
			runNumericalTest(model, query, expected, initialNumberOfSamples, numberOfDiscreteValues, deterministic, conditioningSample, testIndex);
			
		} catch (Throwable t) {
			throw new Error(t);
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
