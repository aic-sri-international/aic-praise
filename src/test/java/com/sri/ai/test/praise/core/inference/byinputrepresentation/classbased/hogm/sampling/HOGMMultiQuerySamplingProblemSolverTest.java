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

class HOGMMultiQuerySamplingProblemSolverTest {

	@Test
	public void normalSamplingTest() {

		String model = 
				"random x : [-10;10];" +
						"x = Normal(0.0, 15.0);"
						;

		String query = "x";
		Expression expected = parse("if x < -7.5 then 0.12 else if x < -2.5 then 0.249 else if x < 2.5 then 0.267 else if x < 7.5 then 0.249 else 0.114");
		int numberOfInitialSamples = 1000;
		int numberOfDiscreteValues = 5;

		HOGMMultiQuerySamplingProblemSolver solver = 
				new HOGMMultiQuerySamplingProblemSolver(
						model, 
						list(query),
						v -> numberOfDiscreteValues, 
						numberOfInitialSamples, 
						new Random());

		List<? extends HOGMProblemResult> results = solver.getResults();

		int oldPrecision = ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(3);

		try {

			assertEquals(1, results.size());

			HOGMProblemResult result = getFirst(results);
			result.getErrors().stream().forEach(e -> println(e));
			assertFalse(result.hasErrors());

			Expression resultValue = result.getResult();
			ExpressionSamplingFactor.sample(resultValue, 1000);

			println("query: " + query);
			println("expected: " + expected);
			println("actual  : " + resultValue);
			String reasonForDifference = areEqualUpToNumericDifference(expected, resultValue, 0.1);
			if (reasonForDifference != "") {
				println(reasonForDifference);
			}
			assertEquals("", reasonForDifference);

		} catch (Throwable t) {
			throw t;
		}
		finally {
			ExpressoConfiguration.setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(oldPrecision);
		}
	}
}
