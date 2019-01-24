package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.sampling;

import java.util.List;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.sampling.ExactBPOnSamplingFactorsExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver.HOGMMultiQueryProblemSolver;

public class HOGMMultiQuerySamplingProblemSolver extends HOGMMultiQueryProblemSolver {

	public HOGMMultiQuerySamplingProblemSolver(
			String model, 
			List<String> queries,
			Function<Expression, Integer> fromVariableToNumberOfDiscreteValues,
			int initialNumberOfSamples,
			Random random) {

		super(
				model, 
				queries, 
				new ExactBPOnSamplingFactorsExpressionBasedSolver(
						fromVariableToNumberOfDiscreteValues, 
						initialNumberOfSamples, 
						random));
	}

	/**
	 * Same as {@link #HOGMMultiQuerySamplingProblemSolver(String, List, Function, int, Random)}
	 * with default of 10 discrete values per variable, 10000 initial samples, and a unique seed {@link Random}.
	 * @param model
	 * @param queries
	 */
	public HOGMMultiQuerySamplingProblemSolver(String model, List<String> queries) {
		this(model, queries, v -> 10, 10000, new Random()); 
	}
}
