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
			int numberOfInitialSamples,
			Random random) {

		super(
				model, 
				queries, 
				new ExactBPOnSamplingFactorsExpressionBasedSolver(
						fromVariableToNumberOfDiscreteValues, 
						numberOfInitialSamples, 
						random));
	}
	
}
