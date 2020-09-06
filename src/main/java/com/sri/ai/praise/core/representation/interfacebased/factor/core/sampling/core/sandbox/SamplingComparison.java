package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sandbox;

import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.removeNonDestructively;
import static com.sri.ai.util.function.api.functions.Functions.functions;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.translation.samplinggraph2d.DefaultSamplingFactorDiscretizedProbabilityDistributionFunction;
import com.sri.ai.util.function.api.functions.Function;
import com.sri.ai.util.function.api.functions.Functions;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Unit;
import com.sri.ai.util.function.core.variables.DefaultSetOfVariables;
import com.sri.ai.util.function.core.variables.RealVariable;
import com.sri.ai.util.graph2d.api.GraphSet;

public class SamplingComparison {

	public static void main(String[] args) {
		
		int length = 40;
		int numberOfSamples = 1000;
		int queryVariableIndex = length - 1;
		int standardDeviation = 4;
		
		// warmup
		executeAllModels(length, numberOfSamples, queryVariableIndex, standardDeviation);
		
		executeAllModels(length, numberOfSamples, queryVariableIndex, standardDeviation);
	}

	private static void executeAllModels(int length, int numberOfSamples, int queryVariableIndex, int standardDeviation) {
		
		@SuppressWarnings("unused")
		SamplingFactor monteCarlo = new RandomWalkMonteCarlo(length, standardDeviation, new Random());
		SamplingFactor weighedGibbs = new RandomWalkWeighedGibbs(length, standardDeviation, new Random());
		@SuppressWarnings("unused")
		SamplingFactor gibbs = new RandomWalkGibbs(length, standardDeviation, new Random());

//		long mcTime = execute(monteCarlo, queryVariableIndex, numberOfSamples, "Monte Carlo");
		long wgTime = execute(weighedGibbs, queryVariableIndex, numberOfSamples, "Weighted Gibbs");
//		long gTime = execute(gibbs, queryVariableIndex, numberOfSamples, "Gibbs");
		
//		println("Monte Carlo took " + mcTime + " ms");
		println("Weighed Gibbs took " + wgTime + " ms");
//		println("Gibbs took " + gTime + " ms");
	}

	private static long execute(SamplingFactor factor, int queryVariableIndex, int numberOfSamples, String description) {
		long initialTime = System.currentTimeMillis();
		factor = makeMarginalFactor(queryVariableIndex, factor);
		plot(factor, queryVariableIndex, numberOfSamples, description);
		long finalTime = System.currentTimeMillis();
		long time = finalTime - initialTime;
		return time;
	}

	private static void plot(SamplingFactor factor, int queryVariableIndex, int numberOfSamples, String filePathnameBase) {
		RealVariable queryGraphVariable = makeVariableWithRange(queryVariableIndex);
		
		Function function = makeFunction(factor, queryGraphVariable, numberOfSamples);

		plot(function, queryGraphVariable, filePathnameBase);
	}

	private static SamplingFactor makeMarginalFactor(int queryVariableIndex, SamplingFactor factor) {
		List<? extends com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable>
		variablesOtherThanLastOne = 
				removeNonDestructively(factor.getVariables(), queryVariableIndex);
		
		factor = (SamplingFactor) factor.sumOut(variablesOtherThanLastOne);
		return factor;
	}

	private static Function makeFunction(SamplingFactor factor, RealVariable queryGraphVariable, int numberOfSamples) {
		SetOfVariables setOfVariables = new DefaultSetOfVariables(queryGraphVariable);
		
		Function function = 
				new DefaultSamplingFactorDiscretizedProbabilityDistributionFunction(
						factor, 
						setOfVariables, 
						0, 
						numberOfSamples);
		return function;
	}

	private static void plot(Function function, RealVariable queryGraphVariable, String filePathnameBase) {
		Functions functions = functions(function);
		
		GraphSet.plot(functions, queryGraphVariable, filePathnameBase);
	}

	private static RealVariable makeVariableWithRange(Integer variableIndex) {
		return new RealVariable("x" + variableIndex, Unit.NONE, "-100", "4", "100");
	}
}
