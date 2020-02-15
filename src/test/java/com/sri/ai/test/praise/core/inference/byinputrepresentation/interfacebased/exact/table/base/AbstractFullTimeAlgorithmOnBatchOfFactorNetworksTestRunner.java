package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base;

import static com.sri.ai.util.Util.compareNumbersComponentWise;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base.configuration.ConfigurationForTestsOnBatchOfFactorNetworks;
import com.sri.ai.util.Timer;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

/** 
 * Abstract class for running a list of "full-time" algorithms on a batch of problems,
 * where "full-time" means that the whole operation is performed in one shot, without intermediary stages.
 * The actual generation of problem is done by extension classes implementing the abstract methods.
 * 
 * @author braz
 *
 */
public abstract class AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner<Result, Configuration extends ConfigurationForTestsOnBatchOfFactorNetworks<Result>> 
extends AbstractBatchOfFactorNetworksTestRunner<Result, Configuration> {

	private int[] totalTimeForEachAlgorithm;
	
	protected AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner(Configuration configuration) {
		super(configuration);
		totalTimeForEachAlgorithm = new int[getAlgorithms().size()];
	}
	
	@Override
	protected void runTest(int testNumber, NullaryFunction<Pair<Variable, FactorNetwork>> problemGenerator) {
		
		var queryAndFactorNetwork = problemGenerator.apply();
		Variable query = queryAndFactorNetwork.first;
		FactorNetwork factorNetwork = queryAndFactorNetwork.second;
		
		println();
		println("********************");
		println("Test #" + testNumber);
		println();
		println("Number of variables: " + factorNetwork.getVariables().size());
		println("Number of factors: " + factorNetwork.getFactors().size());
		println();
		
		var resultsAndTimes = computeResults(query, factorNetwork);
		
		printResults(resultsAndTimes);
		
		compareResults(resultsAndTimes);

		println("Done!\n");
	}

	private ArrayList<Pair<Result, Long>> computeResults(Variable query, FactorNetwork factorNetwork) {
		ArrayList<Pair<Result, Long>> resultsAndTimes = new ArrayList<>(getAlgorithms().size());
		for (int i = 0; i != getAlgorithms().size(); i++) {
			var name = getAlgorithms().get(i).first;
			var algorithm = getAlgorithms().get(i).second;
			ThreadExplanationLogger.setIsActive(algorithm instanceof VariableEliminationSolver);
			var resultAndTime = resultAndTime(name, () -> algorithm.apply(query, factorNetwork));
			ThreadExplanationLogger.setIsActive(false);
			totalTimeForEachAlgorithm[i] += resultAndTime.second;
			resultsAndTimes.add(resultAndTime);
			println();
		}
		return resultsAndTimes;
	}

	private Pair<Result, Long> resultAndTime(String name, NullaryFunction<Result> algorithmInstance) {
		println("Running " + name + "...");
		var resultAndTime = Timer.timeAndGetResult(algorithmInstance);
		println("Done running  " + name + ". Time: " + resultAndTime.second + " ms.");
		return resultAndTime;
	}

	private void printResults(ArrayList<Pair<Result, Long>> resultsAndTimes) {
		for (int i = 0; i != getAlgorithms().size(); i++) {
			var name = getAlgorithms().get(i).first;
			var resultAndTime = resultsAndTimes.get(i);
			println(name + ": " + resultAndTimeString(resultAndTime));
		}
		println();
	}

	private String resultAndTimeString(Pair<Result, Long> resultAndTime) {
		return resultAndTime.first + ", " + resultAndTime.second + " ms";
	}

	private void compareResults(ArrayList<Pair<Result, Long>> resultsAndTimes) {
		for (int i = 0; i != getAlgorithms().size() - 1; i++) {
			var name1 = getAlgorithms().get(i).first;
			var name2 = getAlgorithms().get(i + 1).first;
			var resultAndTime1 = resultsAndTimes.get(i);
			var resultAndTime2 = resultsAndTimes.get(i + 1);
			var array1 = ((TableFactor) resultAndTime1.first).getEntries();
			var array2 = ((TableFactor) resultAndTime2.first).getEntries();
			println("Comparing " + name1 + " and " + name2 + "...");
			compareNumbersComponentWise(array1, array2, 0.001);
		}
		println();
	}

	@Override
	protected void afterRunningAllTests() {
		printTotalTimes();
	}

	private void printTotalTimes() {
		for (int i = 0; i != getAlgorithms().size(); i++) {
			var name = getAlgorithms().get(i).first;
			println("Total time " + name + ": " + totalTimeForEachAlgorithm[i]);
		}
	}

}