package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base;

import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.repeat;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.configuration.ConfigurationForTestsOnBatchOfFactorNetworks;
import com.sri.ai.util.Timer;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

/** 
 * Abstract class for running a list of algorithms on a batch of problems.
 * The actual generation of problem is done by extension classes implementing the abstract methods.
 * @author braz
 *
 */
public abstract class AbstractTestsOnBatchOfFactorNetworks {

	////////// DATA MEMBERS
	
	private List<Pair<String, BinaryFunction<Variable, FactorNetwork, Factor>>> algorithms;

	private int[] totalTime;
	
	private ConfigurationForTestsOnBatchOfFactorNetworks configuration;

	////////// ABSTRACT METHODS
	
	protected abstract List<Pair<String,BinaryFunction<Variable,FactorNetwork,Factor>>> makeAlgorithms();

	protected abstract NullaryFunction<Pair<Variable, FactorNetwork>> makeProblemGenerator();

    //////// IMPLEMENTATION METHODS
	
	protected AbstractTestsOnBatchOfFactorNetworks() {
		totalTime = new int[getAlgorithms().size()];
	}
	
	protected ConfigurationForTestsOnBatchOfFactorNetworks getConfiguration() {
		return configuration;
	}

	protected void setConfiguration(ConfigurationForTestsOnBatchOfFactorNetworks configuration) {
		this.configuration = configuration;
	}

	protected List<Pair<String, BinaryFunction<Variable, FactorNetwork, Factor>>> getAlgorithms() {
		if (algorithms == null) {
			algorithms = makeAlgorithms();
		}
		return algorithms;
	}

	protected int getNumberOfTests() {
		return getConfiguration().getNumberOfTests();
	}

	public void run(ConfigurationForTestsOnBatchOfFactorNetworks configuration) {
		setConfiguration(configuration);
		run();
	}
	
	public void run() {
		int numberOfTests = getNumberOfTests();
		var problemGenerator = makeProblemGenerator();
		repeat(numberOfTests, testNumber -> runTest(testNumber, problemGenerator));
		printTotalTimes();
	}

	private void runTest(int testNumber, NullaryFunction<Pair<Variable, FactorNetwork>> problemGenerator) {
		
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

	private ArrayList<Pair<Factor, Long>> computeResults(Variable query, FactorNetwork factorNetwork) {
		ArrayList<Pair<Factor, Long>> resultsAndTimes = new ArrayList<>(getAlgorithms().size());
		for (int i = 0; i != getAlgorithms().size(); i++) {
			var name = getAlgorithms().get(i).first;
			var algorithm = getAlgorithms().get(i).second;
			ThreadExplanationLogger.setIsActive(algorithm instanceof VariableEliminationSolver);
			var resultAndTime = resultAndTime(name, () -> algorithm.apply(query, factorNetwork));
			ThreadExplanationLogger.setIsActive(false);
			totalTime[i] += resultAndTime.second;
			resultsAndTimes.add(resultAndTime);
			println();
		}
		return resultsAndTimes;
	}

	private Pair<Factor, Long> resultAndTime(String name, NullaryFunction<Factor> algorithmInstance) {
		println("Running " + name + "...");
		var resultAndTime = Timer.timeAndGetResult(algorithmInstance);
		println("Done running  " + name + ". Time: " + resultAndTime.second + " ms.");
		return resultAndTime;
	}

	private void printResults(ArrayList<Pair<Factor, Long>> resultsAndTimes) {
		for (int i = 0; i != getAlgorithms().size(); i++) {
			var name = getAlgorithms().get(i).first;
			var resultAndTime = resultsAndTimes.get(i);
			println(name + ": " + resultAndTimeString(resultAndTime));
		}
		println();
	}

	private String resultAndTimeString(Pair<Factor, Long> resultAndTime) {
		return resultAndTime.first + ", " + resultAndTime.second + " ms";
	}

	private void compareResults(ArrayList<Pair<Factor, Long>> resultsAndTimes) {
		for (int i = 0; i != getAlgorithms().size() - 1; i++) {
			var name1 = getAlgorithms().get(i).first;
			var name2 = getAlgorithms().get(i + 1).first;
			var resultAndTime1 = resultsAndTimes.get(i);
			var resultAndTime2 = resultsAndTimes.get(i + 1);
			var array1 = ((TableFactor) resultAndTime1.first).getEntries();
			var array2 = ((TableFactor) resultAndTime2.first).getEntries();
			println("Comparing " + name1 + " and " + name2 + "...");
			Util.compareNumbersComponentWise(array1, array2, 0.001);
		}
		println();
	}

	private void printTotalTimes() {
		for (int i = 0; i != getAlgorithms().size(); i++) {
			var name = getAlgorithms().get(i).first;
			println("Total time " + name + ": " + totalTime[i]);
		}
	}

}