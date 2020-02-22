package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.pair;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.repeat;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration.ConfigurationForBatchOfFactorNetworksTest;
import com.sri.ai.util.Timer;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

/** 
 * Abstract class for running a list of algorithms on a batch of factor networks.
 * The actual generation of factor networks is done by extending {@link #makeNextFactorNetwork()}.
 * The query is also performed on the first variable provided by {@link FactorNetwork#getVariables()}'s iterator.
 * 
 * @author braz
 *
 */
public abstract class AbstractBatchOfFactorNetworksTestRunner<Result, Configuration extends ConfigurationForBatchOfFactorNetworksTest<Result>> {

	//////////////////// ABSTRACT METHODS
	
	abstract protected FactorNetwork makeNextFactorNetwork();


	abstract protected void beforeExecution(
			String algorithmName,
			BinaryFunction<Variable, FactorNetwork, Result> algorithm,
			Variable query,
			FactorNetwork factorNetwork);

	abstract protected Pair<Result, Long> afterExecution(
			String algorithmName,
			BinaryFunction<Variable, FactorNetwork, Result> algorithm,
			Variable query,
			FactorNetwork factorNetwork,
			Pair<Result, Long> resultAndTime);

	protected abstract void compareResults(ArrayList<Pair<Result, Long>> resultsAndTimes, Variable query, FactorNetwork factorNetwork);

	//////////////////// DATA MEMBER
	
	private Configuration configuration;

	private int[] totalTimeForEachAlgorithm;
	
	//////////////////// CONSTRUCTOR
	
	public AbstractBatchOfFactorNetworksTestRunner(Configuration configuration) {
		this.configuration = configuration;
		this.totalTimeForEachAlgorithm = new int[getAlgorithms().size()];
	}
	
	//////////////////// MAIN FUNCTIONALITY
	
	public void run() {
		repeat(getNumberOfRuns(), testNumber -> runTest(testNumber, getProblemGenerator()));
		afterRunningAllTests();
	}

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
		
		compareResults(resultsAndTimes, query, factorNetwork);

		println("Done!\n");
	}

	private ArrayList<Pair<Result, Long>> computeResults(Variable query, FactorNetwork factorNetwork) {
		ArrayList<Pair<Result, Long>> resultsAndTimes = new ArrayList<>(getAlgorithms().size());
		for (int algorithmIndex = 0; algorithmIndex != getAlgorithms().size(); algorithmIndex++) {
			var name = getAlgorithms().get(algorithmIndex).first;
			var algorithm = getAlgorithms().get(algorithmIndex).second;
			var resultAndTime = execute(name, algorithm, query, factorNetwork);
			addToTotalTime(algorithmIndex, resultAndTime.second);
			resultsAndTimes.add(resultAndTime);
		}
		return resultsAndTimes;
	}

	protected Pair<Result, Long> execute(
			String algorithmName,
			BinaryFunction<Variable, FactorNetwork, Result> algorithm,
			Variable query,
			FactorNetwork factorNetwork) {
		
		beforeExecution(algorithmName, algorithm, query, factorNetwork);
		var resultAndTime = Timer.getResultAndTime(() -> algorithm.apply(query, factorNetwork));
		resultAndTime = afterExecution(algorithmName, algorithm, query, factorNetwork, resultAndTime);
		return resultAndTime;
	}

	//////////////////// TIME-KEEPING

	protected int addToTotalTime(int i, Long executionTime) {
		return totalTimeForEachAlgorithm[i] += executionTime;
	}

	protected int getTotalTimeForAlgorithm(int i) {
		return totalTimeForEachAlgorithm[i];
	}

	//////////////////// REPORTING FOR EACH RUN

	protected void printResults(ArrayList<Pair<Result, Long>> resultsAndTimes) {
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

	//////////////////// REPORTING AFTER ALL RUNS

	/** 
	 * Invoked after all tests are run. By default, prints all total times with {@link #printTotalTimes()}.
	 */
	protected void afterRunningAllTests() {
		printTotalTimes();
	}

	protected void printTotalTimes() {
		for (int i = 0; i != getAlgorithms().size(); i++) {
			var name = getAlgorithms().get(i).first;
			println("Total time " + name + ": " + getTotalTimeForAlgorithm(i));
		}
	}

	//////////////////// GETTERS

	protected Configuration getConfiguration() {
		return configuration;
	}

	protected List<Pair<String, BinaryFunction<Variable, FactorNetwork, Result>>> getAlgorithms() {
		return getConfiguration().getAlgorithms();
	}

	protected int getNumberOfRuns() {
		return getConfiguration().getNumberOfRuns();
	}
	
	//////////////////// MAKING ONCE AND KEEPING PROBLEM GENERATOR

	private NullaryFunction<Pair<Variable, FactorNetwork>> problemGenerator;
	public NullaryFunction<Pair<Variable, FactorNetwork>> getProblemGenerator() {
		if (problemGenerator == null) {
			problemGenerator = makeProblemGenerator();
		}
		return problemGenerator;
	}

	protected NullaryFunction<Pair<Variable, FactorNetwork>> makeProblemGenerator() {
		return () -> {
			FactorNetwork factorNetwork = makeNextFactorNetwork();
			Variable query = getFirst(factorNetwork.getFactors()).getVariables().get(0);
			return pair(query, factorNetwork);
		};
	}

}