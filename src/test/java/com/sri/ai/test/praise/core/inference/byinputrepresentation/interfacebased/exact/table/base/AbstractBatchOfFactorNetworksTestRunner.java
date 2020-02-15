package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base;

import static com.sri.ai.util.Util.repeat;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base.configuration.ConfigurationForTestsOnBatchOfFactorNetworks;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

/** 
 * Abstract class for running a list of algorithms on a batch of problems.
 * The actual generation of problem is done by extension classes implementing the abstract methods.
 * @author braz
 *
 */
public abstract class AbstractBatchOfFactorNetworksTestRunner<Result> {

	//////////////////// ABSTRACT METHODS
	
	protected abstract NullaryFunction<Pair<Variable, FactorNetwork>> makeProblemGenerator();

	protected abstract void runTest(int testNumber, NullaryFunction<Pair<Variable, FactorNetwork>> problemGenerator);

	protected abstract void afterRunningAllTests();

	//////////////////// DATA MEMBER
	
	private ConfigurationForTestsOnBatchOfFactorNetworks<Result> configuration;

	//////////////////// CONSTRUCTOR
	
	public AbstractBatchOfFactorNetworksTestRunner(ConfigurationForTestsOnBatchOfFactorNetworks<Result> configuration) {
		this.configuration = configuration;
	}
	
	//////////////////// IMPLEMENTATION
	
	protected ConfigurationForTestsOnBatchOfFactorNetworks<Result> getConfiguration() {
		return configuration;
	}

	protected List<Pair<String, BinaryFunction<Variable, FactorNetwork, Result>>> getAlgorithms() {
		return getConfiguration().getAlgorithms();
	}

	protected int getNumberOfRuns() {
		return getConfiguration().getNumberOfRuns();
	}

	public void run() {
		repeat(getNumberOfRuns(), testNumber -> runTest(testNumber, getProblemGenerator()));
		afterRunningAllTests();
	}

	private NullaryFunction<Pair<Variable, FactorNetwork>> problemGenerator;
	public NullaryFunction<Pair<Variable, FactorNetwork>> getProblemGenerator() {
		if (problemGenerator == null) {
			problemGenerator = makeProblemGenerator();
		}
		return problemGenerator;
	}

}