package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.pair;
import static com.sri.ai.util.Util.repeat;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base.configuration.ConfigurationForTestsOnBatchOfFactorNetworks;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

/** 
 * Abstract class for running a list of algorithms on a batch of problems.
 * The actual generation of problem is done by extension classes implementing the abstract methods.
 * 
 * @author braz
 *
 */
public abstract class AbstractBatchOfFactorNetworksTestRunner<Result, Configuration extends ConfigurationForTestsOnBatchOfFactorNetworks<Result>> {

	//////////////////// ABSTRACT METHODS
	
	abstract protected TableFactorNetwork makeFactorNetwork();

	protected abstract void runTest(int testNumber, NullaryFunction<Pair<Variable, FactorNetwork>> problemGenerator);

	protected abstract void afterRunningAllTests();

	//////////////////// DATA MEMBER
	
	private Configuration configuration;

	//////////////////// CONSTRUCTOR
	
	public AbstractBatchOfFactorNetworksTestRunner(Configuration configuration) {
		this.configuration = configuration;
	}
	
	//////////////////// IMPLEMENTATION
	
	public void run() {
		repeat(getNumberOfRuns(), testNumber -> runTest(testNumber, getProblemGenerator()));
		afterRunningAllTests();
	}

	protected Configuration getConfiguration() {
		return configuration;
	}

	protected List<Pair<String, BinaryFunction<Variable, FactorNetwork, Result>>> getAlgorithms() {
		return getConfiguration().getAlgorithms();
	}

	protected int getNumberOfRuns() {
		return getConfiguration().getNumberOfRuns();
	}

	private NullaryFunction<Pair<Variable, FactorNetwork>> problemGenerator;
	public NullaryFunction<Pair<Variable, FactorNetwork>> getProblemGenerator() {
		if (problemGenerator == null) {
			problemGenerator = makeProblemGenerator();
		}
		return problemGenerator;
	}

	protected NullaryFunction<Pair<Variable, FactorNetwork>> makeProblemGenerator() {
		return () -> {
			TableFactorNetwork factorNetwork = makeFactorNetwork();
			Variable query = getFirst(factorNetwork.getFactors()).getVariables().get(0);
			return pair(query, factorNetwork);
		};
	}

}