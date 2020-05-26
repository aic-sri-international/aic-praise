package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public class DefaultConfigurationForBatchOfFactorNetworksTest<Result> implements ConfigurationForBatchOfFactorNetworksTest<Result> {

	private List<Pair<String,BinaryFunction<Variable, FactorNetwork, Result>>> algorithms;
	
	private int numberOfRuns;
	
	private double maximumComponentwiseError;

	public DefaultConfigurationForBatchOfFactorNetworksTest(
			List<Pair<String,BinaryFunction<Variable, FactorNetwork, Result>>> algorithms, 
			int numberOfRuns,
			double maximumComponentwiseError) {
		
		this.algorithms = algorithms;
		this.numberOfRuns = numberOfRuns;
		this.maximumComponentwiseError = maximumComponentwiseError;
	}

	@Override
	public List<Pair<String, BinaryFunction<Variable, FactorNetwork, Result>>> getAlgorithms() {
		return algorithms;
	}

	@Override
	public int getNumberOfRuns() {
		return numberOfRuns;
	}
	
	@Override
	public double getMaximumComponentwiseError() {
		return maximumComponentwiseError;
	}

}