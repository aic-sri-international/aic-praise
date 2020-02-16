package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.configuration;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.DefaultConfigurationForRandomTableFactorNetworksGeneration;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration.ConfigurationForBatchOfFactorNetworksTest;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration.DefaultConfigurationForBatchOfFactorNetworksTest;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public class DefaultConfigurationForRandomTableFactorNetworksTest<Result>
extends DefaultConfigurationForRandomTableFactorNetworksGeneration 
implements ConfigurationForRandomTableFactorNetworksTest<Result> {

	private ConfigurationForBatchOfFactorNetworksTest<Result> configurationForTestsOnBatchOfFactorNetworks; // "multiple inheritance" by containment

	protected DefaultConfigurationForRandomTableFactorNetworksTest(
			List<Pair<String,BinaryFunction<Variable, FactorNetwork, Result>>> algorithms,
			int numberOfRuns,
			int minimumNumberOfVariables, int maximumNumberOfVariables, 
			int minimumCardinality, int maximumCardinality, 
			int minimumNumberOfFactors, int maximumNumberOfFactors,
			int minimumNumberOfVariablesPerFactor, int maximumNumberOfVariablesPerFactor,
			double minimumPotential, double maximumPotential,
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker,
			Random random) {
		
		super(
				minimumNumberOfVariables, maximumNumberOfVariables, 
				minimumCardinality, maximumCardinality, 
				minimumNumberOfFactors, maximumNumberOfFactors,
				minimumNumberOfVariablesPerFactor, maximumNumberOfVariablesPerFactor,
				minimumPotential, maximumPotential,
				tableFactorMaker,
				random);
		this.configurationForTestsOnBatchOfFactorNetworks = new DefaultConfigurationForBatchOfFactorNetworksTest<Result>(algorithms, numberOfRuns);
	}

	@Override
	public List<Pair<String, BinaryFunction<Variable, FactorNetwork, Result>>> getAlgorithms() {
		return configurationForTestsOnBatchOfFactorNetworks.getAlgorithms();
	}
	
	@Override
	public int getNumberOfRuns() {
		return configurationForTestsOnBatchOfFactorNetworks.getNumberOfRuns();
	}

}