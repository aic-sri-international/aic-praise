package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

import java.util.ArrayList;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.DefaultConfigurationForRandomTableFactorNetworksGeneration;
import com.sri.ai.util.base.BinaryFunction;

public class DefaultConfigurationForTestsOnRandomTableFactorNetworks
extends DefaultConfigurationForRandomTableFactorNetworksGeneration<TableFactor> 
implements ConfigurationForTestsOnRandomTableFactorNetworks {

	int numberOfTests;

	protected DefaultConfigurationForTestsOnRandomTableFactorNetworks(
			int numberOfTests,
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
		this.numberOfTests = numberOfTests;
	}
	
	@Override
	public int getNumberOfTests() {
		return numberOfTests;
	}

}