package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased;

public class AbstractConfigurationForTestsOnRandomTableFactorNetworks implements ConfigurationForTestsOnRandomTableFactorNetworks {

	int numberOfTests;
	int minimumNumberOfVariables;
	int maximumNumberOfVariables;
	int minimumCardinality;
	int maximumCardinality;
	int minimumNumberOfFactors;
	int maximumNumberOfFactors;
	int minimumNumberOfVariablesPerFactor;
	int maximumNumberOfVariablesPerFactor;
	double minimumPotential;
	double maximumPotential;

	public AbstractConfigurationForTestsOnRandomTableFactorNetworks() {
		super();
	}

	@Override
	public int getNumberOfTests() {
		return numberOfTests;
	}

	@Override
	public int getMinimumNumberOfVariables() {
		return minimumNumberOfVariables;
	}

	@Override
	public int getMaximumNumberOfVariables() {
		return maximumNumberOfVariables;
	}

	@Override
	public int getMinimumCardinality() {
		return minimumCardinality;
	}

	@Override
	public int getMaximumCardinality() {
		return maximumCardinality;
	}

	@Override
	public int getMinimumNumberOfFactors() {
		return minimumNumberOfFactors;
	}

	@Override
	public int getMaximumNumberOfFactors() {
		return maximumNumberOfFactors;
	}

	@Override
	public int getMinimumNumberOfVariablesPerFactor() {
		return minimumNumberOfVariablesPerFactor;
	}

	@Override
	public int getMaximumNumberOfVariablesPerFactor() {
		return maximumNumberOfVariablesPerFactor;
	}

	@Override
	public double getMinimumPotential() {
		return minimumPotential;
	}

	@Override
	public double getMaximumPotential() {
		return maximumPotential;
	}

}