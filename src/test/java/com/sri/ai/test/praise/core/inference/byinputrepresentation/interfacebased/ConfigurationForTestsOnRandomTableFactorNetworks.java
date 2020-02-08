package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased;

public interface ConfigurationForTestsOnRandomTableFactorNetworks {

	int getNumberOfTests();

	int getMinimumNumberOfVariables();

	int getMaximumNumberOfVariables();

	int getMinimumCardinality();

	int getMaximumCardinality();

	int getMinimumNumberOfFactors();

	int getMaximumNumberOfFactors();

	int getMinimumNumberOfVariablesPerFactor();

	int getMaximumNumberOfVariablesPerFactor();

	double getMinimumPotential();

	double getMaximumPotential();

}