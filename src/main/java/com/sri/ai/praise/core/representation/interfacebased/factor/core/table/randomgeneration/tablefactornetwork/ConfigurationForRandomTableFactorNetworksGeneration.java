package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.randomgeneration.tablefactornetwork;

public interface ConfigurationForRandomTableFactorNetworksGeneration {

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