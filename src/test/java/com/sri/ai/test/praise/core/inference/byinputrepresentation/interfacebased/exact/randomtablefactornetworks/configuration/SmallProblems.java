package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

public class SmallProblems extends AbstractConfigurationForTestsOnRandomTableFactorNetworks {

	public SmallProblems() {
		numberOfTests = 100;
		minimumNumberOfVariables = 2;
		maximumNumberOfVariables = 3;
		minimumCardinality = 2;
		maximumCardinality = 2;
		minimumNumberOfFactors = 1;
		maximumNumberOfFactors = 3;
		minimumNumberOfVariablesPerFactor = 1;
		maximumNumberOfVariablesPerFactor = 2;
		minimumPotential = 1.0;
		maximumPotential = 4.0;
	}
}
