package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

public class LargerProblems extends AbstractConfigurationForTestsOnRandomTableFactorNetworks {

	public LargerProblems() {
		numberOfTests = 10;
		minimumNumberOfVariables = 20;
		maximumNumberOfVariables = 30;
		minimumCardinality = 2;
		maximumCardinality = 2;
		minimumNumberOfFactors = 20;
		maximumNumberOfFactors = 35;
		minimumNumberOfVariablesPerFactor = 4;
		maximumNumberOfVariablesPerFactor = 7;
		minimumPotential = 1.0;
		maximumPotential = 4.0;
	}
}
