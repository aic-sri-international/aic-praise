package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased;

public class LargeProblems extends AbstractConfigurationForTestsOnRandomTableFactorNetworks {

	public LargeProblems() {
		numberOfTests = 10;
		minimumNumberOfVariables = 10;
		maximumNumberOfVariables = 25;
		minimumCardinality = 2;
		maximumCardinality = 2;
		minimumNumberOfFactors = 10;
		maximumNumberOfFactors = 25;
		minimumNumberOfVariablesPerFactor = 3;
		maximumNumberOfVariablesPerFactor = 6;
		minimumPotential = 1.0;
		maximumPotential = 4.0;
	}
}
