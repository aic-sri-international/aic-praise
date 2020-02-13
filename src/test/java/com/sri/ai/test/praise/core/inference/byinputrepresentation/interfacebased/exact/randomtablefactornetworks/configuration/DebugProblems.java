package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

import java.util.Random;

public class DebugProblems extends AbstractConfigurationForTestsOnRandomTableFactorNetworks {

	public DebugProblems() {
		random = new Random(); // no seed since we usually want to run different tests each time to ensure we catch errors
		numberOfTests = 1;
		minimumNumberOfVariables = 5;
		maximumNumberOfVariables = 5;
		minimumCardinality = 3;
		maximumCardinality = 3;
		minimumNumberOfFactors = 3;
		maximumNumberOfFactors = 3;
		minimumNumberOfVariablesPerFactor = 3;
		maximumNumberOfVariablesPerFactor = 3;
		minimumPotential = 1.0;
		maximumPotential = 4.0;
	}
}
