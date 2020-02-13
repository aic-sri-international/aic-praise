package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

import java.util.Random;

public class DebugProblems extends AbstractConfigurationForTestsOnRandomTableFactorNetworks {

	public DebugProblems() {
		random = new Random(); // no seed since we usually want to run different tests each time to ensure we catch errors
		numberOfTests = 1;
		minimumNumberOfVariables = 3;
		maximumNumberOfVariables = 3;
		minimumCardinality = 2;
		maximumCardinality = 2;
		minimumNumberOfFactors = 2;
		maximumNumberOfFactors = 2;
		minimumNumberOfVariablesPerFactor = 2;
		maximumNumberOfVariablesPerFactor = 2;
		minimumPotential = 1.0;
		maximumPotential = 4.0;
	}
}
