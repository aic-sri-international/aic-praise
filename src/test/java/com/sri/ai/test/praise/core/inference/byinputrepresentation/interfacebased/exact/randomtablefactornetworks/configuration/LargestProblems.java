package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

import java.util.Random;

public class LargestProblems extends AbstractConfigurationForTestsOnRandomTableFactorNetworks {

	public LargestProblems() {
		random = new Random(0); // fixed seed since we usually want to compare performance between runs
		numberOfTests = 10;
		minimumNumberOfVariables = 20;
		maximumNumberOfVariables = 30;
		minimumCardinality = 2;
		maximumCardinality = 2;
		minimumNumberOfFactors = 20;
		maximumNumberOfFactors = 35;
		minimumNumberOfVariablesPerFactor = 6;
		maximumNumberOfVariablesPerFactor = 9;
		minimumPotential = 1.0;
		maximumPotential = 4.0;
	}
}
