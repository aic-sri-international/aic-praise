package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.configuration;

public class AbstractConfigurationForTestsOnBatchOfFactorNetworks implements ConfigurationForTestsOnBatchOfFactorNetworks {

	private int numberOfTests;

	public AbstractConfigurationForTestsOnBatchOfFactorNetworks(int numberOfTests) {
		this.numberOfTests = numberOfTests;
	}

	@Override
	public int getNumberOfTests() {
		return numberOfTests;
	}

}