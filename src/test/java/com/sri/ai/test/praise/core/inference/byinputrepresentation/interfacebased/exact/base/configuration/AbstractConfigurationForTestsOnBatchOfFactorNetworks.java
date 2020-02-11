package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.configuration;

public class AbstractConfigurationForTestsOnBatchOfFactorNetworks implements ConfigurationForTestsOnBatchOfFactorNetworks {

	protected int numberOfTests;

	@Override
	public int getNumberOfTests() {
		return numberOfTests;
	}

}