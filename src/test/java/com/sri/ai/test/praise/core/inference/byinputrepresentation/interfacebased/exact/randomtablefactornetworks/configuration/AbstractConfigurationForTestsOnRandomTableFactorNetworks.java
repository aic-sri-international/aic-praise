package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.randomgeneration.tablefactornetwork.DefaultConfigurationForRandomTableFactorNetworksGeneration;

public class AbstractConfigurationForTestsOnRandomTableFactorNetworks extends DefaultConfigurationForRandomTableFactorNetworksGeneration implements ConfigurationForTestsOnRandomTableFactorNetworks {

	int numberOfTests;

	@Override
	public int getNumberOfTests() {
		return numberOfTests;
	}

}