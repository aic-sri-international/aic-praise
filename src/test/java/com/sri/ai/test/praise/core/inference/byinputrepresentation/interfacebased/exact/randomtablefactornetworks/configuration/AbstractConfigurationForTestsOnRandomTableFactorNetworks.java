package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.DefaultConfigurationForRandomTableFactorNetworksGeneration;

public class AbstractConfigurationForTestsOnRandomTableFactorNetworks extends DefaultConfigurationForRandomTableFactorNetworksGeneration implements ConfigurationForTestsOnRandomTableFactorNetworks {

	Random random;
	int numberOfTests;

	@Override
	public int getNumberOfTests() {
		return numberOfTests;
	}

	@Override
	public Random getRandom() {
		return random;
	}

}