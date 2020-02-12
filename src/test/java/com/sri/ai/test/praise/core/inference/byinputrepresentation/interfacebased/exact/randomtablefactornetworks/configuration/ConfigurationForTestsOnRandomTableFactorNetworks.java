package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.ConfigurationForRandomTableFactorNetworksGeneration;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.configuration.ConfigurationForTestsOnBatchOfFactorNetworks;

public interface ConfigurationForTestsOnRandomTableFactorNetworks extends ConfigurationForRandomTableFactorNetworksGeneration, ConfigurationForTestsOnBatchOfFactorNetworks {
	
	Random getRandom();

}