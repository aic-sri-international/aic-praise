package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.randomtablefactornetworks.runner.configuration;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.ConfigurationForRandomTableFactorNetworksGeneration;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base.configuration.ConfigurationForTestsOnBatchOfFactorNetworks;

public interface ConfigurationForTestsOnRandomTableFactorNetworks 
extends ConfigurationForRandomTableFactorNetworksGeneration, 
        ConfigurationForTestsOnBatchOfFactorNetworks {
	
}