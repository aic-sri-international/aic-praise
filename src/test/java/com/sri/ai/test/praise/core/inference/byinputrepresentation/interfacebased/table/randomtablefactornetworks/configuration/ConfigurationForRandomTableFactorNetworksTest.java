package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.configuration;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.ConfigurationForRandomTableFactorNetworksGeneration;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration.ConfigurationForBatchOfFactorNetworksTest;

public interface ConfigurationForRandomTableFactorNetworksTest<Result> 
extends ConfigurationForRandomTableFactorNetworksGeneration, 
        ConfigurationForBatchOfFactorNetworksTest<Result> {
	
}