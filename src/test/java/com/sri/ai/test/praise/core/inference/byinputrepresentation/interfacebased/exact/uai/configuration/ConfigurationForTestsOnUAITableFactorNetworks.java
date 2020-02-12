package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.uai.configuration;

import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.configuration.AbstractConfigurationForTestsOnBatchOfFactorNetworks;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.configuration.ConfigurationForTestsOnBatchOfFactorNetworks;

public class ConfigurationForTestsOnUAITableFactorNetworks extends AbstractConfigurationForTestsOnBatchOfFactorNetworks implements ConfigurationForTestsOnBatchOfFactorNetworks {

	String uaiFileResourcePath;
	
	public String getUAIFileResourcePath() {
		return uaiFileResourcePath;
	}

}
