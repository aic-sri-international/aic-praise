package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.uai.fulltime.base;

import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromuaitofactors.FromUAIModelToTableFactors.makeTableFactorNetwork;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.uai.configuration.ConfigurationForUAITest;

public class FullTimeAlgorithmsOnUAITestRunner
extends AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner<ConfigurationForUAITest<Factor>> {

	public FullTimeAlgorithmsOnUAITestRunner(ConfigurationForUAITest<Factor> configuration) {
		super(configuration);
	}

	@Override
	protected TableFactorNetwork makeNextFactorNetwork() {
		return 
				makeTableFactorNetwork(
						getConfiguration().getUAIFileResourcePath(), 
						getConfiguration().getTableFactorMaker());
	}

}