package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.randomtablefactornetworks.runner;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.RandomTableFactorNetworkGenerator.generateRandomTableFactorNetwork;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base.AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.randomtablefactornetworks.runner.configuration.ConfigurationForTestsOnRandomTableFactorNetworks;

public class RandomTableFactorNetworksTestRunner<Result> 
extends AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner<Result, ConfigurationForTestsOnRandomTableFactorNetworks<Result>> {

	public RandomTableFactorNetworksTestRunner(ConfigurationForTestsOnRandomTableFactorNetworks<Result> configuration) {
		super(configuration);
	}

	@Override
	protected TableFactorNetwork makeFactorNetwork() {
		return generateRandomTableFactorNetwork(getConfiguration());
	}

}