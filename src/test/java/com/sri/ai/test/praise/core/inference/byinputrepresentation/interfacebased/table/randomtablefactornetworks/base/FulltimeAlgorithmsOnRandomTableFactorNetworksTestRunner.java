package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.base;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.RandomTableFactorNetworkGenerator.generateRandomTableFactorNetwork;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.AbstractFulltimeAlgorithmOnBatchOfFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.configuration.ConfigurationForRandomTableFactorNetworksTest;

public class FulltimeAlgorithmsOnRandomTableFactorNetworksTestRunner 
extends AbstractFulltimeAlgorithmOnBatchOfFactorNetworksTestRunner<ConfigurationForRandomTableFactorNetworksTest<Factor>> {

	public FulltimeAlgorithmsOnRandomTableFactorNetworksTestRunner(ConfigurationForRandomTableFactorNetworksTest<Factor> configuration) {
		super(configuration);
	}

	@Override
	protected TableFactorNetwork makeNextFactorNetwork() {
		return generateRandomTableFactorNetwork(getConfiguration());
	}

}