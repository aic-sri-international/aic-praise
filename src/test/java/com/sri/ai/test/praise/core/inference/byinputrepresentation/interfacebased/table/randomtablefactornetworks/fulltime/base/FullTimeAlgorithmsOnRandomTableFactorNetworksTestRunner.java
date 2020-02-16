package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.fulltime.base;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.RandomTableFactorNetworkGenerator.generateRandomTableFactorNetwork;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.configuration.ConfigurationForRandomTableFactorNetworksTest;

public class FullTimeAlgorithmsOnRandomTableFactorNetworksTestRunner 
extends AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner<ConfigurationForRandomTableFactorNetworksTest<Factor>> {

	public FullTimeAlgorithmsOnRandomTableFactorNetworksTestRunner(ConfigurationForRandomTableFactorNetworksTest<Factor> configuration) {
		super(configuration);
	}

	@Override
	protected TableFactorNetwork makeNextFactorNetwork() {
		return generateRandomTableFactorNetwork(getConfiguration());
	}

}