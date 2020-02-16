package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.fulltime.base;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.RandomTableFactorNetworkGenerator.generateRandomTableFactorNetwork;

import java.util.Iterator;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.configuration.ConfigurationForRandomTableFactorNetworksTest;

public class AnytimeAlgorithmsOnRandomTableFactorNetworksTestRunner<PartialResult> 
extends 
AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner
<PartialResult, 
ConfigurationForRandomTableFactorNetworksTest<Iterator<PartialResult>>> {

	public AnytimeAlgorithmsOnRandomTableFactorNetworksTestRunner(
			ConfigurationForRandomTableFactorNetworksTest<Iterator<PartialResult>> configuration) {
		
		super(configuration);
	}

	@Override
	protected TableFactorNetwork makeNextFactorNetwork() {
		return generateRandomTableFactorNetwork(getConfiguration());
	}

}