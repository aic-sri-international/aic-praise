package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.base;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.RandomTableFactorNetworkGenerator.generateRandomTableFactorNetwork;

import java.util.Iterator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.configuration.ConfigurationForRandomTableFactorNetworksTest;
import com.sri.ai.util.computation.anytime.api.Approximation;

public class AnytimeAlgorithmsOnRandomTableFactorNetworksTestRunner 
extends 
AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner
<ConfigurationForRandomTableFactorNetworksTest<Iterator<Approximation<Factor>>>> {

	public AnytimeAlgorithmsOnRandomTableFactorNetworksTestRunner(
			ConfigurationForRandomTableFactorNetworksTest<Iterator<Approximation<Factor>>> configuration) {
		
		super(configuration);
	}

	@Override
	protected TableFactorNetwork makeNextFactorNetwork() {
		return generateRandomTableFactorNetwork(getConfiguration());
	}

}