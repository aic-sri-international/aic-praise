package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.uai.base;

import static com.sri.ai.praise.core.representation.translation.fromuaitofactors.FromUAIModelToTableFactors.makeTableFactorNetwork;

import java.util.Iterator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.uai.configuration.ConfigurationForUAITest;
import com.sri.ai.util.computation.anytime.api.Approximation;

public class AnytimeAlgorithmsOnUAITestRunner
extends 
AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner<ConfigurationForUAITest<Iterator<Approximation<Factor>>>> {

	public AnytimeAlgorithmsOnUAITestRunner(ConfigurationForUAITest<Iterator<Approximation<Factor>>> configuration) {
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