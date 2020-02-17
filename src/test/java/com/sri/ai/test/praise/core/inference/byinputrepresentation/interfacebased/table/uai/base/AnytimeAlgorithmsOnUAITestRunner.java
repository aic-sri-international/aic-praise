package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.uai.base;

import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromuaitofactors.FromUAIModelToTableFactors.makeTableFactorNetwork;

import java.util.Iterator;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.uai.configuration.ConfigurationForUAITest;

public class AnytimeAlgorithmsOnUAITestRunner<PartialResult>
extends 
AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner
<PartialResult, 
ConfigurationForUAITest<Iterator<PartialResult>>> {

	public AnytimeAlgorithmsOnUAITestRunner(ConfigurationForUAITest<Iterator<PartialResult>> configuration) {
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