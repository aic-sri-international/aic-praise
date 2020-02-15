package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.uai.runner;

import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromuaitofactors.FromUAIModelToTableFactors.fromUAIModelToTableFactorNetwork;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.parsing.UAIModelReader;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base.AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.uai.instances.TestsOnUAITableFactorNetworks;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.uai.runner.configuration.ConfigurationForTestOnUAIFile;

public class UAITableFactorNetworksTestRunner<Result>
extends AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner<Result, ConfigurationForTestOnUAIFile<Result>> {

	public UAITableFactorNetworksTestRunner(ConfigurationForTestOnUAIFile<Result> configuration) {
		super(configuration);
	}

	@Override
	protected TableFactorNetwork makeFactorNetwork() {
		String uaiFileResourcePath = getConfiguration().getUAIFileResourcePath();
		TableFactorNetwork factorNetwork; 
		try {
			InputStream resourceStream = TestsOnUAITableFactorNetworks.class.getResourceAsStream(uaiFileResourcePath);
			InputStreamReader resourceReader = new InputStreamReader(resourceStream);
			UAIModel uaiModel = UAIModelReader.read(resourceReader);
			factorNetwork = fromUAIModelToTableFactorNetwork(uaiModel, getConfiguration().getTableFactorMaker());
		} catch (IOException e) {
			throw new Error("Could not read UAI file " + uaiFileResourcePath);
		}
		return factorNetwork;
	}

}