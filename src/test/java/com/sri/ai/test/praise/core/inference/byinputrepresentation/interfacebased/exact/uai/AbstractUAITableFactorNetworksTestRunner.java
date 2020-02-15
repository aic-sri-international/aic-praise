package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.uai;

import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromuaitofactors.FromUAIModelToTableFactors.fromUAIModelToTableFactorNetwork;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.base.Pair.pair;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.parsing.UAIModelReader;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.AbstractTestsOnBatchOfFactorNetworks;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.uai.configuration.ConfigurationForTestsOnUAITableFactorNetworks;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

public abstract class AbstractUAITableFactorNetworksTestRunner extends AbstractTestsOnBatchOfFactorNetworks {

	@Override
	protected NullaryFunction<Pair<Variable, FactorNetwork>> makeProblemGenerator() {
		return () -> {
			String uaiFileResourcePath = getConfiguration().getUAIFileResourcePath();
			TableFactorNetwork factorNetwork = readTableFactorNetwork(uaiFileResourcePath);
			Variable query = getFirst(factorNetwork.getFactors()).getVariables().get(0);
			return pair(query, factorNetwork);
		};
	}

	private TableFactorNetwork readTableFactorNetwork(String uaiFileResourcePath) throws Error {
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

	@Override
	protected ConfigurationForTestsOnUAITableFactorNetworks getConfiguration() {
		return (ConfigurationForTestsOnUAITableFactorNetworks) super.getConfiguration();
	}

}