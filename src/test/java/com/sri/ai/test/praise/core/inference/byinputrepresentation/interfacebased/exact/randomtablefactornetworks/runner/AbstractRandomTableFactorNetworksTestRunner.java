package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.runner;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.RandomTableFactorNetworkGenerator.generateRandomTableFactorNetwork;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.base.Pair.pair;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.AbstractTestsOnBatchOfFactorNetworks;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.runner.configuration.ConfigurationForTestsOnRandomTableFactorNetworks;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

public abstract class AbstractRandomTableFactorNetworksTestRunner extends AbstractTestsOnBatchOfFactorNetworks {

	@Override
	protected NullaryFunction<Pair<Variable, FactorNetwork>> makeProblemGenerator() {
		return () -> {
			TableFactorNetwork factorNetwork = generateRandomTableFactorNetwork(getConfiguration());
			Variable query = getFirst(factorNetwork.getFactors()).getVariables().get(0);
			return pair(query, factorNetwork);
		};
	}

	@Override
	protected ConfigurationForTestsOnRandomTableFactorNetworks getConfiguration() {
		return (ConfigurationForTestsOnRandomTableFactorNetworks) super.getConfiguration();
	}

}