package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public interface ConfigurationForBatchOfFactorNetworksTest<Result> {

	int getNumberOfRuns();
	
	double getMaximumComponentwiseError();
	
	List<Pair<String, BinaryFunction<Variable, FactorNetwork, Result>>> getAlgorithms();

}