package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.base.configuration;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public interface ConfigurationForTestsOnBatchOfFactorNetworks {

	int getNumberOfRuns();
	
	List<Pair<String,BinaryFunction<Variable,FactorNetwork,Factor>>> getAlgorithms();

}