package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.instances.configurations;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.configuration.DefaultConfigurationForRandomTableFactorNetworksTest;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public class SmallProblems<Result> extends DefaultConfigurationForRandomTableFactorNetworksTest<Result> {

	public SmallProblems(List<Pair<String, BinaryFunction<Variable, FactorNetwork, Result>>> algorithms) {
		super(
				algorithms,
				/* numberOfRuns = */ 1000,
				/* maximumComponentwiseError = */ Defaults.MAXIMUM_COMPONENTWISE_ERROR,
				/* minimumNumberOfVariables = */ 3,
				/* maximumNumberOfVariables = */ 5,
				/* minimumCardinality = */ 2,
				/* maximumCardinality = */ 3,
				/* minimumNumberOfFactors = */ 1,
				/* maximumNumberOfFactors = */ 4,
				/* minimumNumberOfVariablesPerFactor = */ 1,
				/* maximumNumberOfVariablesPerFactor = */ 3,
				/* minimumPotential = */ 1.0,
				/* maximumPotential = */ 4.0,
				/* tableFactorMaker = */ 
					// (v,e) -> new NDArrayTableFactor(v,e);
					(v,e) -> new ArrayTableFactor(v,e),
				/* random = */ new Random() // no seed since we usually want to run different tests each time to ensure we catch errors
				);
	}
}
