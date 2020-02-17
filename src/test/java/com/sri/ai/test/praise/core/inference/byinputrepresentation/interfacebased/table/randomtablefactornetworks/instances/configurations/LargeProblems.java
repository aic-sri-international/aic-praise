package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.instances.configurations;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.configuration.DefaultConfigurationForRandomTableFactorNetworksTest;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public class LargeProblems<Result> extends DefaultConfigurationForRandomTableFactorNetworksTest<Result> {

	public LargeProblems(List<Pair<String,BinaryFunction<Variable, FactorNetwork, Result>>> algorithms) {
		super(
				algorithms,
				/* numberOfRuns = */ 10,
				/* minimumNumberOfVariables = */ 10,
				/* maximumNumberOfVariables = */ 25,
				/* minimumCardinality = */ 2,
				/* maximumCardinality = */ 2,
				/* minimumNumberOfFactors = */ 10,
				/* maximumNumberOfFactors = */ 25,
				/* minimumNumberOfVariablesPerFactor = */ 3,
				/* maximumNumberOfVariablesPerFactor = */ 6,
				/* minimumPotential = */ 1.0,
				/* maximumPotential = */ 4.0,
				/* tableFactorMaker = */ 
					// (v,e) -> new NDArrayTableFactor(v,e);
					(v,e) -> new ArrayTableFactor(v,e),
				/* random = */ new Random(0) // fixed seed since we usually want to compare performance between runs
				);
	}
}
