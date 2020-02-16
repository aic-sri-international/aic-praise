package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.fulltime.instances.configurations;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.configuration.DefaultConfigurationForRandomTableFactorNetworksTest;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public class LargerProblems<Result> extends DefaultConfigurationForRandomTableFactorNetworksTest<Result> {

	public LargerProblems(List<Pair<String,BinaryFunction<Variable, FactorNetwork, Result>>> algorithms) {
		super(
				algorithms,
				/* numberOfRuns = */ 1,
				/* minimumNumberOfVariables = */ 3,
				/* maximumNumberOfVariables = */ 3,
				/* minimumCardinality = */ 2,
				/* maximumCardinality = */ 2,
				/* minimumNumberOfFactors = */ 2,
				/* maximumNumberOfFactors = */ 2,
				/* minimumNumberOfVariablesPerFactor = */ 2,
				/* maximumNumberOfVariablesPerFactor = */ 2,
				/* minimumPotential = */ 1.0,
				/* maximumPotential = */ 4.0,
				/* tableFactorMaker = */ 
					// (v,e) -> new NDArrayTableFactor(v,e);
					(v,e) -> new ArrayTableFactor(v,e),
				/* random = */ new Random(0) // fixed seed since we usually want to compare performance between runs
				);
	}
}
