package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.randomtablefactornetworks.instances;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.randomtablefactornetworks.runner.configuration.DefaultConfigurationForTestsOnRandomTableFactorNetworks;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public class LargestProblems<Result> extends DefaultConfigurationForTestsOnRandomTableFactorNetworks<Result> {

	public LargestProblems(List<Pair<String,BinaryFunction<Variable,FactorNetwork,Result>>> algorithms) {
		super(
				algorithms,
				/* numberOfRuns = */ 10,
				/* minimumNumberOfVariables = */ 20,
				/* maximumNumberOfVariables = */ 30,
				/* minimumCardinality = */ 2,
				/* maximumCardinality = */ 2,
				/* minimumNumberOfFactors = */ 20,
				/* maximumNumberOfFactors = */ 35,
				/* minimumNumberOfVariablesPerFactor = */ 6,
				/* maximumNumberOfVariablesPerFactor = */ 9,
				/* minimumPotential = */ 1.0,
				/* maximumPotential = */ 4.0,
				/* tableFactorMaker = */ 
					// (v,e) -> new NDArrayTableFactor(v,e);
					(v,e) -> new ArrayTableFactor(v,e),
				/* random = */ new Random(0) // fixed seed since we usually want to compare performance between runs
				);
	}
}
