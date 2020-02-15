package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;

public class LargeProblems extends AbstractConfigurationForTestsOnRandomTableFactorNetworks {

	public LargeProblems() {
		super(
				/* numberOfTests = */ 10,
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
