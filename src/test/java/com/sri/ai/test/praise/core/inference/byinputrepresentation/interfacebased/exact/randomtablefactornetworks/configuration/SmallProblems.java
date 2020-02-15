package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;

public class SmallProblems extends DefaultConfigurationForTestsOnRandomTableFactorNetworks {

	public SmallProblems() {
		super(
				/* numberOfTests = */ 100,
				/* minimumNumberOfVariables = */ 2,
				/* maximumNumberOfVariables = */ 3,
				/* minimumCardinality = */ 2,
				/* maximumCardinality = */ 2,
				/* minimumNumberOfFactors = */ 1,
				/* maximumNumberOfFactors = */ 3,
				/* minimumNumberOfVariablesPerFactor = */ 1,
				/* maximumNumberOfVariablesPerFactor = */ 2,
				/* minimumPotential = */ 1.0,
				/* maximumPotential = */ 4.0,
				/* tableFactorMaker = */ 
					// (v,e) -> new NDArrayTableFactor(v,e);
					(v,e) -> new ArrayTableFactor(v,e),
				/* random = */ new Random() // no seed since we usually want to run different tests each time to ensure we catch errors
				);
	}
}
