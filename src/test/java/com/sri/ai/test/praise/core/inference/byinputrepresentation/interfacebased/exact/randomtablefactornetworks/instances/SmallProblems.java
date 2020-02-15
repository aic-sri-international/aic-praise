package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.instances;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.runner.configuration.DefaultConfigurationForTestsOnRandomTableFactorNetworks;

public class SmallProblems extends DefaultConfigurationForTestsOnRandomTableFactorNetworks {

	public SmallProblems() {
		super(
				/* numberOfTests = */ 1000,
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
