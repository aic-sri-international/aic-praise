package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork;

import java.util.ArrayList;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.util.base.BinaryFunction;

public interface ConfigurationForRandomTableFactorNetworksGeneration {

	int getMinimumNumberOfVariables();

	int getMaximumNumberOfVariables();

	int getMinimumCardinality();

	int getMaximumCardinality();

	int getMinimumNumberOfFactors();

	int getMaximumNumberOfFactors();

	int getMinimumNumberOfVariablesPerFactor();

	int getMaximumNumberOfVariablesPerFactor();

	double getMinimumPotential();

	double getMaximumPotential();
	
	BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> getTableFactorMaker();
	
	Random getRandom();

}