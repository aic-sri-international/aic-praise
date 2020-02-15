package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork;

import java.util.ArrayList;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.util.base.BinaryFunction;

public class DefaultConfigurationForRandomTableFactorNetworksGeneration
implements          ConfigurationForRandomTableFactorNetworksGeneration {

	private int minimumNumberOfVariables;
	private int maximumNumberOfVariables;
	private int minimumCardinality;
	private int maximumCardinality;
	private int minimumNumberOfFactors;
	private int maximumNumberOfFactors;
	private int minimumNumberOfVariablesPerFactor;
	private int maximumNumberOfVariablesPerFactor;
	private double minimumPotential;
	private double maximumPotential;
	private BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker;
	private Random random;
	
	public DefaultConfigurationForRandomTableFactorNetworksGeneration(
			int minimumNumberOfVariables, int maximumNumberOfVariables, 
			int minimumCardinality, int maximumCardinality, 
			int minimumNumberOfFactors, int maximumNumberOfFactors,
			int minimumNumberOfVariablesPerFactor, int maximumNumberOfVariablesPerFactor,
			double minimumPotential, double maximumPotential,
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker,
			Random random) {
		
		this.minimumNumberOfVariables = minimumNumberOfVariables;
		this.maximumNumberOfVariables = maximumNumberOfVariables;
		this.minimumCardinality = minimumCardinality;
		this.maximumCardinality = maximumCardinality;
		this.minimumNumberOfFactors = minimumNumberOfFactors;
		this.maximumNumberOfFactors = maximumNumberOfFactors;
		this.minimumNumberOfVariablesPerFactor = minimumNumberOfVariablesPerFactor;
		this.maximumNumberOfVariablesPerFactor = maximumNumberOfVariablesPerFactor;
		this.minimumPotential = minimumPotential;
		this.maximumPotential = maximumPotential;
		this.tableFactorMaker = tableFactorMaker;
		this.random = random;
	}

	@Override
	public int getMinimumNumberOfVariables() {
		return minimumNumberOfVariables;
	}

	@Override
	public int getMaximumNumberOfVariables() {
		return maximumNumberOfVariables;
	}

	@Override
	public int getMinimumCardinality() {
		return minimumCardinality;
	}

	@Override
	public int getMaximumCardinality() {
		return maximumCardinality;
	}

	@Override
	public int getMinimumNumberOfFactors() {
		return minimumNumberOfFactors;
	}

	@Override
	public int getMaximumNumberOfFactors() {
		return maximumNumberOfFactors;
	}

	@Override
	public int getMinimumNumberOfVariablesPerFactor() {
		return minimumNumberOfVariablesPerFactor;
	}

	@Override
	public int getMaximumNumberOfVariablesPerFactor() {
		return maximumNumberOfVariablesPerFactor;
	}

	@Override
	public double getMinimumPotential() {
		return minimumPotential;
	}

	@Override
	public double getMaximumPotential() {
		return maximumPotential;
	}

	@Override
	public BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> getTableFactorMaker() {
		return tableFactorMaker;
	}

	@Override
	public Random getRandom() {
		return random;
	}

}