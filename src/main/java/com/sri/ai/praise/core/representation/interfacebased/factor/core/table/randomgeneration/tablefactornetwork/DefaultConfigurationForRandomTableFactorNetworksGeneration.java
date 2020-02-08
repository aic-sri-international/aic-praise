package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.randomgeneration.tablefactornetwork;

public class DefaultConfigurationForRandomTableFactorNetworksGeneration implements ConfigurationForRandomTableFactorNetworksGeneration {

	protected int minimumNumberOfVariables;
	protected int maximumNumberOfVariables;
	protected int minimumCardinality;
	protected int maximumCardinality;
	protected int minimumNumberOfFactors;
	protected int maximumNumberOfFactors;
	protected int minimumNumberOfVariablesPerFactor;
	protected int maximumNumberOfVariablesPerFactor;
	protected double minimumPotential;
	protected double maximumPotential;

	protected DefaultConfigurationForRandomTableFactorNetworksGeneration() {
	}
	
	public DefaultConfigurationForRandomTableFactorNetworksGeneration(
			int minimumNumberOfVariables, int maximumNumberOfVariables, 
			int minimumCardinality, int maximumCardinality, 
			int minimumNumberOfFactors, int maximumNumberOfFactors,
			int minimumNumberOfVariablesPerFactor, int maximumNumberOfVariablesPerFactor,
			double minimumPotential, double maximumPotential) {
		
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

}