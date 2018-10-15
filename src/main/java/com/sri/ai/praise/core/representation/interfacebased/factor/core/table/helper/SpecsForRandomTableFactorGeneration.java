package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper;

import java.util.ArrayList;

public class SpecsForRandomTableFactorGeneration {

	public ArrayList<Integer> cardinalities;
	public double minimumPotential;
	public double maximumPotential;
	public boolean integerIncrements;
	
	public SpecsForRandomTableFactorGeneration(ArrayList<Integer> cardinalities, 
							double minimumPotential, double maximumPotential, boolean integerIncrements)
	{
		this.cardinalities = cardinalities;
		this.minimumPotential = minimumPotential;
		this.maximumPotential = maximumPotential;
		this.integerIncrements = integerIncrements;
	}
	
	public SpecsForRandomTableFactorGeneration(SpecsForRandomTableFactorGeneration toCopy)
	{
		cardinalities = new ArrayList<>(toCopy.cardinalities.size());
		for(Integer cardinality : toCopy.cardinalities)
		{
			this.cardinalities.add(new Integer(cardinality));
		}
		this.minimumPotential = toCopy.minimumPotential;
		this.maximumPotential = toCopy.maximumPotential;
		this.integerIncrements = toCopy.integerIncrements;
	}

}
