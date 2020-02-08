package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.randomgeneration.tablefactor;

import java.util.ArrayList;

/**
 * The parameters for generating random table factors:
 * <ul>
 * <li> the cardinalities of the variables
 * <li> the minimum and maximum potential to be randomly selected
 * <li> whether to select potentials as integer increments from the minimal potential
 * </ul>
 * 
 * @author braz
 *
 */
public class ConfigurationForRandomTableFactorGeneration {

	public ArrayList<Integer> cardinalities;
	public double minimumPotential;
	public double maximumPotential;
	public boolean integerIncrements;

	public ConfigurationForRandomTableFactorGeneration(
			ArrayList<Integer> cardinalities, 
			double minimumPotential, 
			double maximumPotential, 
			boolean integerIncrements) {
		
		this.cardinalities = cardinalities;
		this.minimumPotential = minimumPotential;
		this.maximumPotential = maximumPotential;
		this.integerIncrements = integerIncrements;
	}

	public ConfigurationForRandomTableFactorGeneration(ConfigurationForRandomTableFactorGeneration toCopy) {
		cardinalities = new ArrayList<>(toCopy.cardinalities.size());
		for (Integer cardinality : toCopy.cardinalities) {
			this.cardinalities.add(Integer.valueOf(cardinality));
		}
		this.minimumPotential = toCopy.minimumPotential;
		this.maximumPotential = toCopy.maximumPotential;
		this.integerIncrements = toCopy.integerIncrements;
	}

}
