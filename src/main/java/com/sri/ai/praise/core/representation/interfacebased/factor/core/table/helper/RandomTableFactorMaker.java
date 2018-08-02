package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper;

import static com.sri.ai.util.Util.mapIntegersIntoArrayList;
import static com.sri.ai.util.Util.product;

import java.util.ArrayList;
import java.util.Random;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;

public class RandomTableFactorMaker {
	
	public static class TableFactorSpecs{
		public ArrayList<Integer> cardinalities;
		public double minimumPotential;
		public double maximumPotential;
		
		public TableFactorSpecs(ArrayList<Integer> cardinalities, double minimumPotential, double maximumPotential)
		{
			this.cardinalities = cardinalities;
			this.minimumPotential = minimumPotential;
			this.maximumPotential = maximumPotential;
		}
		
		public TableFactorSpecs(TableFactorSpecs toCopy)
		{
			cardinalities = new ArrayList<>(toCopy.cardinalities.size());
			for(Integer cardinality : toCopy.cardinalities)
			{
				this.cardinalities.add(new Integer(cardinality));
			}
			this.minimumPotential = toCopy.minimumPotential;
			this.maximumPotential = toCopy.maximumPotential;
		}
	}	

	public static TableFactor makeRandomTableFactor(
			ArrayList<Integer> cardinalities, double minimumPotential, double maximumPotential, Function<Integer, String> fromVariableIndexToName, Random random) {
		
		ArrayList<TableVariable> variables = makeVariables(cardinalities, fromVariableIndexToName);
		ArrayList<Double> entries = makeUniformlyDistributedRandomEntries(cardinalities, minimumPotential, maximumPotential, random);
		TableFactor tableFactor = new TableFactor(variables, entries);
		return tableFactor;
	}
	
	public static TableFactor makeRandomTableFactor(TableFactorSpecs specs, Function<Integer, String> fromVariableIndexToName, Random random)
	{
		return makeRandomTableFactor(specs.cardinalities, specs.minimumPotential, specs.maximumPotential, fromVariableIndexToName, random);
	}


	private static ArrayList<TableVariable> makeVariables(ArrayList<Integer> cardinalities, Function<Integer, String> fromVariableIndexToName) {
		int numberOfVariables = cardinalities.size();
		ArrayList<TableVariable> result = mapIntegersIntoArrayList(numberOfVariables, fromIndexToTableVariable(cardinalities, fromVariableIndexToName));
		return result;
	}

	private static Function<Integer, TableVariable> fromIndexToTableVariable(ArrayList<Integer> cardinalities, Function<Integer, String> fromVariableIndexToName) {
		return i -> makeTableVariable(i, cardinalities, fromVariableIndexToName);
	}

	private static TableVariable makeTableVariable(Integer i, ArrayList<Integer> cardinalities, Function<Integer, String> fromVariableIndexToName) {
		TableVariable result = new TableVariable(fromVariableIndexToName.apply(i), cardinalities.get(i));
		return result;
	}

	private static ArrayList<Double> makeUniformlyDistributedRandomEntries(
			ArrayList<Integer> cardinalities, double minimumPotential, double maximumPotential, Random random) {
		
		int numberOfEntries = product(cardinalities).intValue();
		ArrayList<Double> entries = mapIntegersIntoArrayList(numberOfEntries, i -> samplePotentialInRange(minimumPotential, maximumPotential, random));
		return entries;
	}

	private static Double samplePotentialInRange(double minimumPotential, double maximumPotential, Random random) {
		double result = minimumPotential + random.nextDouble()*(maximumPotential - minimumPotential);
		return result;
	}

}
