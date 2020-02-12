package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactor;

import static com.sri.ai.util.Util.mapIntegersIntoArrayList;
import static com.sri.ai.util.Util.product;

import java.util.ArrayList;
import java.util.Random;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.util.base.BinaryFunction;

public class RandomTableFactorGenerator {
	
	public static <TableFactor> TableFactor makeRandomTableFactor(
			ConfigurationForRandomTableFactorGeneration specs, 
			Function<Integer, String> fromVariableIndexToName,
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, TableFactor> tableFactorMaker,
			Random random) {
		
		ArrayList<TableVariable> variables = makeVariables(specs.cardinalities, fromVariableIndexToName);
		ArrayList<Double> entries = makeUniformlyDistributedRandomEntries(specs, random);
		TableFactor tableFactor = tableFactorMaker.apply(variables, entries);
		return tableFactor;
	}


	private static ArrayList<TableVariable> makeVariables(
			ArrayList<Integer> cardinalities, 
			Function<Integer, String> fromVariableIndexToName) {
		
		int numberOfVariables = cardinalities.size();
		ArrayList<TableVariable> result = 
				mapIntegersIntoArrayList(numberOfVariables, fromIndexToTableVariable(cardinalities, fromVariableIndexToName));
		return result;
	}

	private static Function<Integer, TableVariable> fromIndexToTableVariable(
			ArrayList<Integer> cardinalities, 
			Function<Integer, String> fromVariableIndexToName) {
		return i -> makeTableVariable(i, cardinalities, fromVariableIndexToName);
	}

	private static TableVariable makeTableVariable(
			Integer i, 
			ArrayList<Integer> cardinalities, 
			Function<Integer, String> fromVariableIndexToName) {
		
		TableVariable result = new TableVariable(fromVariableIndexToName.apply(i), cardinalities.get(i));
		return result;
	}

	private static ArrayList<Double> makeUniformlyDistributedRandomEntries(
			ConfigurationForRandomTableFactorGeneration specs, Random random) {
		
		int numberOfEntries = product(specs.cardinalities).intValue();
		ArrayList<Double> entries = mapIntegersIntoArrayList(numberOfEntries, i -> samplePotentialInRange(specs, random));
		return entries;
	}

	private static Double samplePotentialInRange(ConfigurationForRandomTableFactorGeneration specs, Random random) {
		double result;
		if (specs.integerIncrements) {
			result = specs.minimumPotential + random.nextInt((int) (Math.round(specs.maximumPotential)));
		}
		else {
			result = specs.minimumPotential + random.nextDouble()*(specs.maximumPotential - specs.minimumPotential);
		}
	
		return result;
	}

}
