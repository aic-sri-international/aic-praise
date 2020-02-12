package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactor.RandomTableFactorGenerator.makeRandomTableFactor;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntegersIntoList;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.pickInt;
import static com.sri.ai.util.Util.pickKElementsWithoutReplacement;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactor.ConfigurationForRandomTableFactorGeneration;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

/** 
 * A utility for generating random table factor networks given a set of parameters with self-explanatory names.
 */
public class RandomTableFactorNetworkGenerator<T extends TableFactor> implements NullaryFunction<TableFactorNetwork> {

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

	BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, T> tableFactorMaker;
	
	private Random random;
	
	private ArrayList<Integer> variableIndices;
	private int[] cardinalitiesByVariable;
	
	public static <T1 extends TableFactor> TableFactorNetwork generateRandomTableFactorNetwork(
			ConfigurationForRandomTableFactorNetworksGeneration configuration, 
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, T1> tableFactorMaker,
			Random random) {
		return 
				new RandomTableFactorNetworkGenerator<T1>(configuration, tableFactorMaker, random)
				.apply();
	}
	
	public static <T1 extends TableFactor> TableFactorNetwork generateRandomTableFactorNetwork(
			int minimumNumberOfVariables, int maximumNumberOfVariables,
			int minimumCardinality, int maximumCardinality, 
			int minimumNumberOfFactors, int maximumNumberOfFactors,
			int minimumNumberOfVariablesPerFactor, int maximumNumberOfVariablesPerFactor, 
			double minimumPotential, double maximumPotential, 
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, T1> tableFactorMaker,
			Random random) {
		
		return 
				new RandomTableFactorNetworkGenerator<T1>(minimumNumberOfVariables, maximumNumberOfVariables, minimumCardinality, maximumCardinality, minimumNumberOfFactors, maximumNumberOfFactors, minimumNumberOfVariablesPerFactor, maximumNumberOfVariablesPerFactor, minimumPotential, maximumPotential, tableFactorMaker, random)
				.apply();
	}
	
	public RandomTableFactorNetworkGenerator(
			int minimumNumberOfVariables, int maximumNumberOfVariables,
			int minimumCardinality, int maximumCardinality, 
			int minimumNumberOfFactors, int maximumNumberOfFactors,
			int minimumNumberOfVariablesPerFactor, int maximumNumberOfVariablesPerFactor, 
			double minimumPotential, double maximumPotential,
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, T> tableFactorMaker,
			Random random) {
		
		this(
				new DefaultConfigurationForRandomTableFactorNetworksGeneration(
						minimumNumberOfVariables,
						maximumNumberOfVariables,
						minimumCardinality,
						maximumCardinality,
						minimumNumberOfFactors,
						maximumNumberOfFactors,
						minimumNumberOfVariablesPerFactor,
						maximumNumberOfVariablesPerFactor,
						minimumPotential,
						maximumPotential),
				tableFactorMaker,	
				random);
	}

	public RandomTableFactorNetworkGenerator(
			ConfigurationForRandomTableFactorNetworksGeneration configuration,
			BinaryFunction<ArrayList<TableVariable>, ArrayList<Double>, T> tableFactorMaker,
			Random random) {
		
		this.minimumNumberOfVariables = configuration.getMinimumNumberOfVariables();
		this.maximumNumberOfVariables = configuration.getMaximumNumberOfVariables();
		this.minimumCardinality = configuration.getMinimumCardinality();
		this.maximumCardinality = configuration.getMaximumCardinality();
		this.minimumNumberOfFactors = configuration.getMinimumNumberOfFactors();
		this.maximumNumberOfFactors = configuration.getMaximumNumberOfFactors();
		this.minimumNumberOfVariablesPerFactor = configuration.getMinimumNumberOfVariablesPerFactor();
		this.maximumNumberOfVariablesPerFactor = configuration.getMaximumNumberOfVariablesPerFactor();
		this.minimumPotential = configuration.getMinimumPotential();
		this.maximumPotential = configuration.getMaximumPotential();
		this.tableFactorMaker = tableFactorMaker;
		this.random = random;
	}

	@Override
	public TableFactorNetwork apply() {
		makeVariables();
		var factors = makeFactors();
		return new TableFactorNetwork(factors);
	}

	public void makeVariables() {
		int numberOfVariables = pickInt(minimumNumberOfVariables, maximumNumberOfVariables + 1, random);
		explain("Number of variables: ", numberOfVariables);
		variableIndices = new ArrayList<>(numberOfVariables);
		cardinalitiesByVariable = new int[numberOfVariables];
		for (int i = 0; i != numberOfVariables; i++) {
			makeVariable(i);
		}
	}

	public void makeVariable(int i) {
		variableIndices.add(i);
		cardinalitiesByVariable[i] = pickInt(minimumCardinality, maximumCardinality + 1, random);
	}

	public List<TableFactor> makeFactors() {
		int numberOfFactors = pickInt(minimumNumberOfFactors, maximumNumberOfFactors + 1, random);
		List<TableFactor> factors = mapIntegersIntoList(numberOfFactors, i -> makeFactor());
		return factors;
	}

	public TableFactor makeFactor() {
		int numberOfVariablesInFactor = pickInt(minimumNumberOfVariablesPerFactor, maximumNumberOfVariablesPerFactor + 1, random);
		ArrayList<Integer> variablesInFactor = pickKElementsWithoutReplacement(variableIndices, numberOfVariablesInFactor, random);
		explain("Number of variables in factor: ", numberOfVariablesInFactor);
		explain("Variables in factor: ", variablesInFactor);
		ArrayList<Integer> cardinalitiesInFactor = mapIntoArrayList(variablesInFactor, index -> cardinalitiesByVariable[index]);
		ConfigurationForRandomTableFactorGeneration specs = makeSpecs(cardinalitiesInFactor);
		TableFactor factor = makeRandomTableFactor(specs, index -> "X" + variablesInFactor.get(index), tableFactorMaker, random);
		return factor;
	}

	public ConfigurationForRandomTableFactorGeneration makeSpecs(ArrayList<Integer> cardinalitiesInFactor) {
		return new ConfigurationForRandomTableFactorGeneration(cardinalitiesInFactor, minimumPotential, maximumPotential, false);
	}
	
	public static void main(String[] args) {

		ThreadExplanationLogger.setIsActive(false);
		
		int minimumNumberOfVariables = 10;
		int maximumNumberOfVariables = 10;
		
		int minimumCardinality = 2;
		int maximumCardinality = 2;
		
		int minimumNumberOfFactors = 5;
		int maximumNumberOfFactors = 5;

		int minimumNumberOfVariablesPerFactor = 3;
		int maximumNumberOfVariablesPerFactor = 5;

		double minimumPotential = 1.0;
		double maximumPotential = 4.0;
		
		TableFactorNetwork network = 
				generateRandomTableFactorNetwork(
						minimumNumberOfVariables, maximumNumberOfVariables, 
						minimumCardinality, maximumCardinality, 
						minimumNumberOfFactors, maximumNumberOfFactors, 
						minimumNumberOfVariablesPerFactor, maximumNumberOfVariablesPerFactor, 
						minimumPotential, maximumPotential,
						(variables, entries) -> new ArrayTableFactor(variables, entries),
						new Random());
		
		println(join("\n", network.getVariables()));
		println();
		println(join("\n", network.getFactors()));
	}

}
