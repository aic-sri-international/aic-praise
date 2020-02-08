package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.randomgeneration.tablefactornetwork.RandomTableFactorNetworkGenerator.generateRandomTableFactorNetwork;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.repeat;
import static com.sri.ai.util.base.Pair.pair;

import java.util.ArrayList;
import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBPSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering.MinFillEliminationOrdering;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.ArrayListTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration.ConfigurationForTestsOnRandomTableFactorNetworks;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration.LargerProblems;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration.SmallProblems;
import com.sri.ai.util.Timer;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

class TestsOnRandomTableFactorNetworks {

	private
	ArrayList<Pair<String, BinaryFunction<Variable, FactorNetwork, Factor>>> algorithms = arrayList( 
			pair("VE_MI", new VariableEliminationSolver(new MinFillEliminationOrdering())),
			// pair("VE_DC", new VariableEliminationSolver(new DontCareEliminationOrdering())),
			pair("  EBP", new ExactBPSolver())
	);
	
	int totalTime[] = new int[algorithms.size()];
	
	@Test
	void test() {
		run(new SmallProblems());
	}

	private void run(ConfigurationForTestsOnRandomTableFactorNetworks configuration) {
		Random random = new Random();

		repeat(configuration.getNumberOfTests(), testNumber ->
		runTestForARandomFactorNetwork(testNumber, configuration, random));
		
		printTotalTimes();
	}

	public void runTestForARandomFactorNetwork(
			int testNumber,
			ConfigurationForTestsOnRandomTableFactorNetworks configuration,
			Random random) {
		
		TableFactorNetwork factorNetwork = generateRandomTableFactorNetwork(configuration, random);

		Variable query = getFirst(factorNetwork.getFactors()).getVariables().get(0);
		
		println();
		println("********************");
		println("Test #" + testNumber);
		println();
		println("Number of variables: " + factorNetwork.getVariables().size());
		println("Number of factors: " + factorNetwork.getFactors().size());
		println();

		ArrayList<Pair<Factor, Long>> resultsAndTimes = new ArrayList<>(algorithms.size());
		
		computeResults(query, factorNetwork, resultsAndTimes);
		
		printResults(resultsAndTimes);
		
		compareResults(resultsAndTimes);

		println("Done!\n");
		
	}

	private void computeResults(Variable query, TableFactorNetwork factorNetwork, ArrayList<Pair<Factor, Long>> resultsAndTimes) {
		for (int i = 0; i != algorithms.size(); i++) {
			var name = algorithms.get(i).first;
			var algorithm = algorithms.get(i).second;
			var resultAndTime = resultAndTime(name, () -> algorithm.apply(query, factorNetwork));
			totalTime[i] += resultAndTime.second;
			resultsAndTimes.add(resultAndTime);
			println();
		}
	}

	private Pair<Factor, Long> resultAndTime(String name, NullaryFunction<Factor> algorithmInstance) {
		println("Running " + name + "...");
		var resultAndTime = Timer.timeAndGetResult(algorithmInstance);
		println("Done running  " + name + ". Time: " + resultAndTime.second + " ms.");
		return resultAndTime;
	}

	private void printResults(ArrayList<Pair<Factor, Long>> resultsAndTimes) {
		for (int i = 0; i != algorithms.size(); i++) {
			var name = algorithms.get(i).first;
			var resultAndTime = resultsAndTimes.get(i);
			println(name + ": " + resultAndTimeString(resultAndTime));
		}
		println();
	}

	public String resultAndTimeString(Pair<Factor, Long> resultAndTime) {
		return resultAndTime.first + ", " + resultAndTime.second + " ms";
	}

	private void compareResults(ArrayList<Pair<Factor, Long>> resultsAndTimes) {
		for (int i = 0; i != algorithms.size() - 1; i++) {
			var name1 = algorithms.get(i).first;
			var name2 = algorithms.get(i + 1).first;
			var resultAndTime1 = resultsAndTimes.get(i);
			var resultAndTime2 = resultsAndTimes.get(i + 1);
			var array1 = ((ArrayListTableFactor) resultAndTime1.first).getEntries();
			var array2 = ((ArrayListTableFactor) resultAndTime2.first).getEntries();
			println("Comparing " + name1 + " and " + name2 + "...");
			Util.compareNumbersComponentWise(array1, array2, 0.001);
		}
		println();
	}

	private void printTotalTimes() {
		for (int i = 0; i != algorithms.size(); i++) {
			var name = algorithms.get(i).first;
			println("Total time " + name + ": " + totalTime[i]);
		}
	}

	public static void main(String[] args) {
		new TestsOnRandomTableFactorNetworks().run(new LargerProblems());
	}
}
