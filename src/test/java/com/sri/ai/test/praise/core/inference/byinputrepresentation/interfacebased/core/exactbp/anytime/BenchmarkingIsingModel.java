package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime;

import static com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.TestCases.gridModelWithRandomFactors;
import static com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.TestCases.isingModelGridWithWeigthsAndPotetialFixed;
import static com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.TestCases.isingModelGridWithWeigthsAndPotetialNormalyDistributed;
import static com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.AnytimeExactBPTest2.solveWithGabriels;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.pair;
import static com.sri.ai.util.Util.println;

import java.util.LinkedList;
import java.util.List;
import java.util.function.BiFunction;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.TestCases;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.rplot.dataframe.AEBPTestingDataFrame;

public class BenchmarkingIsingModel {
	public static void main(String[] args) {
		int timeLimit = 1;//100;
		int nRepetitions = 3;
		
		LinkedList<Integer> complexitiesForExactBP = list(2,3,4,5,6);//,7,8); // 10 is too much I think
		
		AEBPTestingDataFrame df = new AEBPTestingDataFrame();
		
		testManyEBP(nRepetitions, complexitiesForExactBP, df);
		toCsv(df);
		
		int complexityForAnytime = 20;
		
		LinkedList<Pair<Double, Double>> listWithOneElement = list(pair(1., 1.));	
		
		LinkedList<Pair<Double,Double>> weightsForGridWithRandomFactors = listWithOneElement;
		LinkedList<Pair<Double, Double>> weightsForIsingWithWeigthsAndPotentialFixed =
				list(
						pair(.01, .01), pair(.01, .1), pair(.01, 1.), pair(.01, 10.),
						pair(.1 , .01), pair(.1 , .1), pair(.1 , 1.), pair(.1 , 10.),
						pair(1. , .01), pair(1. , .1), pair(1. , 1.), pair(1. , 10.),
						pair(10., .01), pair(10., .1), pair(10., 1.), pair(10., 10.)
					);
		LinkedList<Pair<Double, Double>> weightsForIsingWithWeigthsAndPotentialNormal = 
				list(
						pair(.001, -1.),
						pair(.01 , -1.),
						pair(1.  , -1.),
						pair(10. , -1.));
	
		testManyAEBP(timeLimit, nRepetitions, df, complexityForAnytime, 
				weightsForGridWithRandomFactors,
				weightsForIsingWithWeigthsAndPotentialFixed, 
				weightsForIsingWithWeigthsAndPotentialNormal);

		// write dataframe
		toCsv(df);
		println("done!!!!!");
	}

	private static void toCsv(AEBPTestingDataFrame df) {
		df.printToCsv("../masterThesis/data/benchmark_ising/exactBPAndAEBPIsing.csv");
	}

	private static void testManyAEBP(int timeLimit, int nRepetitions, AEBPTestingDataFrame df, int complexityForAnytime,
			LinkedList<Pair<Double, Double>> weightsForGridWithRandomFactors,
			LinkedList<Pair<Double, Double>> weightsForIsingWithWeigthsAndPotentialFixed,
			LinkedList<Pair<Double, Double>> weightsForIsingWithWeigthsAndPotentialNormal) {
		BiFunction<Integer, List<TableFactor>, TableVariable> getQuery = 
				(complexity,network) -> TestCases.getTableVariableByName("A_"+complexity/2+"_"+complexity/2, network);
		testAEBP(df,nRepetitions,timeLimit,complexityForAnytime,
				weightsForGridWithRandomFactors,(comp,weight) -> gridModelWithRandomFactors(comp, true), getQuery,
				"grid-random-factors");

		testAEBP(df,nRepetitions,timeLimit,complexityForAnytime,
				weightsForIsingWithWeigthsAndPotentialFixed,(comp,weight) -> isingModelGridWithWeigthsAndPotetialFixed(comp, weight.first,weight.second, true), getQuery,
				"ising-fixed-factors");

		testAEBP(df,nRepetitions,timeLimit,complexityForAnytime,
				weightsForIsingWithWeigthsAndPotentialNormal,(comp,weight) -> isingModelGridWithWeigthsAndPotetialNormalyDistributed(comp,weight.first, true), getQuery,
				"ising-normal-random");
	}

	private static void testManyEBP(int nRepetitions, LinkedList<Integer> complexitiesForExactBP,
			AEBPTestingDataFrame df) {
		LinkedList<Pair<Double, Double>> listWithOneElement = list(pair(1., 1.));	
		BiFunction<Integer, List<TableFactor>, TableVariable> getQuery = 
				(complexity,network) -> TestCases.getTableVariableByName("A_"+complexity/2+"_"+complexity/2, network);
		
		testEBP(df,nRepetitions,complexitiesForExactBP, 
				listWithOneElement, (comp,weight) -> gridModelWithRandomFactors(comp, true), getQuery,
				"grid-random-factors");
		
		testEBP(df,nRepetitions,complexitiesForExactBP, 
				listWithOneElement, (comp,weight) -> isingModelGridWithWeigthsAndPotetialFixed(comp, weight.first,weight.second, true), getQuery,
				"ising-fixed-factors");
		
		testEBP(df,nRepetitions,complexitiesForExactBP, 
				listWithOneElement, (comp,weight) -> isingModelGridWithWeigthsAndPotetialNormalyDistributed(comp,weight.first, true), getQuery,
				"ising-normal-random");
	}

	private static void testAEBP(AEBPTestingDataFrame df, int nRepetitions, int timeLimit, int complexity,
			LinkedList<Pair<Double, Double>> weights, BiFunction<Integer, Pair<Double,Double>, List<? extends Factor>> networkGenerator,
			BiFunction<Integer, List<TableFactor>, TableVariable> getQuery, String netName) {

		for(Pair<Double, Double> weight : weights) {
			for (int i = 0; i < nRepetitions; i++) {
				@SuppressWarnings("unchecked")
				List<TableFactor> network = (List<TableFactor>) networkGenerator.apply(complexity, weight);
				TableVariable query = getQuery.apply(complexity, network);
				
				testAEBPP(df,i,timeLimit,network,query,netName + " - " + complexity);
			}
		}

	}

	private static void testAEBPP(AEBPTestingDataFrame df, int runNumber, int timeLimit, List<TableFactor> network,
			TableVariable query, String netName) {
		TableFactorNetwork factorNet = new TableFactorNetwork(network);
		
		println("solveWithExactBP : " + netName + "; iteration: " + runNumber);
		AEBPTestingDataFrame dfToAdd = solveWithGabriels(query, factorNet, timeLimit, netName,runNumber);
		addRowsFromSecondDfToFirstDf(df,dfToAdd);
		toCsv(df);	
	}

	private static void addRowsFromSecondDfToFirstDf(AEBPTestingDataFrame df, AEBPTestingDataFrame dfToAdd) {
		for (int i = 0; i < dfToAdd.getNumberOfRows(); i++) {
			df.addRow(dfToAdd.getRow(i));
		}
	}


	private static void testEBP(AEBPTestingDataFrame df, int nRepetitions, LinkedList<Integer> complexities,
			LinkedList<Pair<Double, Double>> weights, BiFunction<Integer, Pair<Double,Double>, List<? extends Factor>> networkGenerator,
			BiFunction<Integer, List<TableFactor>, TableVariable> getQuery, String netName) {
		// TestEBP
		for(Integer complexity : complexities) {
			for(Pair<Double, Double> weight : weights) {
				@SuppressWarnings("unchecked")
				List<TableFactor> network = (List<TableFactor>) networkGenerator.apply(complexity, weight);
				TableVariable query = getQuery.apply(complexity, network);
				testEBPP(df,nRepetitions,network,query,netName + " - " + complexity);
			}
		}
	}

	private static void testEBPP(AEBPTestingDataFrame df, int nRepetitions, List<TableFactor> network, TableVariable query, String netName) {
		TableFactorNetwork factorNetwork = new TableFactorNetwork(network);

		ExactBPNode<Variable,Factor> exactBP = new ExactBP(query, factorNetwork);
		for (int i = 0; i < nRepetitions; i++) {	
			println("solveWithExactBP : " + netName);
			Pair<Double, Factor> p = solveAndPrint(exactBP);
			df.addRow(
					i,//run number
					-1, // iteration
					((TableFactor)p.second).getEntries().get(0),//minPTrue
					((TableFactor)p.second).getEntries().get(0),//MaxPTrue
					p.first,//iteration time
					p.first,//total time
					"ExactBP",//InferenceMethodUsed
					netName + "; query: " + query.toString()//GraphicalModelName
					);
		}
	}
	
	public static <T> Pair<Double,T> solveAndPrint(NullaryFunction<T> func) {
		long startTime = System.currentTimeMillis();
		System.out.println("Started...");
        T result;
		result = func.apply();
        System.out.println("Finished!");
        
        long TotalTime = System.currentTimeMillis() - startTime;
        Double timeInSeconds = TotalTime / 1000.;
        println("result: " + result);
        println("time  : " + timeInSeconds);
        
        return new Pair<Double, T>(timeInSeconds, result);
	}
}
