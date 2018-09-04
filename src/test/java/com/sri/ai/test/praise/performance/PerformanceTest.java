package com.sri.ai.test.praise.performance;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorMaker.makeRandomTableFactor;
import static com.sri.ai.util.Timer.timeAndGetResult;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.getFirstHalfSubList;
import static com.sri.ai.util.Util.getLastHalfSubList;
import static com.sri.ai.util.Util.print;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlockToFile;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Random;

import org.junit.BeforeClass;
import org.junit.Test;
import com.google.common.base.Function;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.tester.ContextSplittingTester;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.SpecsForRandomFactor;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.FromTableToExpressionFactorConverter;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.explanation.logging.api.ExplanationConfiguration;


/**
 * This class is designed to carry out performance tests on TableFactor and ExpressionFactor operations.
 * <P>
 * To use, please first adjust the "GLOBAL TEST SETTINGS" to your preferences, and then adjust the individual 
 * settings for each specific JUnit test.
 * 
 * @author Rodrigo de Salvo Braz
 * @author Bobak Pezeshki
 *
 */
public class PerformanceTest {
	

	//////////////////////////////////////////////////////////////
	// GLOBAL TEST SETTINGS  /////////////////////////////////////
	//////////////////////////////////////////////////////////////

	private static final boolean verbose = false;
	private static final boolean testMultipleTheories = false;
	private static final boolean compareWithExpectedContextSplittingTimes = false;

	private static final int timeLimitPerOperation = 10000;	// approximately how long (ms) you are willing to wait for a factor operation to complete

	private static final Theory SingledOutTheory = new DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters(false, true);
	//	theories to use:	new DifferenceArithmeticTheory(false, true)
	//						new DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters(false, true)
	
	private static final boolean includeTableFactor = false;
	private static final boolean includeTreeBasedExpressionFactors = true;
	private static final boolean includeLinearTableExpressionFactors = false;

	private static final int numberOfVariablesPerFactor = 1;
	private static final int cardinalityOfVariables = 2;
	private static final double minimumPotential = 1.0;
	private static final double maximumPotential = 10.0;
	private static final boolean integerIncrements = true;

	Function<Factor, Factor> unaryFactorOperation = (Factor f) -> sumOutAllVariables(f);
	// possible functions:	sumOutFirstHalfOfVariables(Factor f), sumOutLastHalfOfVariables(Factor f), sumOutAllVariables(Factor f), 
	//						sumOutFirstVariable(Factor f), sumOutLastVariable(Factor f)

	///////////////////////////////////////////////////////////////

			
			
	// GLOBAL CONSTANTS
	private static final Theory[] POSSIBLE_THEORIES = { new DifferenceArithmeticTheory(false, true), 
														new DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters(false, true) };
	
	private static final Theory[] THEORIES_TO_TEST = 	testMultipleTheories 	?   POSSIBLE_THEORIES	:	new Theory[] {SingledOutTheory};
	
	private static final int NUMBER_OF_TESTED_THEORIES = THEORIES_TO_TEST.length;
	private static final int NUMBER_OF_TESTED_FACTORS = (includeTableFactor 				  ?  1 							:  0) 
													  + (includeTreeBasedExpressionFactors    ?  NUMBER_OF_TESTED_THEORIES  :  0)
													  + (includeLinearTableExpressionFactors  ?  NUMBER_OF_TESTED_THEORIES  :  0);
	private static final int TABLE_FACTOR_INDEX = 0;
	private static final int TREE_BASED_EXPRESSION_FACTOR_INDEX = (includeTableFactor ? 1 : 0);
	private static final int LINEAR_TABLE_EXPRESSION_FACTOR_INDEX = TREE_BASED_EXPRESSION_FACTOR_INDEX + (includeTreeBasedExpressionFactors ?  NUMBER_OF_TESTED_THEORIES  :  0);
				
	private static final Function<Integer, String> FROM_VARIABLE_INDEX_TO_NAME = i -> "X" + i;
			
	private static final Random RANDOM = new Random();

	FromTableToExpressionFactorConverter TABLE_TO_EXPRESSION_FACTOR_CONVERTER = new FromTableToExpressionFactorConverter();
	
	private static final SpecsForRandomFactor GLOBAL_TABLE_FACTOR_SPECS = 
			new SpecsForRandomFactor( 
					fill(numberOfVariablesPerFactor, cardinalityOfVariables), // ArrayList of variable cardinalities
					minimumPotential, maximumPotential, integerIncrements);

	private static final FactorOperationResultAndTimeComparator COMPARITOR_OF_TEST_RESULTS_BY_TEST_TIME = new FactorOperationResultAndTimeComparator();
	
	
	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// JUNIT TESTS ////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	@BeforeClass
	public static void printGeneralTestingInfo() {
		println("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||");
		println("||                                      GENERAL SETTINGS                                        ||");
		println("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||");
		println("                Settings can be adjusted in the class: " + PerformanceTest.class.getSimpleName() );
		println("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||");
		printVerboseMessage();
		println("--------------------------------------------------------------------------------------------------");
		printTheoriesBeingTested();
		println("--------------------------------------------------------------------------------------------------");
		printContextSplittingSetting();
		println("||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||");
		println();
	}
	
	
	
	@Test
	public void repeatTestFxnNTimes() {
		
		ExplanationConfiguration.WHETHER_EXPLANATION_LOGGERS_ARE_ACTIVE_BY_DEFAULT = true;
		final int N = 4;
		explanationBlockToFile("explanation.txt", "Perfomance tests of unary operation...", code( () -> {	
			repeatNtimes(() -> singleRunForUnaryFactorOperation(), N);
		}));
	}

	

	//@Test
	public void singleRunForUnaryFactorOperation() {
		println("\n");

		

			println("==================================================================================================");
			println("||                                  Testing UNARY OPERATION                                     ||");
			println("==================================================================================================");
			println("  Test Parameters:");
			println("      number of variables = " + numberOfVariablesPerFactor);
			println("      variable cardinality = " + cardinalityOfVariables);
			println("==================================================================================================");

			SpecsForRandomFactor factorSpecs = new SpecsForRandomFactor(GLOBAL_TABLE_FACTOR_SPECS);
			
			ContextSplittingTester contextSplittingTest;
			long contextSplittingTime = -1;

			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);
			
			contextSplittingTest = new ContextSplittingTester(numberOfVariablesPerFactor, cardinalityOfVariables, false, SingledOutTheory); // false <-- focus on recording overall time

			ArrayList<FactorOperationResultAndTime> operationResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

			printOperationResults(factors, operationResultsAndTimes);
			
			if(compareWithExpectedContextSplittingTimes) {
			println("--------------------------------------------------------------------------------------------------");
			contextSplittingTime = contextSplittingTest.performContextSplittingTest();
			printExpectedContextSplittingTime(contextSplittingTime);
			println("--------------------------------------------------------------------------------------------------");
			printPercentageOfOperationTimeDueTo(operationResultsAndTimes, contextSplittingTime);
			}

			println("==================================================================================================");
			println();


	}
	
	
	
	//@Test
	public void varyingNumberOfVariablesForUnaryFactorOperation() {
		println("\n");
		
		println("==================================================================================================");
		println("||    Testing UNARY OPERATION based on NUMBER OF VARIABLES and comparing to CONTEXT SPLITTING   ||");
		println("==================================================================================================");
		println("  Test Parameters:");
		println("      number of variables = ||varies||");
		println("      variable cardinality = " + cardinalityOfVariables);
		println("==================================================================================================");

		SpecsForRandomFactor factorSpecs = new SpecsForRandomFactor(GLOBAL_TABLE_FACTOR_SPECS);
		
		ContextSplittingTester contextSplittingTest;
		long contextSplittingTime = -1;
		
		// STARTING VARIABLE NUMBER
		int numberOfVariables = 1;
		ArrayList<FactorOperationResultAndTime> operationResultsAndTimes;

		do {

			println("|| " + numberOfVariables + " VARIABLES ||");
			
			factorSpecs.cardinalities = fill(numberOfVariables, cardinalityOfVariables);
			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);
		
			contextSplittingTest = new ContextSplittingTester(numberOfVariables, cardinalityOfVariables, false, SingledOutTheory); // false <-- focus on recording overall time
			
			operationResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

			printOperationResults(factors, operationResultsAndTimes);
			
			if(compareWithExpectedContextSplittingTimes) {
			println("--------------------------------------------------------------------------------------------------");
			contextSplittingTime = contextSplittingTest.performContextSplittingTest();
			printExpectedContextSplittingTime(contextSplittingTime);
			println("--------------------------------------------------------------------------------------------------");
			printPercentageOfOperationTimeDueTo(operationResultsAndTimes, contextSplittingTime);
			}
			
			println("==================================================================================================");
			
		} while (estimateTimeForNextVariableCount(numberOfVariables++, operationResultsAndTimes) < timeLimitPerOperation);
		
		println();
	}
	


	//@Test
	public void varyingCardinalityOfVariablesForUnaryFactorOperation() {
		println("\n");
		
		println("==================================================================================================");
		println("|| Testing UNARY OPERATION based on CARDINALITY OF VARIABLES and comparing to CONTEXT SPLITTING ||");
		println("==================================================================================================");
		println("  Test Parameters:");
		println("      number of variables = numberOfVariablesPerFactor");
		println("      variable cardinality = ||varies||");
		println("==================================================================================================");

		SpecsForRandomFactor factorSpecs = new SpecsForRandomFactor(GLOBAL_TABLE_FACTOR_SPECS);
		
		ContextSplittingTester contextSplittingTest;
		long contextSplittingTime = -1;
		
		// STARTING VARIABLE NUMBER
		int cardinality = 1;
		ArrayList<FactorOperationResultAndTime> operationResultsAndTimes;
		do {

			println("|| CARDINALITY: " + cardinality + " ||");
			
			factorSpecs.cardinalities = fill(numberOfVariablesPerFactor, cardinality);
			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);
			
			contextSplittingTest = new ContextSplittingTester(numberOfVariablesPerFactor, cardinality, false, SingledOutTheory); // false <-- focus on recording overall time
			
			operationResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

			printOperationResults(factors, operationResultsAndTimes);
			
			if(compareWithExpectedContextSplittingTimes) {
				println("----------------------------------------------------------------------------------------------");
				contextSplittingTime = contextSplittingTest.performContextSplittingTest();
				printExpectedContextSplittingTime(contextSplittingTime);
				println("----------------------------------------------------------------------------------------------");
				printPercentageOfOperationTimeDueTo(operationResultsAndTimes, contextSplittingTime);
			}

			println("==============================================================================================");
			
		} while (estimateTimeForNextCardinality(cardinality++, operationResultsAndTimes) < timeLimitPerOperation);
		
		println();
	}
	
	
	
	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SUPPORT CLASSES AND METHODS ////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	/// STRUCTS W/ SUPPORTING METHODS //////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static class FactorOperationResultAndTime{
		public Pair<Factor, Long> resultAndTime;
		
		public FactorOperationResultAndTime(Pair<Factor, Long> resultAndTime)
		{
			this.resultAndTime = resultAndTime;
		}

		public Factor result()
		{
			return  resultAndTime.first;
		}
		
		public Long time()
		{
			return  resultAndTime.second;
		}
	}
	
	private static class FactorOperationResultAndTimeComparator implements Comparator<FactorOperationResultAndTime>{
	     
		public int compare(FactorOperationResultAndTime resultA, FactorOperationResultAndTime resultB)
	    {
			int result;
			
			if(resultA == null) {
				if(resultB == null)
				{
					result = 0;
				}
				else {
					result = -1;
				}
			}
			else if (resultB == null) {
				result = 1;
			}
			else {
				result = resultA.time().compareTo(resultB.time());
			}

	        return result;
	    }
	}
	
	
	
	
	/// FACTOR CONSTRUCTION METHODS ////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private List<Factor> constructEquivalentRandomFactors(SpecsForRandomFactor factorSpecs)
	{
		TableFactor tableFactor = makeRandomTableFactor(factorSpecs, FROM_VARIABLE_INDEX_TO_NAME, RANDOM);
		
		ArrayList<Factor> factors = new ArrayList<>(NUMBER_OF_TESTED_FACTORS);
		
		if(includeTableFactor) {
			addTableFactorToListOfFactors(tableFactor, factors);
		}
		if(includeTreeBasedExpressionFactors) {
			addTreeBasedExpressionFactorsToListOfFactors(tableFactor, factors);
		}
		if(includeLinearTableExpressionFactors) {
			addLinearTableExpressionFactorsToListOfFactors(tableFactor, factors);
		}
		
		return factors;
	}
	
	
	private void addTableFactorToListOfFactors(TableFactor tableFactor, ArrayList<Factor> factors) {
		factors.add(tableFactor);
	}
	
	private void addTreeBasedExpressionFactorsToListOfFactors(TableFactor tableFactor, ArrayList<Factor> factors) {
		Factor newFactor;
		for(Theory theory : THEORIES_TO_TEST)
		{
			newFactor = TABLE_TO_EXPRESSION_FACTOR_CONVERTER.convert(tableFactor, theory, true);
			factors.add(newFactor);
		}
	}
	
	private void addLinearTableExpressionFactorsToListOfFactors(TableFactor tableFactor, ArrayList<Factor> factors) {
		Factor newFactor;
		for(Theory theory : THEORIES_TO_TEST)
		{
			newFactor = TABLE_TO_EXPRESSION_FACTOR_CONVERTER.convert(tableFactor, theory, false);
			factors.add(newFactor);
		}
	}
	
	
	
	
	/// RECORDING RESULTS FROM FACTOR OPERATIONS ////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static ArrayList<FactorOperationResultAndTime> recordTimesForFactorOperation(Function<Factor, Factor> unaryFactorOperation, List<Factor> factors)
	{
		ArrayList<FactorOperationResultAndTime> operationResults = new ArrayList<>(NUMBER_OF_TESTED_FACTORS);
		
		for(int i = 0; i < NUMBER_OF_TESTED_FACTORS; ++i)
		{
			explain("Expression Factor Tested: ", factors.get(i));
			Factor factorToTest = factors.get(i);
			operationResults.add( timeFactorOperation(() -> unaryFactorOperation.apply(factorToTest)) );
		}
		
		return operationResults;
	}


	private static FactorOperationResultAndTime timeFactorOperation(NullaryFunction<Factor> operation) {
		FactorOperationResultAndTime result = new FactorOperationResultAndTime( timeAndGetResult(() -> operation.apply()) );
		return result;
	}
	
	
	
	
	/// POSSIBLE UNARY FACTOR OPERATIONS ///////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////

	private static Factor sumOutFirstHalfOfVariables(Factor factor) {
		List<? extends Variable> variablesToSumOut = getFirstHalfSubList(factor.getVariables());
		Factor result = factor.sumOut(variablesToSumOut);
		return result;
	}
	
	
	private static Factor sumOutLastHalfOfVariables(Factor factor) {
		List<? extends Variable> variablesToSumOut = getLastHalfSubList(factor.getVariables());
		Factor result = factor.sumOut(variablesToSumOut);
		return result;
	}
	
	
	private static Factor sumOutAllVariables(Factor factor) {
		List<? extends Variable> variablesToSumOut = factor.getVariables();
		Factor result = factor.sumOut(variablesToSumOut);
		return result;
	}
	
	
	private static Factor sumOutFirstVariable(Factor factor) {
		List<? extends Variable> factorVariables = factor.getVariables();
		int indexOfFirstVariable = 0;
		List<Variable> variablesToSumOut = new ArrayList<>();
		if(factorVariables.size() > 0)
		{
			variablesToSumOut.add(factorVariables.get(indexOfFirstVariable));
		}
		Factor result = factor.sumOut(variablesToSumOut);
		return result;
	}
	
	
	private static Factor sumOutLastVariable(Factor factor) {
		List<? extends Variable> factorVariables = factor.getVariables();
		int indexOfLastVariable = factorVariables.size() - 1;
		List<Variable> variablesToSumOut = new ArrayList<>();
		if(factorVariables.size() > 0)
		{
			variablesToSumOut.add(factorVariables.get(indexOfLastVariable));
		}
		Factor result = factor.sumOut(variablesToSumOut);

		return result;
	}
	
	
	
	/// PRINTING HELPER METHODS ////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////	
	
	private static void printOperationResults(List<Factor> factors, List<FactorOperationResultAndTime> results)
	{		
		if (verbose) {
			println("      Initial Factors and Resulting Factors:");
			if (includeTableFactor) {
			    println("          TABLE FACTOR:");
			    printFactorAndResultingFactor(factors, results, TABLE_FACTOR_INDEX, false);
			}
			if (includeTreeBasedExpressionFactors) {
				println("         TREE-BASED EXPRESSION FACTOR:");
				printFactorAndResultingFactor(factors, results, TREE_BASED_EXPRESSION_FACTOR_INDEX, true);
			}
			if (includeLinearTableExpressionFactors) {
				println("          LINEAR-TABLE EXPRESSION FACTOR:");
				printFactorAndResultingFactor(factors, results, LINEAR_TABLE_EXPRESSION_FACTOR_INDEX, true);
			}
			println("--------------------------------------------------------------------------------------------------");
		}
		
		println("      Operation Times:");
		if (includeTableFactor) {
			print("          TABLE FACTOR:");
			printOperationTimes(results, TABLE_FACTOR_INDEX, false);
		}
		if (includeTreeBasedExpressionFactors) {
			println("          TREE-BASED EXPRESSION FACTOR:");
			printOperationTimes(results, TREE_BASED_EXPRESSION_FACTOR_INDEX, true);
		}
		if (includeLinearTableExpressionFactors) {
			println("          LINEAR-TABLE EXPRESSION FACTOR:");
			printOperationTimes(results, LINEAR_TABLE_EXPRESSION_FACTOR_INDEX, true);
		}
	}
	
	
	private static void printFactorAndResultingFactor(List<Factor> factors, List<FactorOperationResultAndTime> results,
																						int startingIndex, boolean isExpressionFactor) {
		int factorIndex = startingIndex;
		if (isExpressionFactor) {
			for (int i = 1; i <= NUMBER_OF_TESTED_THEORIES; ++i) {
				println("              Theory " + i + ":  ");
				println("                  initial:  " + factors.get(factorIndex));
				printResultingFactor(results, factorIndex);
				++factorIndex;
			}
		} else {
			println("                  initial:  " + factors.get(factorIndex));
			printResultingFactor(results, factorIndex);
		}
	}

	private static void printOperationTimes(List<FactorOperationResultAndTime> results, int startingIndex,
			boolean isExpressionFactor) {
		int factorIndex = startingIndex;
		if (isExpressionFactor) {
			for (int i = 1; i <= NUMBER_OF_TESTED_THEORIES; ++i) {
				print("              Theory " + i + ":  ");
				println(results.get(factorIndex).time() + " ms");
				++factorIndex;
			}
		} else {
			println("      " + results.get(factorIndex).time() + " ms");
		}
	}
	
	private static void printPercentUsageOfTime(List<FactorOperationResultAndTime> results, long subTime, int startingIndex) {
		int factorIndex = startingIndex;
		for (int i = 1; i <= NUMBER_OF_TESTED_THEORIES; ++i) {
			print("              Theory " + i + ":  ");
			println(Math.round(1000.0 * subTime / results.get(factorIndex).time())/10.0 + "%");
			++factorIndex;
		}
	}
	

	private static void printPercentageOfOperationTimeDueTo(List<FactorOperationResultAndTime> results, long subTime) {
		println("      Percentage of Time Due To Expected Number of Context Splittings:");
		if (includeTreeBasedExpressionFactors) {
			println("          TREE-BASED EXPRESSION FACTOR:");
			printPercentUsageOfTime(results, subTime, TREE_BASED_EXPRESSION_FACTOR_INDEX);
		}
		if (includeLinearTableExpressionFactors) {
			println("          LINEAR-TABLE EXPRESSION FACTOR:");
			printPercentUsageOfTime(results, subTime, LINEAR_TABLE_EXPRESSION_FACTOR_INDEX);
		}
	}
	
	
	private static void printResultingFactor(List<FactorOperationResultAndTime> results, int index) {
		println("                  result:   " + results.get(index).result());
	}
	
	private void printExpectedContextSplittingTime(long contextSplittingTime) {
		println("      Time for Expected Number of Context Splittings:  " + contextSplittingTime + " ms");
		
	}
	
	
	
	private static void printVerboseMessage() {
		String message = verbose ?	"  Verbose mode ON."
								 :	"  Verbose mode OFF.";
		println(message);
	}	
	
	private static void printTheoriesBeingTested() {
		println("  Theories being tested:");
		for(int i = 0; i < NUMBER_OF_TESTED_THEORIES; ++i)
		{
			println("      Theory " + (i+1) + ":  " + THEORIES_TO_TEST[i]);
		}
	}
	
	private static void printContextSplittingSetting() {
		String message = compareWithExpectedContextSplittingTimes ?	
									"  Comparing with expected context splitting times."
								 :	"  Not Comparing with expected context splitting times.";
		println(message);
	}
	
	
	
	
	/// TEST OPERATION ESTIMATOR ///////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static long estimateTimeForNextVariableCount(int currentCardinality, ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes) {
		
		long timeTakenForCurrentVariable = Collections.max(opeartionResultsAndTimes, COMPARITOR_OF_TEST_RESULTS_BY_TEST_TIME).time();
		double timeForIncrementedNumberOfVariables = timeTakenForCurrentVariable * cardinalityOfVariables;
		
		return (long) timeForIncrementedNumberOfVariables;
	}
	
	private static long estimateTimeForNextCardinality(int currentCardinality, ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes) {
		
		long timeTakenForCurrentCardinality = Collections.max(opeartionResultsAndTimes, COMPARITOR_OF_TEST_RESULTS_BY_TEST_TIME).time();
		double timePerFactorParameter = timeTakenForCurrentCardinality / Math.pow(currentCardinality, numberOfVariablesPerFactor);
		double timeForIncrementedVariableCardinality = timePerFactorParameter*Math.pow(++currentCardinality, numberOfVariablesPerFactor);
		
		return (long) timeForIncrementedVariableCardinality;
	}
	
	
	
	
	/// REPEATERS //////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public static <T> T repeatNtimes(NullaryFunction<T> procedure, int N) {
		int i = 0;
		for(; i < N-1; ++i)
		{
			procedure.apply();
		}
		
		return procedure.apply();
	}
	
	public static void repeatNtimes(Runnable procedure, int N) {
		int i = 0;
		for(; i < N; ++i)
		{
			explanationBlock("Test # ", i, code( () -> {
			procedure.run();
			}), "Result is ", RESULT);
		}
	}
	
	
}


