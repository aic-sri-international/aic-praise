package com.sri.ai.test.praise.performance;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorMaker.makeRandomTableFactor;
import static com.sri.ai.util.Timer.timeAndGetResult;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.getFirstHalfSubList;
import static com.sri.ai.util.Util.getLastHalfSubList;
import static com.sri.ai.util.Util.print;
import static com.sri.ai.util.Util.println;
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
import org.junit.jupiter.api.Test;

import com.google.common.base.Function;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.constraint.ConstraintSplitting;
import com.sri.ai.grinder.tester.ContextSplittingTester;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.SpecsForRandomTableFactorGeneration;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.FromTableToExpressionFactorConverter;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.explanation.logging.api.ExplanationConfiguration;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;


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
	
	// GLOBAL CONSTANTS	
	private static final Theory DIFFERENCE_ARITHMETIC_THEORY = new DifferenceArithmeticTheory(false, true);
	private static final Theory SIMPLIFIED_DIFFERENCE_ARITHMETIC_THEORY = new DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters(false, true);
	private static final Theory[] POSSIBLE_THEORIES = 
		{ 
				SIMPLIFIED_DIFFERENCE_ARITHMETIC_THEORY,
				DIFFERENCE_ARITHMETIC_THEORY, 
		};

	
		//////////////////////////////////////////////////////////////
		// GLOBAL TEST SETTINGS  /////////////////////////////////////
		//////////////////////////////////////////////////////////////
	
		private static final boolean verbose = true;
		private static final boolean testMultipleTheories = true;
		private static final boolean compareWithSimulatedContextSplittingTimes = true;
		
		private static final int initialNumberOfVariablesWhenVaryingThat = 5;
		private static final int finalNumberOfVariablesWhenVaryingThat = 8;
	
		private static final int timeLimitPerOperation = 10000;	// approximately how long (ms) you are willing to wait for a factor operation to complete
	
		private static final Theory singledOutTheory = SIMPLIFIED_DIFFERENCE_ARITHMETIC_THEORY;
		//	theories to use:	DIFFERENCE_ARITHMETIC_THEORY
		//						SIMPLIFIED_DIFFERENCE_ARITHMETIC_THEORY
		
		private static final boolean includeTableFactor = true;
		private static final boolean includeTreeBasedExpressionFactors = true;
		private static final boolean includeLinearTableExpressionFactors = false;
	
		private static final int numberOfVariablesPerFactor = 2;
		private static final int cardinalityOfVariables = 2;
		private static final double minimumPotential = 1.0;
		private static final double maximumPotential = 10.0;
		private static final boolean integerIncrements = true;
	
		Function<Factor, Factor> unaryFactorOperation = PerformanceTest::sumOutAllVariables;
		// possible functions:	sumOutFirstHalfOfVariables(Factor f), sumOutLastHalfOfVariables(Factor f), sumOutAllVariables(Factor f), 
		//						sumOutFirstVariable(Factor f), sumOutLastVariable(Factor f)
	
		///////////////////////////////////////////////////////////////

			
			
	// GLOBAL CONSTANTS	-- CONTINUED
	private static final Theory[] THEORIES_TO_TEST = 	testMultipleTheories 	?   POSSIBLE_THEORIES	:	new Theory[] {singledOutTheory};
	
	private static final int NUMBER_OF_TESTED_THEORIES = THEORIES_TO_TEST.length;
	private static final int NUMBER_OF_TESTED_FACTORS = (includeTableFactor 				  ?  1 							:  0) 
													  + (includeTreeBasedExpressionFactors    ?  NUMBER_OF_TESTED_THEORIES  :  0)
													  + (includeLinearTableExpressionFactors  ?  NUMBER_OF_TESTED_THEORIES  :  0);
	private static final int TABLE_FACTOR_INDEX = 0;
	private static final int TREE_BASED_EXPRESSION_FACTOR_INDEX = (includeTableFactor ? 1 : 0);
	private static final int LINEAR_TABLE_EXPRESSION_FACTOR_INDEX = (includeTableFactor? 1 : 0) + (includeTreeBasedExpressionFactors? 1 : 0);
				
	private static final Function<Integer, String> FROM_VARIABLE_INDEX_TO_NAME = i -> "X" + i;
			
	private static final Random RANDOM = new Random(0);

	private static final FromTableToExpressionFactorConverter TABLE_TO_EXPRESSION_FACTOR_CONVERTER = new FromTableToExpressionFactorConverter();
	
	private static final SpecsForRandomTableFactorGeneration GLOBAL_TABLE_FACTOR_SPECS = 
			new SpecsForRandomTableFactorGeneration( 
					fill(numberOfVariablesPerFactor, cardinalityOfVariables), // ArrayList of variable cardinalities
					minimumPotential, maximumPotential, integerIncrements);

	private static final FactorOperationResultAndTimeComparator COMPARATOR_OF_TEST_RESULTS_BY_TEST_TIME = new FactorOperationResultAndTimeComparator();
	
	
	
	
	
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
	public void repeatTestFunctionNTimes() {
		//repeatNtimes(() -> singleRunForUnaryFactorOperation(), 5); // burn-in for class loading, etc
		
		final int n = 1;
		explanationBlockToFile("explanation.txt", "Perfomance tests of unary operation...", code( () -> {	
			repeatNtimes(() -> singleRunForUnaryFactorOperation(), n);
		}));
	}

	

	private void singleRunForUnaryFactorOperation() {

		println("\n");
		println("==================================================================================================");
		println("||                                  Testing UNARY OPERATION                                     ||");
		println("==================================================================================================");
		println("  Test Parameters:");
		println("      number of variables = " + numberOfVariablesPerFactor);
		println("      variable cardinality = " + cardinalityOfVariables);
		println("==================================================================================================");

		SpecsForRandomTableFactorGeneration factorGenerationSpecs = new SpecsForRandomTableFactorGeneration(GLOBAL_TABLE_FACTOR_SPECS);

		List<Factor> factors = constructEquivalentRandomFactorsRepresentedInDifferentWays(factorGenerationSpecs);

		ArrayList<FactorOperationResultAndTimes> operationResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

		printOperationResults(factors, operationResultsAndTimes);

		compareWithSimulatedContextSplittingTimesIfNeeded(operationResultsAndTimes);

		println("==================================================================================================");
		println();

	}



	@Test
	public void varyingNumberOfVariablesForUnaryFactorOperationWithBurnIn() {
		varyingNumberOfVariablesForUnaryFactorOperationBurnIn();
		varyingNumberOfVariablesForUnaryFactorOperation();
	}



	private void varyingNumberOfVariablesForUnaryFactorOperationBurnIn() {
		boolean oldValue = ExplanationConfiguration.setWhetherExplanationLoggersAreActiveByDefaultAndReturnOldValue(false);
		for (int i = 0; i != 3; i++) {
			varyingNumberOfVariablesForUnaryFactorOperation();
		}
		ExplanationConfiguration.setWhetherExplanationLoggersAreActiveByDefaultAndReturnOldValue(oldValue);
	}
	
	@Test
	public void varyingNumberOfVariablesForUnaryFactorOperation() {
	
		println("\n");
		println("==================================================================================================");
		println("||    Testing UNARY OPERATION based on NUMBER OF VARIABLES and comparing to CONTEXT SPLITTING   ||");
		println("==================================================================================================");
		println("  Test Parameters:");
		println("      number of variables = ||varies||");
		println("      variable cardinality = " + cardinalityOfVariables);
		println("==================================================================================================");

		SpecsForRandomTableFactorGeneration factorSpecs = new SpecsForRandomTableFactorGeneration(GLOBAL_TABLE_FACTOR_SPECS);
		
		explanationBlockToFile("explanation.txt", "Perfomance tests of unary operation...", code( () -> {	
			// STARTING VARIABLE NUMBER
			int numberOfVariables = initialNumberOfVariablesWhenVaryingThat - 1;
			ArrayList<FactorOperationResultAndTimes> operationResultsAndTimes;
			do {
				numberOfVariables++;
				if (numberOfVariables == finalNumberOfVariablesWhenVaryingThat + 1) break;
				operationResultsAndTimes = evaluateForGivenNumberOfVariables(numberOfVariables, factorSpecs);
			} while (estimateTimeForNextVariableCount(operationResultsAndTimes) < timeLimitPerOperation);
		}));
		
		println();
	}



	private ArrayList<FactorOperationResultAndTimes> evaluateForGivenNumberOfVariables(
			int numberOfVariables,
			SpecsForRandomTableFactorGeneration factorSpecs) {

		return
		explanationBlock(numberOfVariables + " variables...", code( () -> {	
			println("|| " + numberOfVariables + " VARIABLES ||");
			factorSpecs.cardinalities = fill(numberOfVariables, cardinalityOfVariables);
			ArrayList<FactorOperationResultAndTimes> operationResultsAndTimes = evaluate(factorSpecs);
			println("==================================================================================================");
			return operationResultsAndTimes;
		}));
	}



	@Test
	public void varyingCardinalityOfVariablesForUnaryFactorOperation() {
		println("\n");
		
		println("==================================================================================================");
		println("|| Testing UNARY OPERATION based on CARDINALITY OF VARIABLES and comparing to CONTEXT SPLITTING ||");
		println("==================================================================================================");
		println("  Test Parameters:");
		println("      number of variables = numberOfVariablesPerFactor");
		println("      variable cardinality = ||varies||");
		println("==================================================================================================");

		SpecsForRandomTableFactorGeneration factorSpecs = new SpecsForRandomTableFactorGeneration(GLOBAL_TABLE_FACTOR_SPECS);
		
		// STARTING VARIABLE NUMBER
		int cardinality = 0;
		ArrayList<FactorOperationResultAndTimes> operationResultsAndTimes;
		do {
			cardinality++;
			
			println("|| CARDINALITY: " + cardinality + " ||");
			factorSpecs.cardinalities = fill(numberOfVariablesPerFactor, cardinality);

			operationResultsAndTimes = evaluate(factorSpecs);
			
			println("==================================================================================================");
			
		} while (estimateTimeForNextCardinality(cardinality, operationResultsAndTimes) < timeLimitPerOperation);
		
		println();
	}


	private ArrayList<FactorOperationResultAndTimes> evaluate(SpecsForRandomTableFactorGeneration factorSpecs) {
		ArrayList<FactorOperationResultAndTimes> operationResultsAndTimes;
		List<Factor> factors = constructEquivalentRandomFactorsRepresentedInDifferentWays(factorSpecs);

		operationResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

		printOperationResults(factors, operationResultsAndTimes);

		compareWithSimulatedContextSplittingTimesIfNeeded(operationResultsAndTimes);
		return operationResultsAndTimes;
	}



///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SUPPORT CLASSES AND METHODS ////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	private void compareWithSimulatedContextSplittingTimesIfNeeded(List<FactorOperationResultAndTimes> operationResultsAndTimes) {
		if (compareWithSimulatedContextSplittingTimes) {
			ThreadExplanationLogger.explanationBlock("Running context splitting", code(() -> {
				ContextSplittingTester contextSplittingTest;
				long timeForSimulatedContextSplittings;

				contextSplittingTest = new ContextSplittingTester(numberOfVariablesPerFactor, cardinalityOfVariables, /* verbose */ false, singledOutTheory);
				println("--------------------------------------------------------------------------------------------------");
				timeForSimulatedContextSplittings = contextSplittingTest.performContextSplittingTest();
				printSimulatedContextSplittingTime(timeForSimulatedContextSplittings);
				println("--------------------------------------------------------------------------------------------------");
				printPercentageOfOperationTimesDueTo(operationResultsAndTimes, timeForSimulatedContextSplittings);
				println("--------------------------------------------------------------------------------------------------");
				printActualContextSplittingTime(operationResultsAndTimes);
				println("--------------------------------------------------------------------------------------------------");
				printPercentageOfOperationTimesDueToContextSplitting(operationResultsAndTimes);
			}));
		}
	}






	/// STRUCTS W/ SUPPORTING METHODS //////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	/**
	 * Struct to hold relevant results from a factor operation.
	 * <p>
	 * Field a Triple resultAndTimes such that: <br>
	 * ... resultAndTimes.first = the resulting factor from the factor operation <br>
	 * ... resultAndTimes.second = the time taken by the factor operation <br>
	 * ... resultAndTimes.third = the total time taken to split contexts <br>
	 *    
	 * @author Bobak
	 *
	 */
	private static class FactorOperationResultAndTimes {
		public Triple<Factor, Long, Long> resultAndTimes;
		
		public FactorOperationResultAndTimes(Triple<Factor, Long, Long> resultAndTimes) {
			this.resultAndTimes = resultAndTimes;
		}
		
		public FactorOperationResultAndTimes(Factor result, Long operationTime, Long contextSplittingTime) {
			this(new Triple<Factor, Long, Long>(result, operationTime, contextSplittingTime));
		}

		public Factor result() {
			return  resultAndTimes.first;
		}
		
		public Long operationTime() {
			return  resultAndTimes.second;
		}
		
		public Long contextSplittingTime() {
			return  Math.round(resultAndTimes.third/1000000.0);
		}
	}
	
	private static class FactorOperationResultAndTimeComparator implements Comparator<FactorOperationResultAndTimes> {
	     
		@Override
		public int compare(FactorOperationResultAndTimes resultA, FactorOperationResultAndTimes resultB) {
			int result;
			
			if (resultA == null) {
				if (resultB == null) {
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
				result = resultA.operationTime().compareTo(resultB.operationTime());
			}

	        return result;
	    }
	}
	
	
	
	
	/// FACTOR CONSTRUCTION METHODS ////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private List<Factor> constructEquivalentRandomFactorsRepresentedInDifferentWays(SpecsForRandomTableFactorGeneration factorSpecs)
	{
		TableFactor tableFactor = makeRandomTableFactor(factorSpecs, FROM_VARIABLE_INDEX_TO_NAME, RANDOM);
		
		ArrayList<Factor> factors = new ArrayList<>(NUMBER_OF_TESTED_FACTORS);
		
		if (includeTableFactor) {
			addTableFactorToListOfFactors(tableFactor, factors);
		}
		if (includeTreeBasedExpressionFactors) {
			addTreeBasedExpressionFactorsToListOfFactors(tableFactor, factors);
		}
		if (includeLinearTableExpressionFactors) {
			addLinearTableExpressionFactorsToListOfFactors(tableFactor, factors);
		}
		
		return factors;
	}
	
	
	private void addTableFactorToListOfFactors(TableFactor tableFactor, ArrayList<Factor> factors) {
		factors.add(tableFactor);
	}
	
	private void addTreeBasedExpressionFactorsToListOfFactors(TableFactor tableFactor, ArrayList<Factor> factors) {
		Factor newFactor;
		for (Theory theory : THEORIES_TO_TEST) {
			newFactor = TABLE_TO_EXPRESSION_FACTOR_CONVERTER.convert(tableFactor, theory, true);
			factors.add(newFactor);
		}
	}
	
	private void addLinearTableExpressionFactorsToListOfFactors(TableFactor tableFactor, ArrayList<Factor> factors) {
		Factor newFactor;
		for (Theory theory : THEORIES_TO_TEST) {
			newFactor = TABLE_TO_EXPRESSION_FACTOR_CONVERTER.convert(tableFactor, theory, false);
			factors.add(newFactor);
		}
	}
	
	
	
	
	/// RECORDING RESULTS FROM FACTOR OPERATIONS ////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static ArrayList<FactorOperationResultAndTimes> recordTimesForFactorOperation(Function<Factor, Factor> unaryFactorOperation, List<Factor> factors) {
		ArrayList<FactorOperationResultAndTimes> operationResults = new ArrayList<>(NUMBER_OF_TESTED_FACTORS);
		
		for (int i = 0; i < NUMBER_OF_TESTED_FACTORS; ++i) {
			Factor factor = factors.get(i);
			testFactor(factor, unaryFactorOperation, operationResults);
		}
		
		return operationResults;
	}



	private static void testFactor(
			Factor factor, 
			Function<Factor, Factor> unaryFactorOperation, 
			ArrayList<FactorOperationResultAndTimes> operationResults) {
		
		explanationBlock("Factor tested: ", factor, code( () -> {
			if (factor instanceof ExpressionFactor) {
				explain("ExpressionFactor with theory " + ((ExpressionFactor) factor).getContext().getTheory());
			}
			operationResults.add( timeFactorOperation(() -> unaryFactorOperation.apply(factor)) );
		}));
	}


	private static FactorOperationResultAndTimes timeFactorOperation(NullaryFunction<Factor> operation) {
		ConstraintSplitting.resetTotalConstraintSplittingTime();
		Pair<Factor,Long> timeAndResult = timeAndGetResult( () -> operation.apply() );
		long contextSplittingTime = ConstraintSplitting.getTotalConstraintSplittingTime();
		FactorOperationResultAndTimes result = new FactorOperationResultAndTimes( timeAndResult.first,
																				  timeAndResult.second,
																				  contextSplittingTime);
		return result;
	}
	
	
	
	
	/// POSSIBLE UNARY FACTOR OPERATIONS ///////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////

	@SuppressWarnings("unused")
	private static Factor sumOutFirstHalfOfVariables(Factor factor) {
		List<? extends Variable> variablesToSumOut = getFirstHalfSubList(factor.getVariables());
		Factor result = factor.sumOut(variablesToSumOut);
		return result;
	}
	
	
	@SuppressWarnings("unused")
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
	
	
	@SuppressWarnings("unused")
	private static Factor sumOutFirstVariable(Factor factor) {
		List<? extends Variable> factorVariables = factor.getVariables();
		int indexOfFirstVariable = 0;
		List<Variable> variablesToSumOut = new ArrayList<>();
		if (factorVariables.size() > 0)
		{
			variablesToSumOut.add(factorVariables.get(indexOfFirstVariable));
		}
		Factor result = factor.sumOut(variablesToSumOut);
		return result;
	}
	
	
	@SuppressWarnings("unused")
	private static Factor sumOutLastVariable(Factor factor) {
		List<? extends Variable> factorVariables = factor.getVariables();
		int indexOfLastVariable = factorVariables.size() - 1;
		List<Variable> variablesToSumOut = new ArrayList<>();
		if (factorVariables.size() > 0)
		{
			variablesToSumOut.add(factorVariables.get(indexOfLastVariable));
		}
		Factor result = factor.sumOut(variablesToSumOut);

		return result;
	}
	
	
	
	/// PRINTING HELPER METHODS ////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////	
	
	private static void printOperationResults(List<Factor> factors, List<FactorOperationResultAndTimes> results) {		
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
	
	
	private static void printFactorAndResultingFactor(List<Factor> factors, List<FactorOperationResultAndTimes> results,
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

	private static void printOperationTimes(List<FactorOperationResultAndTimes> results, int startingIndex,
			boolean isExpressionFactor) {
		int factorIndex = startingIndex;
		if (isExpressionFactor) {
			for (int i = 1; i <= NUMBER_OF_TESTED_THEORIES; ++i) {
				print("              Theory " + i + ":  ");
				println(results.get(factorIndex).operationTime() + " ms");
				++factorIndex;
			}
		} else {
			println("      " + results.get(factorIndex).operationTime() + " ms");
		}
	}
	

	private static void printPercentageOfOperationTimesDueTo(List<FactorOperationResultAndTimes> results, long subTime) {
		println("      Percentage of Time Due To Simulated Number of Context Splittings:");
		if (includeTreeBasedExpressionFactors) {
			println("          TREE-BASED EXPRESSION FACTOR:");
			printPercentageOfOperationTimesForFactorSetDueTo(results, subTime, TREE_BASED_EXPRESSION_FACTOR_INDEX);
		}
		if (includeLinearTableExpressionFactors) {
			println("          LINEAR-TABLE EXPRESSION FACTOR:");
			printPercentageOfOperationTimesForFactorSetDueTo(results, subTime, LINEAR_TABLE_EXPRESSION_FACTOR_INDEX);
		}
	}
	
	
	private static void printPercentageOfOperationTimesForFactorSetDueTo(List<FactorOperationResultAndTimes> results, long subTime, int startingIndex) {
		int factorIndex = startingIndex;
		for (int i = 1; i <= NUMBER_OF_TESTED_THEORIES; ++i) {
			print("              Theory " + i + ":  ");
			println(Math.round(1000.0 * subTime / results.get(factorIndex).operationTime())/10.0 + "%");
			++factorIndex;
		}
	}


	private void printPercentageOfOperationTimesDueToContextSplitting(List<FactorOperationResultAndTimes> results) {
		println("      Percentage of Time Due To Actual Context Splittings:");
		if (includeTreeBasedExpressionFactors) {
			println("          TREE-BASED EXPRESSION FACTOR:");
			printPercentageOfOperationTimesForFactorSetDueToContextSplitting(results, TREE_BASED_EXPRESSION_FACTOR_INDEX);
		}
		if (includeLinearTableExpressionFactors) {
			println("          LINEAR-TABLE EXPRESSION FACTOR:");
			printPercentageOfOperationTimesForFactorSetDueToContextSplitting(results, LINEAR_TABLE_EXPRESSION_FACTOR_INDEX);
		}
		
	}
	
	private static void printPercentageOfOperationTimesForFactorSetDueToContextSplitting(List<FactorOperationResultAndTimes> results, int startingIndex) {
		int factorIndex = startingIndex;
		for (int i = 1; i <= NUMBER_OF_TESTED_THEORIES; ++i) {
			print("              Theory " + i + ":  ");
			println(Math.round(1000.0 * results.get(factorIndex).contextSplittingTime() / results.get(factorIndex).operationTime())/10.0 + "%");
			++factorIndex;
		}
	}
	
	
	private static void printResultingFactor(List<FactorOperationResultAndTimes> results, int index) {
		println("                  result:   " + results.get(index).result());
	}
	
	private void printSimulatedContextSplittingTime(long contextSplittingTime) {
		println("      Simulated Context Splitting Time:  " + contextSplittingTime + " ms");
	}
	
	
	private void printActualContextSplittingTime(List<FactorOperationResultAndTimes> results) {
		println("      Actual Context Splitting Times:");
		if (includeTreeBasedExpressionFactors) {
			println("          TREE-BASED EXPRESSION FACTOR:");
			printContextSplittingTimes(results, TREE_BASED_EXPRESSION_FACTOR_INDEX);
		}
		if (includeLinearTableExpressionFactors) {
			println("          LINEAR-TABLE EXPRESSION FACTOR:");
			printContextSplittingTimes(results, LINEAR_TABLE_EXPRESSION_FACTOR_INDEX);
		}
		
	}
	
	
	private void printContextSplittingTimes(List<FactorOperationResultAndTimes> results, int startingIndex) {
		int factorIndex = startingIndex;
		for (int i = 1; i <= NUMBER_OF_TESTED_THEORIES; ++i) {
			print("              Theory " + i + ":  ");
			println(results.get(factorIndex).contextSplittingTime() + " ms");
			++factorIndex;
		}
	}



	private static void printVerboseMessage() {
		String message = verbose ?	"  Verbose mode ON."
								 :	"  Verbose mode OFF.";
		println(message);
	}	
	
	private static void printTheoriesBeingTested() {
		println("  Theories being tested:");
		for (int i = 0; i < NUMBER_OF_TESTED_THEORIES; ++i)
		{
			println("      Theory " + (i+1) + ":  " + THEORIES_TO_TEST[i]);
		}
	}
	
	private static void printContextSplittingSetting() {
		String message = compareWithSimulatedContextSplittingTimes ?	
									"  Comparing with simulated context splitting times."
								 :	"  Not Comparing with simulated context splitting times.";
		println(message);
	}
	
	
	
	
	/// TEST OPERATION ESTIMATOR ///////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static long estimateTimeForNextVariableCount(ArrayList<FactorOperationResultAndTimes> operationResultsAndTimes) {
		
		long timeTakenForCurrentVariable = Collections.max(operationResultsAndTimes, COMPARATOR_OF_TEST_RESULTS_BY_TEST_TIME).operationTime();
		double timeForIncrementedNumberOfVariables = timeTakenForCurrentVariable * cardinalityOfVariables;
		
		return (long) timeForIncrementedNumberOfVariables;
	}
	
	private static long estimateTimeForNextCardinality(int currentCardinality, ArrayList<FactorOperationResultAndTimes> operationResultsAndTimes) {
		
		long timeTakenForCurrentCardinality = Collections.max(operationResultsAndTimes, COMPARATOR_OF_TEST_RESULTS_BY_TEST_TIME).operationTime();
		double timePerFactorParameter = timeTakenForCurrentCardinality / Math.pow(currentCardinality, numberOfVariablesPerFactor);
		double timeForIncrementedVariableCardinality = timePerFactorParameter*Math.pow(currentCardinality + 1, numberOfVariablesPerFactor);
		
		return (long) timeForIncrementedVariableCardinality;
	}
	
	
	
	
	/// REPEATERS //////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public static <T> T repeatNtimes(NullaryFunction<T> procedure, int n) {
		int i = 0;
		for (; i < n - 1; ++i) {
			procedure.apply();
		}
		
		return procedure.apply();
	}
	
	public static void repeatNtimes(Runnable procedure, int n) {
		int i = 0;
		for (; i < n; ++i) {
			ConstraintSplitting.resetCounter();
			explanationBlock("Test # ", i, code( () -> {
				procedure.run();
			}));
		}
	}
	
	
}


