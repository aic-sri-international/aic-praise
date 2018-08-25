package com.sri.ai.test.praise.performance;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorMaker.makeRandomTableFactor;
import static com.sri.ai.util.Timer.timeAndGetResult;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.getFirstHalfSubList;
import static com.sri.ai.util.Util.getLastHalfSubList;
import static com.sri.ai.util.Util.print;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlockToFile;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Random;

import org.junit.Test;

import com.google.common.base.Function;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.tester.ContextSplittingTester;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorSpecs;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.FromTableToExpressionFactorConverter;
import com.sri.ai.util.base.BinaryFunction;
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

	private static final boolean verbose = true;

	private static final int timeLimitPerOperation = 120000;	// how long (ms) you are willing to wait for a factor operation to complete

	private static final boolean includeTables = true;
	private static final boolean includeTreeBasedExpressions = true;
	private static final boolean includeLinearTableExpressions = false;

	private static final int numberOfVariablesPerFactor = 1;
	private static final int cardinalityOfVariables = 2;
	private static final double minimumPotential = 1.0;
	private static final double maximumPotential = 5.0;
	private static final boolean integerIncrements = true;

	Function<Factor, Factor> unaryFactorOperation = (Factor f) -> sumOutAllVariables(f);
	// possible functions:	sumOutFirstHalfOfVariables(Factor f), sumOutLastHalfOfVariables(Factor f), sumOutAllVariables(Factor f), 
	//						sumOutFirstVariable(Factor f), sumOutLastVariable(Factor f)

	BinaryFunction<Factor, Factor, Factor> binaryFactorOperation = (Factor A, Factor B) -> A.multiply(B);
	// possible functions:	A.multiply(B), B.multiply(A)

	///////////////////////////////////////////////////////////////

			
			
	// OTHER GLOBAL CONSTANTS
	private static final Theory THEORY = new DifferenceArithmeticTheory(false, true);
			
	private static final int NUMBER_OF_SUPPORTED_FACTOR_TYPES = 3;  	// TableFactor, ExpressionFactor expressed as tree, ExpressionFactor expressed as linear table
	private static final int TABLE_FACTOR_INDEX = 0; 					// index of list holding TableFactor
	private static final int TREE_BASED_EXPRESSION_FACTOR_INDEX = 1;  	// index of list holding ExpressionFactor expressed as a tree
	private static final int LINEAR_TABLE_EXPRESSION_FACTOR_INDEX = 2;	// index of list holding ExpressionFactor expressed as a linear table
				
	private static final Function<Integer, String> FROM_VARIABLE_INDEX_TO_NAME = i -> "X" + i;
			
	private static final Random RANDOM = new Random();
	private static final FromTableToExpressionFactorConverter FROM_TABLE_TO_EXPRESSION_FACTOR_CONVERTER = new FromTableToExpressionFactorConverter(THEORY);
	
	
	private static final RandomTableFactorSpecs GLOBAL_TABLE_FACTOR_SPECS = 
			new RandomTableFactorSpecs( 
					fill(numberOfVariablesPerFactor, cardinalityOfVariables), // ArrayList of variable cardinalities
					minimumPotential, maximumPotential, integerIncrements);

	private static final FactorOperationResultAndTimeComparator TEST_RESULT_TIME_COMPARATOR = new FactorOperationResultAndTimeComparator();
	
	
	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// JUNIT TESTS ////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Test
	public void singleRunForUnaryFactorOperation() {
		
		ExplanationConfiguration.WHETHER_EXPLANATION_LOGGERS_ARE_ACTIVE_BY_DEFAULT = false;
		
		explanationBlockToFile("explanation.txt", "Perfomance test of unary operation", code( () -> {

			println("===============================================================================================\n");
			println("Testing UNARY OPERATION");
			println("  number of variables = " + numberOfVariablesPerFactor);
			println("  variable cardinality = " + cardinalityOfVariables);
			verboseMessage(verbose);

			RandomTableFactorSpecs factorSpecs = new RandomTableFactorSpecs(GLOBAL_TABLE_FACTOR_SPECS);

			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);

			ArrayList<FactorOperationResultAndTime> operationResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

			print("    total operation time");
			printOperationTimes(factors, operationResultsAndTimes);

			println();

		}));
	}
	
	
	
	//@Test
	public void varyingNumberOfVariablesForUnaryFactorOperationComparedWithContextSplittingTime() {
		
		println("===============================================================================================\n");
		println("Testing UNARY OPERATION based on NUMBER OF VARIABLES and comparing to CONTEXT SPLITTING");
		println("  variable cardinality = " + cardinalityOfVariables);
		println();

		RandomTableFactorSpecs factorSpecs = new RandomTableFactorSpecs(GLOBAL_TABLE_FACTOR_SPECS);
		ContextSplittingTester contextSplittingTest;
		
		long contextSplittingTime = -1;
		
		// STARTING VARIABLE NUMBER
		int numberOfVariables = 1;
		ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes;

		do {
			
			factorSpecs.cardinalities = fill(numberOfVariables, cardinalityOfVariables);
			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);
			
			contextSplittingTest = new ContextSplittingTester(numberOfVariables, cardinalityOfVariables, false, THEORY); // false <-- focus on recording overall time
			
			opeartionResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);
			println("|| " + numberOfVariables + " variables ||");
			print("    total operation time");
			printOperationTimes(factors, opeartionResultsAndTimes);
			
			contextSplittingTime = contextSplittingTest.performContextSplittingTest();
			println("    context splitting time,  tree-based expression: " + contextSplittingTime + " ms");
			
			printPercentageOfOperationTimeDueTo(opeartionResultsAndTimes, contextSplittingTime);
			
			println();
			
		} while (estimateTimeForNextVariableCount(numberOfVariables++, opeartionResultsAndTimes) < timeLimitPerOperation);
		
		println();
	}
	
	
	
	
	//@Test
	public void varyingCardinalityOfVariablesForUnaryFactorOperationComparedWithContextSplittingTime() {
		
		println("===============================================================================================\n");
		println("Testing UNARY OPERATION based on CARDINALITY OF VARIABLES and comparing to CONTEXT SPLITTING");
		println("  number of variables = " + numberOfVariablesPerFactor);
		println();

		RandomTableFactorSpecs factorSpecs = new RandomTableFactorSpecs(GLOBAL_TABLE_FACTOR_SPECS);
		ContextSplittingTester contextSplittingTest;
		
		long contextSplittingTime = -1;
		
		// STARTING VARIABLE NUMBER
		int cardinality = 1;
		ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes;

		do {
			
			factorSpecs.cardinalities = fill(numberOfVariablesPerFactor, cardinality);
			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);
			
			contextSplittingTest = new ContextSplittingTester(numberOfVariablesPerFactor, cardinality, false, THEORY); // false <-- focus on recording overall time
			
			opeartionResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);
			println("|| " + "variables with cardinality of " + cardinality + " ||");
			print("    total operation time");
			printOperationTimes(factors, opeartionResultsAndTimes);
			
			contextSplittingTime = contextSplittingTest.performContextSplittingTest();
			println("    context splitting time,  tree-based expression: " + contextSplittingTime + " ms");
			
			printPercentageOfOperationTimeDueTo(opeartionResultsAndTimes, contextSplittingTime);
			
			println();
			
		} while (estimateTimeForNextCardinality(cardinality++, opeartionResultsAndTimes) < timeLimitPerOperation);
		
		println();
	}
	
	
	
	//@Test
	public void varyingNumberOfVariablesForUnaryFactorOperation() {
		
		println("===============================================================================================\n");
		println("Testing UNARY OPERATION based on NUMBER OF VARIABLES");
		println("  variable cardinality = " + cardinalityOfVariables);
		verboseMessage(verbose);

		RandomTableFactorSpecs factorSpecs = new RandomTableFactorSpecs(GLOBAL_TABLE_FACTOR_SPECS);
		
		// STARTING VARIABLE NUMBER
		int numberOfVariables = 1;
		ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes;

		do {
			
			factorSpecs.cardinalities = fill(numberOfVariables, cardinalityOfVariables);

			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);

			opeartionResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

			print("|| " + numberOfVariables + " variables ||");
			printOperationTimes(factors, opeartionResultsAndTimes);
			
		} while (estimateTimeForNextVariableCount(numberOfVariables++, opeartionResultsAndTimes) < timeLimitPerOperation);
		
		println();
	}
	
	
	
	//@Test
	public void varyingCardinalityOfVariablesForUnaryFactorOperation() {
		
		println("===============================================================================================\n");
		println("Testing UNARY OPERATION based on VARIABLE CARDINALITY");
		println("  number of variables = " + numberOfVariablesPerFactor);
		verboseMessage(verbose);
		
		RandomTableFactorSpecs factorSpecs = new RandomTableFactorSpecs(GLOBAL_TABLE_FACTOR_SPECS);
		
		// STARTING CARDINALITY
		int cardinality = 1;
		ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes;
		do {
			
			factorSpecs.cardinalities = fill(numberOfVariablesPerFactor, cardinality);
			
			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);

			opeartionResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

			print("|| cardinality " + cardinality + " ||");
			printOperationTimes(factors, opeartionResultsAndTimes);
			
		} while (estimateTimeForNextCardinality(cardinality++, opeartionResultsAndTimes) < timeLimitPerOperation);
		
		println();
	}
	
	
	
	// TODO:  resolve why cannot multiply to factor with more variables
	// TODO:  create loop with automation of loop termination for varying number of variables
	//@Test
	public void varyingNumberOfVariablesForBinaryFactorOperation() {
		
		println("===============================================================================================\n");
		println("Testing BINARY OPERATION based on NUMBER OF VARIABLES");
		println("  number of variables = " + numberOfVariablesPerFactor);
		verboseMessage(verbose);
		
		RandomTableFactorSpecs factorASpecs = new RandomTableFactorSpecs(GLOBAL_TABLE_FACTOR_SPECS);
		List<Factor> factorArepresentations = constructEquivalentRandomFactors(factorASpecs);
		List<Factor> factorBrepresentations = constructEquivalentRandomFactors(factorASpecs);

		ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes = recordTimesForFactorOperation(binaryFactorOperation, factorArepresentations, factorBrepresentations);

		print("For binary operation on factors with " + numberOfVariablesPerFactor + " variables");
		printOperationTimes(factorArepresentations, factorBrepresentations, opeartionResultsAndTimes);
	
	println();
		
	}
	
	
	
	//@Test
	public void repeatTestFxnNTimes() {
		
		final int N = 4;
		
		repeatNtimes(() -> varyingCardinalityOfVariablesForUnaryFactorOperation(), N);
	}
	
	
	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ADDITINAL TESTING METHODS //////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	//@Test
	public void testExpressionFactorPrintOut() {
		
		RandomTableFactorSpecs tableFactorSpecs = new RandomTableFactorSpecs(GLOBAL_TABLE_FACTOR_SPECS);
		TableFactor tableFactor = makeRandomTableFactor(tableFactorSpecs, FROM_VARIABLE_INDEX_TO_NAME, RANDOM);
		ExpressionFactor treeExpressionFactor = FROM_TABLE_TO_EXPRESSION_FACTOR_CONVERTER.convert(tableFactor, true);
		ExpressionFactor linearExpressionFactor = FROM_TABLE_TO_EXPRESSION_FACTOR_CONVERTER.convert(tableFactor, false);
		
		println("test");
		println("-------------------------------------------------------------------------");
		println("TableFactor");
		println("    factor:            " + tableFactor);
		println("    variable list:     " + tableFactor.getVariables());
		println("    operation result:  " + unaryFactorOperation.apply(tableFactor));
		println("TreeExpressionFactor");
		println("    factor:            " + treeExpressionFactor);
		println("    variable list:     " + treeExpressionFactor.getVariables());
		println("    operation result:  " + unaryFactorOperation.apply(treeExpressionFactor));
		println("LinearExpressionFactor");
		println("    factor:            " + linearExpressionFactor);
		println("    variable list:     " + linearExpressionFactor.getVariables());
		println("    operation result:  " + unaryFactorOperation.apply(linearExpressionFactor));
		println("-------------------------------------------------------------------------");
	}
	
	
	
	//@Test
	public void testDifferentExpressionFactorRepresentationsOfATableFactor()
	{
		// FACTORS TO TEST  ///////////////////////////////////////////////////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////////////////////////////////////////

		// TableFactor  (tablefactor)
		TableVariable V1 = new TableVariable("V1", 2);
		TableVariable V2 = new TableVariable("V2", 2);
		TableVariable V3 = new TableVariable("V3", 2);
		TableVariable V4 = new TableVariable("V4", 2);
		
		TableFactor tablefactor = new TableFactor(arrayList(V1, V2, V3, V4), 1.);
		tablefactor.setName("tablefactor");
		


		// Converter and context for creating ExpressionFactor
		FromTableToExpressionFactorConverter fromTableToExpressionFactorConverter = new FromTableToExpressionFactorConverter(THEORY);
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(
				"V1", "0..1",
				"V2", "0..1",
				"V3", "0..1",
				"V4", "0..1");
		
		
		// ExpressionFactor that uses if/else binary branching  (expressionfactor1)
		ExpressionFactor expressionfactor1 = new DefaultExpressionFactor(parse(""
															+ "if V1 = 0 then "
																+ "if V2 = 0 then "
																	+ "if V3 = 0 then "
																		+ "if V4 = 0 then 1 else 1 "
																	+ "else "
																		+ "if V4 = 0 then 1 else 1 "
																+ "else "
																	+ "if V3 = 0 then "
																		+ "if V4 = 0 then 1 else 1 "
																	+ "else "
																		+ "if V4 = 0 then 1 else 1 "
															+ "else "
																+ "if V2 = 0 then "
																	+ "if V3 = 0 then "
																		+ "if V4 = 0 then 1 else 1 "
																	+ "else "
																		+ "if V4 = 0 then 1 else 1 "
																+ "else "
																	+ "if V3 = 0 then "
																		+ "if V4 = 0 then 1 else 1 "
																	+ "else "
																		+ "if V4 = 0 then 1 else 1"), context);



		ExpressionFactor expressionfactor2 = fromTableToExpressionFactorConverter.convert(tablefactor, true);
		
		
		// ExpressionFactor that is effectively a linear table  (expressionfactor3)
		ExpressionFactor expressionfactor3 = new DefaultExpressionFactor(parse(""
															+ "if (V1 = 0) and (V2 = 0) and (V3 = 0) and (V4 = 0) then 1 "
															+ "else if (V1 = 0) and (V2 = 0) and (V3 = 0) and (V4 = 1) then 1 "
															+ "else if (V1 = 0) and (V2 = 0) and (V3 = 1) and (V4 = 0) then 1 "
															+ "else if (V1 = 0) and (V2 = 0) and (V3 = 1) and (V4 = 1) then 1 "
															+ "else if (V1 = 0) and (V2 = 1) and (V3 = 0) and (V4 = 0) then 1 "
															+ "else if (V1 = 0) and (V2 = 1) and (V3 = 0) and (V4 = 1) then 1 "
															+ "else if (V1 = 0) and (V2 = 1) and (V3 = 1) and (V4 = 0) then 1 "
															+ "else if (V1 = 0) and (V2 = 1) and (V3 = 1) and (V4 = 1) then 1 "
															+ "else if (V1 = 1) and (V2 = 0) and (V3 = 0) and (V4 = 0) then 1 "
															+ "else if (V1 = 1) and (V2 = 0) and (V3 = 0) and (V4 = 1) then 1 "
															+ "else if (V1 = 1) and (V2 = 0) and (V3 = 1) and (V4 = 0) then 1 "
															+ "else if (V1 = 1) and (V2 = 0) and (V3 = 1) and (V4 = 1) then 1 "
															+ "else if (V1 = 1) and (V2 = 1) and (V3 = 0) and (V4 = 0) then 1 "
															+ "else if (V1 = 1) and (V2 = 1) and (V3 = 0) and (V4 = 1) then 1 "
															+ "else if (V1 = 1) and (V2 = 1) and (V3 = 1) and (V4 = 0) then 1 "
															+ "else 1"), context);
		
		// ExpressionFactor that is effectively a linear table  (expressionfactor2)
		ExpressionFactor expressionfactor4 = fromTableToExpressionFactorConverter.convert(tablefactor, false);
		
		
		
		// LISTS OF VARIABLES TO BE SUMMED OUT (AS ARRAYLISTS) TO TEST  ///////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		
		// tablefactor variables to be summed out
		List<? extends Variable> tablefactorVariables = expressionfactor1.getVariables();
		List<? extends Variable> tablefactorVariablesToBeSummedOut = new ArrayList<>(tablefactorVariables);
		tablefactorVariablesToBeSummedOut.remove(tablefactorVariablesToBeSummedOut.size()-1);  // remove V4 from list	
		
		// expressionfactor1 variables to be summed out
		List<? extends Variable> expressionfactor1Variables = expressionfactor1.getVariables();
		List<? extends Variable> expressionfactor1VariablesToBeSummedOut = new ArrayList<>(expressionfactor1Variables);
		expressionfactor1VariablesToBeSummedOut.remove(expressionfactor1VariablesToBeSummedOut.size()-1);  // remove V4 from list
		
		// expressionfactor2 variables to be summed out
		List<? extends Variable> expressionfactor2Variables = expressionfactor2.getVariables();
		List<? extends Variable> expressionfactor2VariablesToBeSummedOut = new ArrayList<>(expressionfactor2Variables);
		expressionfactor2VariablesToBeSummedOut.remove(expressionfactor2VariablesToBeSummedOut.size()-1);  // remove V4 from list	
		
		// expressionfactor3 variables to be summed out
		List<? extends Variable> expressionfactor3Variables = expressionfactor3.getVariables();
		List<? extends Variable> expressionfactor3VariablesToBeSummedOut = new ArrayList<>(expressionfactor3Variables);
		expressionfactor3VariablesToBeSummedOut.remove(expressionfactor3VariablesToBeSummedOut.size()-1);  // remove V4 from list	
		
		// expressionfactor4 variables to be summed out
		List<? extends Variable> expressionfactor4Variables = expressionfactor4.getVariables();
		List<? extends Variable> expressionfactor4VariablesToBeSummedOut = new ArrayList<>(expressionfactor4Variables);
		expressionfactor4VariablesToBeSummedOut.remove(expressionfactor4VariablesToBeSummedOut.size()-1);  // remove V4 from list	
		
		
		
		// TIME SUMMING OUT VARIABLES, REPEATING PROCESS N TIMES  /////////////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		
		final int N = 1;
		
		FactorOperationResultAndTime tableFactorResult =
				new FactorOperationResultAndTime(timeAndGetResult(() -> repeatNtimes(() -> tablefactor.sumOut(tablefactorVariablesToBeSummedOut), N)));
		
		FactorOperationResultAndTime expressionFactor1Result =
				new FactorOperationResultAndTime(timeAndGetResult(() -> repeatNtimes(() -> expressionfactor1.sumOut(expressionfactor1VariablesToBeSummedOut), N)));
		
		FactorOperationResultAndTime expressionFactor2Result =
				new FactorOperationResultAndTime(timeAndGetResult(() -> repeatNtimes(() -> expressionfactor2.sumOut(expressionfactor2VariablesToBeSummedOut), N)));
		
		FactorOperationResultAndTime expressionFactor3Result =
				new FactorOperationResultAndTime(timeAndGetResult(() -> repeatNtimes(() -> expressionfactor3.sumOut(expressionfactor3VariablesToBeSummedOut), N)));
		
		FactorOperationResultAndTime expressionFactor4Result =
				new FactorOperationResultAndTime(timeAndGetResult(() -> repeatNtimes(() -> expressionfactor4.sumOut(expressionfactor4VariablesToBeSummedOut), N)));
		
		
		
		// PRINTED REPORT  ////////////////////////////////////////////////////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////////////////////////////////////////

		println("INITIAL FACTORS");
		println("===============");
		println(tablefactor);
		println("expressionfactor1: " + expressionfactor1);
		println("expressionfactor2: " + expressionfactor2);
		println("expressionfactor3: " + expressionfactor3);
		println("expressionfactor4: " + expressionfactor4);
		println();
		
		println("SUMMING OUT TIMES");
		println("=================");
		println("tablefactor SumOut time: " + tableFactorResult.time()+"ms");
			println("\t" + tableFactorResult.result());
		println("expressionfactor1 SumOut time: " + expressionFactor1Result.time()+"ms");
			println("\tphi: " + expressionFactor1Result.result());
		println("expressionfactor2 SumOut time: " + expressionFactor2Result.time()+"ms");
			println("\tphi: " + expressionFactor2Result.result());
		println("expressionfactor3 SumOut time: " + expressionFactor3Result.time()+"ms");
			println("\tphi: " + expressionFactor3Result.result());
		println("expressionfactor4 SumOut time: " + expressionFactor4Result.time()+"ms");
			println("\tphi: " + expressionFactor4Result.result());
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
	
	private List<Factor> constructEquivalentRandomFactors(RandomTableFactorSpecs factorSpecs)
	{
		TableFactor tableFactor = makeRandomTableFactor(factorSpecs, FROM_VARIABLE_INDEX_TO_NAME, RANDOM);
		
		ArrayList<Factor> factors = new ArrayList<>(NUMBER_OF_SUPPORTED_FACTOR_TYPES);
		
		factors.add(tableFactor);
		factors.add(includeTreeBasedExpressions ? FROM_TABLE_TO_EXPRESSION_FACTOR_CONVERTER.convert(tableFactor, true) : null);
		factors.add(includeLinearTableExpressions ? FROM_TABLE_TO_EXPRESSION_FACTOR_CONVERTER.convert(tableFactor, false) : null);
		
		return factors;
	}
	
	
	
	
	/// RECORDING RESULTS FROM FACTOR OPERATIONS ////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static ArrayList<FactorOperationResultAndTime> recordTimesForFactorOperation(Function<Factor, Factor> unaryFactorOperation, List<Factor> factors)
	{
		ArrayList<FactorOperationResultAndTime> operationTimes = new ArrayList<>(NUMBER_OF_SUPPORTED_FACTOR_TYPES);
		
		operationTimes.add(includeTables? 
								timeFactorOperation(() -> unaryFactorOperation.apply(factors.get(TABLE_FACTOR_INDEX)))  				  :  null);
		operationTimes.add(includeTreeBasedExpressions? 
								timeFactorOperation(() -> unaryFactorOperation.apply(factors.get(TREE_BASED_EXPRESSION_FACTOR_INDEX)))    :  null);
		operationTimes.add(includeLinearTableExpressions? 
								timeFactorOperation(() -> unaryFactorOperation.apply(factors.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX)))  :  null);
		
		return operationTimes;
	}
	
	private static ArrayList<FactorOperationResultAndTime> recordTimesForFactorOperation(BinaryFunction<Factor, Factor, Factor> binaryFactorOperation, 
																															List<Factor> A, List<Factor> B)
	{
		ArrayList<FactorOperationResultAndTime> operationTimes = new ArrayList<>(NUMBER_OF_SUPPORTED_FACTOR_TYPES);
		
		operationTimes.add(includeTables? 
								timeFactorOperation(() -> binaryFactorOperation.apply(A.get(TABLE_FACTOR_INDEX), B.get(TABLE_FACTOR_INDEX)))  				  :  null);
		operationTimes.add(includeTreeBasedExpressions? 
								timeFactorOperation(() -> binaryFactorOperation.apply(A.get(TREE_BASED_EXPRESSION_FACTOR_INDEX), B.get(TREE_BASED_EXPRESSION_FACTOR_INDEX)))    :  null);
		operationTimes.add(includeLinearTableExpressions? 
								timeFactorOperation(() -> binaryFactorOperation.apply(A.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX), B.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX)))  :  null);
		
		return operationTimes;
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
	
	
//	CURRENTLY, EXPRESSION FACTORS CANNOT BE NORMALIZED
//	private static Factor normalize(Factor factor) {
//		Factor result = factor.normalize();
//		return result;
//	}
	
	
	
	
	/// PRINTING HELPER METHODS ////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static void verboseMessage(boolean verbose) {
		if (verbose) {
			println("  Verbose mode on (set global variable in " + PerformanceTest.class.getSimpleName() + " for disabling it)");
		}
		else {
			println("  Verbose mode off (set global variable in " + PerformanceTest.class.getSimpleName() + " for enabling it)");
		}
		println();
	}
	
	
	private static void printOperationTimes(List<Factor> factors, List<FactorOperationResultAndTime> results)
	{		
		if (verbose) {
			println();
			    println("     Random table factor:                         " + factors.get(TABLE_FACTOR_INDEX));
			if (includeTables) {
				printResultingFactor(results, TABLE_FACTOR_INDEX);
			}
			if (includeTreeBasedExpressions) {
				println("     Equivalent tree-based expression factor:     " + factors.get(TREE_BASED_EXPRESSION_FACTOR_INDEX));
				printResultingFactor(results, TREE_BASED_EXPRESSION_FACTOR_INDEX);
			}
			if (includeLinearTableExpressions) {
				println("     Equivalent linear-table expression factor:   " + factors.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX));
				printResultingFactor(results, LINEAR_TABLE_EXPRESSION_FACTOR_INDEX);
			}
			println("     -------------  Time for operation  ------------");
			if (includeTables) {
				println("     Table representation:                        " + results.get(TABLE_FACTOR_INDEX).time() + " ms");
			}
			if (includeTreeBasedExpressions) {
				println("     Tree-Based Expression representation:        " + results.get(TREE_BASED_EXPRESSION_FACTOR_INDEX).time() + " ms");
			}
			if (includeLinearTableExpressions) {
				println("     Linear-Table Expression representation:      " + results.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX).time() + " ms");
			}
			println();
		}
		else {
			if (includeTables) {
				print(",  table: " + results.get(TABLE_FACTOR_INDEX).time() + " ms");
			}
			if (includeTreeBasedExpressions) {
				print(",  tree-based expression: " + results.get(TREE_BASED_EXPRESSION_FACTOR_INDEX).time() + " ms");
			}
			if (includeLinearTableExpressions) {
				print(",  linear expression: " + results.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX).time() + " ms");
			}
			println();
		}
	}
	
	private static void printOperationTimes(List<Factor> factorsA, List<Factor> factorsB, List<FactorOperationResultAndTime> results)
	{		
		if (verbose) {
			println();
			    println("     Random table factor A:                       " + factorsA.get(TABLE_FACTOR_INDEX));
			    println("     Random table factor B:                       " + factorsB.get(TABLE_FACTOR_INDEX));
			if (includeTables) {
				printResultingFactor(results, TABLE_FACTOR_INDEX);
			}
			if (includeTreeBasedExpressions) {
				println("     Equivalent tree-based expression factor A:   " + factorsA.get(TREE_BASED_EXPRESSION_FACTOR_INDEX));
				println("     Equivalent tree-based expression factor B:   " + factorsB.get(TREE_BASED_EXPRESSION_FACTOR_INDEX));
				printResultingFactor(results, TREE_BASED_EXPRESSION_FACTOR_INDEX);
			}
			if (includeLinearTableExpressions) {
				println("     Equivalent linear-table expression factor A: " + factorsA.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX));
				println("     Equivalent linear-table expression factor B: " + factorsB.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX));
				printResultingFactor(results, LINEAR_TABLE_EXPRESSION_FACTOR_INDEX);
			}
			println("   -------------  Time for operation  ------------");
			if (includeTables) {
				println("     Table representation:                      " + results.get(TABLE_FACTOR_INDEX).time() + " ms");
			}
			if (includeTreeBasedExpressions) {
				println("     Tree-Based Expression representation:      " + results.get(TREE_BASED_EXPRESSION_FACTOR_INDEX).time() + " ms");
			}
			if (includeLinearTableExpressions) {
				println("     Linear-Table Expression representation:    " + results.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX).time() + " ms");
			}
			println();
		}
		else {
			if (includeTables) {
				print(",  table: " + results.get(TABLE_FACTOR_INDEX).time() + " ms");
			}
			if (includeTreeBasedExpressions) {
				print(",  tree-based expression: " + results.get(TREE_BASED_EXPRESSION_FACTOR_INDEX).time() + " ms");
			}
			if (includeLinearTableExpressions) {
				print(",  linear expression: " + results.get(LINEAR_TABLE_EXPRESSION_FACTOR_INDEX).time() + " ms");
			}
			println();
		}
	}
	
	// TODO: expand to include ability to compare linear table expressions to their context splitting times as well (need to adjust ContextSplittingTester)
	private static void printPercentageOfOperationTimeDueTo(List<FactorOperationResultAndTime> results, long subTime) {
		print("    percentage of time spent in context splitting");
		if (includeTreeBasedExpressions) {
			print(", tree-based expression: "
					+ Math.round(1000.0 * subTime / results.get(TREE_BASED_EXPRESSION_FACTOR_INDEX).time())/10.0 + "%");
		}
		else		{
			println("currently only Tree Based Expression can be compared to their context splitting times");
		}
		println();
	}
	
	
	private static void printResultingFactor(List<FactorOperationResultAndTime> results, int index) {
		println("       operation result:                          " + results.get(index).result());
	}
	
	
	
	
	/// TEST OPERATION ESTIMATOR ///////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static long estimateTimeForNextVariableCount(int currentCardinality, ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes) {
		
		long timeTakenForCurrentVariable = Collections.max(opeartionResultsAndTimes, TEST_RESULT_TIME_COMPARATOR).time();
		double timeForIncrementedNumberOfVariables = timeTakenForCurrentVariable * cardinalityOfVariables;
		
		return (long) timeForIncrementedNumberOfVariables;
	}
	
	private static long estimateTimeForNextCardinality(int currentCardinality, ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes) {
		
		long timeTakenForCurrentCardinality = Collections.max(opeartionResultsAndTimes, TEST_RESULT_TIME_COMPARATOR).time();
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
			procedure.run();
		}
	}
	
	
}


