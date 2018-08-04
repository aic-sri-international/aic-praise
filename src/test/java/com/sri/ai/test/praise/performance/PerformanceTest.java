package com.sri.ai.test.praise.performance;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorMaker.makeRandomTableFactor;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.getFirstHalfSubList;
import static com.sri.ai.util.Util.getLastHalfSubList;
import static com.sri.ai.util.Util.print;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Timer.timeAndGetResult;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorMaker.TableFactorSpecs;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Random;


import org.junit.Test;
import com.sri.ai.util.base.Pair;
import com.google.common.base.Function;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.FromTableToExpressionFactorConverter;
import com.sri.ai.util.base.NullaryFunction;



public class PerformanceTest {

	private static final Theory THEORY = new DifferenceArithmeticTheory(false, true);
	
	private static final int NUMBEROFFACTORTYPES = 3;
	private static final int TABLEFACTORINDEX = 0;
	private static final int TREEBASEDEXPRESSIONFACTORINDEX = 1;
	private static final int LINEARTABLEEXPRESSIONFACTORINDEX = 2;
	
	private static final Function<Integer, String> fromVariableIndexToName = i -> "X" + i;
	
	private static final Random random = new Random();
	private static final FromTableToExpressionFactorConverter fromTableToExpressionFactorConverter = new FromTableToExpressionFactorConverter(THEORY);

	
	
	//////////////////////////////////////////////////////////////
	// GLOBAL TEST SETTINGS  /////////////////////////////////////
	//////////////////////////////////////////////////////////////
	
	private static final boolean verbose = true;
	
	private static final boolean includeTables = true;
	private static final boolean includeTreeBasedExpressions = true;
	private static final boolean includeLinearTableExpressions = true;
	
	private static final int numberOfVariablesPerFactor = 4;
	private static final int cardinalityOfEachVariable = 4;
	private static final double minimumPotential = 1.0;
	private static final double maximumPotential = 1.0;
	
	Function<Factor,Factor> unaryFactorOperation = (Factor f) -> sumOutFirstHalfOfVariables(f);
	//possible functions:	sumOutFirstHalfOfVariables(Factor f), sumOutFirstHalfOfVariables(Factor f), 
	//						sumOutFirstVariable(Factor f), sumOutLastVariable(Factor f), (Factor f).normalize()
	
	///////////////////////////////////////////////////////////////
	
	
	private static final TableFactorSpecs globalTableFactorSpecs = new TableFactorSpecs( 
																fill(numberOfVariablesPerFactor, cardinalityOfEachVariable), //ArrayList of variable cardinalities
																minimumPotential,
																maximumPotential);
	

	@Test
	public void testExpressionFactorPrintOut() {
		
		TableFactorSpecs tableFactorSpecs = new TableFactorSpecs(globalTableFactorSpecs);
		TableFactor tableFactor = makeRandomTableFactor(tableFactorSpecs, fromVariableIndexToName, random);
		ExpressionFactor treeExpressionFactor = fromTableToExpressionFactorConverter.convert(tableFactor, true);
		ExpressionFactor linearExpressionFactor = fromTableToExpressionFactorConverter.convert(tableFactor, false);
		
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
	
	
	
	
	@Test
	public void varyingNumberOfVariables() {
		
		verboseMessage(verbose);
		
		int initialNumberOfVariables = 			1 	;
		int finalNumberOfVariables = 
				includeLinearTableExpressions? 	5 
			    : includeTreeBasedExpressions? 	7
											 :  12	;
		
		
		TableFactorSpecs factorSpecs = new TableFactorSpecs(globalTableFactorSpecs);
		for (int numberOfVariables = initialNumberOfVariables; numberOfVariables <= finalNumberOfVariables; ++numberOfVariables) {
			
			factorSpecs.cardinalities = fill(numberOfVariables, cardinalityOfEachVariable);

			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);

			ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

			print("For operating on factors with " + numberOfVariables + " variables");
			printTimes(factors, opeartionResultsAndTimes);
		}
		println();
	}
	
	
	
	
	@Test
	public void varyingCardinalityOfVariables() {
		
		verboseMessage(verbose);
		
		int initialVariableCardinality = 		1	;
		int finalVariableCardinality = 
				includeLinearTableExpressions? 	6 
			    : includeTreeBasedExpressions? 	7
											 :  20	;
		
		
		TableFactorSpecs factorSpecs = new TableFactorSpecs(globalTableFactorSpecs);
		for (int cardinality = initialVariableCardinality; cardinality <= finalVariableCardinality; ++cardinality) {
			
			factorSpecs.cardinalities = fill(numberOfVariablesPerFactor, cardinality);
			
			List<Factor> factors = constructEquivalentRandomFactors(factorSpecs);

			ArrayList<FactorOperationResultAndTime> opeartionResultsAndTimes = recordTimesForFactorOperation(unaryFactorOperation, factors);

			print("For variables with cardinality of " + cardinality);
			printTimes(factors, opeartionResultsAndTimes);
		}
		println();
	}
	
	
	
	
	//@Test
	public void testDifferentExpressionFactorRepresentationsOfATableFactor()
	{
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		// FACTORS TO TEST  ///////////////////////////////////////////////////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////////////////////////////////////////

		// TableFactor  (tablefactor)
		TableVariable V1 = new TableVariable("V1", 2);
		TableVariable V2 = new TableVariable("V2", 2);
		TableVariable V3 = new TableVariable("V3", 2);
		TableVariable V4 = new TableVariable("V4", 2);
		
		TableFactor tablefactor = new TableFactor(arrayList(V1,V2,V3,V4), 1.);
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
		
		
		
		
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		// LISTS OF VARIABLES TO BE SUMMED OUT (AS ARRAYLISTS) TO TEST  ///////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		
		// tablefactor variables to be summed out
		List<? extends Variable> tablefactorVariables = expressionfactor1.getVariables();
		List<? extends Variable> tablefactorVariablesToBeSummedOut = new ArrayList<>(tablefactorVariables);
		tablefactorVariablesToBeSummedOut.remove(tablefactorVariablesToBeSummedOut.size()-1);  //remove V4 from list	
		
		// expressionfactor1 variables to be summed out
		List<? extends Variable> expressionfactor1Variables = expressionfactor1.getVariables();
		List<? extends Variable> expressionfactor1VariablesToBeSummedOut = new ArrayList<>(expressionfactor1Variables);
		expressionfactor1VariablesToBeSummedOut.remove(expressionfactor1VariablesToBeSummedOut.size()-1);  //remove V4 from list
		
		// expressionfactor2 variables to be summed out
		List<? extends Variable> expressionfactor2Variables = expressionfactor2.getVariables();
		List<? extends Variable> expressionfactor2VariablesToBeSummedOut = new ArrayList<>(expressionfactor2Variables);
		expressionfactor2VariablesToBeSummedOut.remove(expressionfactor2VariablesToBeSummedOut.size()-1);  //remove V4 from list	
		
		// expressionfactor3 variables to be summed out
		List<? extends Variable> expressionfactor3Variables = expressionfactor3.getVariables();
		List<? extends Variable> expressionfactor3VariablesToBeSummedOut = new ArrayList<>(expressionfactor3Variables);
		expressionfactor3VariablesToBeSummedOut.remove(expressionfactor3VariablesToBeSummedOut.size()-1);  //remove V4 from list	
		
		// expressionfactor4 variables to be summed out
		List<? extends Variable> expressionfactor4Variables = expressionfactor4.getVariables();
		List<? extends Variable> expressionfactor4VariablesToBeSummedOut = new ArrayList<>(expressionfactor4Variables);
		expressionfactor4VariablesToBeSummedOut.remove(expressionfactor4VariablesToBeSummedOut.size()-1);  //remove V4 from list	
		
		
		
		
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
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
		
		
		
		
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
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
// HELPERS ////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static class FactorOperationResultAndTime{
		public Pair<Factor,Long> resultAndTime;
		
		public FactorOperationResultAndTime(Pair<Factor,Long> resultAndTime)
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
	
	private static class FactorOperationResultAndTimeComparitor implements Comparator<FactorOperationResultAndTime>{
	     
		public int compare(FactorOperationResultAndTime resultA, FactorOperationResultAndTime resultB)
	    {
	        return resultA.time().compareTo(resultB.time());
	    }
	}
	
	private static void printTimes(List<Factor> factors, List<FactorOperationResultAndTime> results)
	{		
		if (verbose) {
			println();
			    println("  Random table factor:                       " + factors.get(TABLEFACTORINDEX));
			if (includeTables) {
				printResultingFactor(results,TABLEFACTORINDEX);
			}
			if (includeTreeBasedExpressions) {
				println("  Equivalent tree-based expression factor:   " + factors.get(TREEBASEDEXPRESSIONFACTORINDEX));
				printResultingFactor(results,TREEBASEDEXPRESSIONFACTORINDEX);
			}
			if (includeLinearTableExpressions) {
				println("  Equivalent linear-table expression factor: " + factors.get(LINEARTABLEEXPRESSIONFACTORINDEX));
				printResultingFactor(results,LINEARTABLEEXPRESSIONFACTORINDEX);
			}
			println("  -------------  Time for operation  ------------");
			if (includeTables) {
				println("  Table representation:                      " + results.get(TABLEFACTORINDEX).time() + " ms");
			}
			if (includeTreeBasedExpressions) {
				println("  Tree-Based Expression representation:      " + results.get(TREEBASEDEXPRESSIONFACTORINDEX).time() + " ms");
			}
			if (includeLinearTableExpressions) {
				println("  Linear-Table Expression representation:    " + results.get(LINEARTABLEEXPRESSIONFACTORINDEX).time() + " ms");
			}
			println();
		}
		else {
			if (includeTables) {
				print(",  table: " + results.get(TABLEFACTORINDEX).time() + " ms");
			}
			if (includeTreeBasedExpressions) {
				print(",  linear expression: " + results.get(TREEBASEDEXPRESSIONFACTORINDEX).time() + " ms");
			}
			if (includeLinearTableExpressions) {
				print(",  tree-based expression: " + results.get(LINEARTABLEEXPRESSIONFACTORINDEX).time() + " ms");
			}
			println();
		}
	}
	
	private static void printResultingFactor(List<FactorOperationResultAndTime> results, int index) {
		println("      operation result:                      " + results.get(index).result());
	}
	
	private static ArrayList<FactorOperationResultAndTime> recordTimesForFactorOperation(Function<Factor,Factor> unaryFactorOperation, List<Factor> factors)
	{
		ArrayList<FactorOperationResultAndTime> operationTimes = new ArrayList<>(NUMBEROFFACTORTYPES);
		
		operationTimes.add(includeTables? 
								timeFactorOperation(() -> unaryFactorOperation.apply(factors.get(TABLEFACTORINDEX)))  				  :  null);
		operationTimes.add(includeTreeBasedExpressions? 
								timeFactorOperation(() -> unaryFactorOperation.apply(factors.get(TREEBASEDEXPRESSIONFACTORINDEX)))    :  null);
		operationTimes.add(includeLinearTableExpressions? 
								timeFactorOperation(() -> unaryFactorOperation.apply(factors.get(LINEARTABLEEXPRESSIONFACTORINDEX)))  :  null);
		
		return operationTimes;
	}

	private static void verboseMessage(boolean verbose) {
		if (verbose) {
			println("Verbose mode on (set local variable in test for disabling it)");
		}
		else {
			println("Verbose mode off (set local variable in test for enabling it)");
		}
		println();
	}
	
	private List<Factor> constructEquivalentRandomFactors(TableFactorSpecs factorSpecs)
	{
		TableFactor tableFactor = makeRandomTableFactor(factorSpecs, fromVariableIndexToName, random);
		
		ArrayList<Factor> factors = new ArrayList<>(NUMBEROFFACTORTYPES);
		
		factors.add(tableFactor);
		factors.add(includeTreeBasedExpressions ? fromTableToExpressionFactorConverter.convert(tableFactor, true) : null);
		factors.add(includeLinearTableExpressions ? fromTableToExpressionFactorConverter.convert(tableFactor, false) : null);
		
		return factors;
	}

	private static FactorOperationResultAndTime timeFactorOperation(NullaryFunction<Factor> opeartion) {
		FactorOperationResultAndTime result = new FactorOperationResultAndTime( timeAndGetResult(() -> opeartion.apply()) );
		return result;
	}

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
	
	
	
	public static <T> T repeatNtimes(NullaryFunction<T> procedure, int N) {
		int i = 0;
		for(; i < N-1; ++i)
		{
			procedure.apply();
		}
		
		return procedure.apply();
	}
	
}


