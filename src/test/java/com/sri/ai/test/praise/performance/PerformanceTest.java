package com.sri.ai.test.praise.performance;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorMaker.makeRandomTableFactor;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.getFirstHalfSubList;
import static com.sri.ai.util.Util.mapIntegersIntoArrayList;
import static com.sri.ai.util.Util.print;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.expresso.helper.Expressions.parse;

import static java.lang.Math.min;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;


import org.junit.Test;

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
import com.sri.ai.util.Timer;
import com.sri.ai.util.base.NullaryFunction;

public class PerformanceTest {


	private static final Theory THEORY = new DifferenceArithmeticTheory(false, true);

	@Test
	public void test() {
		
		Random random = new Random(0);
		
		boolean verbose = false;
		
		boolean includeTables = true;
		boolean includeExpressions = true;
		
		int initialNumberOfVariables = 1;
		int finalNumberOfVariables = includeExpressions? 4 : 9;
		int cardinalityOfAllVariables = 4;
		double minimumPotential = 0.1;
		double maximumPotential = 1.0;

		FromTableToExpressionFactorConverter fromTableToExpressionFactorConverter = new FromTableToExpressionFactorConverter(THEORY);
		
		Collection<? extends ArrayList<Integer>> cardinalitiesSet = 
				makeCardinalitiesSetByVaryingNumberOfVariables(
						initialNumberOfVariables, finalNumberOfVariables, cardinalityOfAllVariables);
		
		verboseMessage(verbose);
		
		for (ArrayList<Integer> cardinalities : cardinalitiesSet) {
			
			Function<Integer, String> fromVariableIndexToName = i -> "X" + i;
			TableFactor tableFactor = makeRandomTableFactor(cardinalities, minimumPotential, maximumPotential, fromVariableIndexToName, random);
			long timeForEliminatingHalfOfVariablesFromTableFactor = 0;
			if (includeTables) {
				timeForEliminatingHalfOfVariablesFromTableFactor = timeEliminationOfFirstHalfOfVariables(tableFactor);
			}
			
			ExpressionFactor expressionFactor = null;
			long timeForEliminatingHalfOfVariablesFromExpressionFactor = 0;
			if (includeExpressions) {
				expressionFactor = fromTableToExpressionFactorConverter.convert(tableFactor);
				timeForEliminatingHalfOfVariablesFromExpressionFactor = timeEliminationOfFirstHalfOfVariables(expressionFactor);
			}
			
			int numberOfVariables = cardinalities.size();
			
			if (verbose) {
				println("For eliminating half of " + numberOfVariables + " variables:");
				println("    Random table factor         : " + tableFactor);
				if (includeExpressions) {
					println("    Equivalent expression factor: " + expressionFactor);
				}
				println("    Time for eliminating half the variables:");
				if (includeTables) {
					println("    Table      representation: " + timeForEliminatingHalfOfVariablesFromTableFactor + " ms");
				}
				if (includeExpressions) {
					println("    Expression representation: " + timeForEliminatingHalfOfVariablesFromExpressionFactor + " ms");
				}
				println();
			}
			else {
				print("For eliminating half of " + numberOfVariables + " variables");
				if (includeTables) {
					print(", table: " + timeForEliminatingHalfOfVariablesFromTableFactor + " ms");
				}
				if (includeExpressions) {
					print(", expression: " + timeForEliminatingHalfOfVariablesFromExpressionFactor + " ms");
				}
				println();
			}
		}
	}

	private static Collection<? extends ArrayList<Integer>> makeCardinalitiesSetByVaryingNumberOfVariables(
			int initialNumberOfVariables, int finalNumberOfVariables, int cardinalityOfAllVariables) {
		
		List<ArrayList<Integer>> result = 
				mapIntegersIntoArrayList(initialNumberOfVariables, finalNumberOfVariables + 1, i -> fill(i, cardinalityOfAllVariables));
		return result;
	}

	private void verboseMessage(boolean verbose) {
		if (verbose) {
			println("Verbose mode on (set local variable in test for disabling it)");
		}
		else {
			println("Verbose mode off (set local variable in test for enabling it)");
		}
		println();
	}

	private long timeEliminationOfFirstHalfOfVariables(Factor factor) {
		long time = Timer.time(() -> eliminateFirstHalfOfVariables(factor));
		return time;
	}

	private Factor eliminateFirstHalfOfVariables(Factor factor) {
		List<? extends Variable> variablesToBeEliminated = getFirstHalfSubList(factor.getVariables());
		Factor result = factor.sumOut(variablesToBeEliminated);
		return result;
	}
	
	
	
	

	@Test
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
		
		long tablefactortime = Timer.time(() -> repeatNtimes(() -> tablefactor.sumOut(tablefactorVariablesToBeSummedOut), N));
		
		long expressionfactor1timeA = Timer.time(() -> repeatNtimes(() -> expressionfactor1.sumOut(expressionfactor1VariablesToBeSummedOut), N));
		long expressionfactor1timeB = Timer.time(() -> repeatNtimes(() -> expressionfactor1.sumOut(expressionfactor1VariablesToBeSummedOut), N));
		long expressionfactor1time = min(expressionfactor1timeA, expressionfactor1timeB);
		
		long expressionfactor2timeA = Timer.time(() -> repeatNtimes(() -> expressionfactor2.sumOut(expressionfactor2VariablesToBeSummedOut), N));
		long expressionfactor2timeB = Timer.time(() -> repeatNtimes(() -> expressionfactor2.sumOut(expressionfactor2VariablesToBeSummedOut), N));
		long expressionfactor2time = min(expressionfactor2timeA, expressionfactor2timeB);
		
		long expressionfactor3timeA = Timer.time(() -> repeatNtimes(() -> expressionfactor3.sumOut(expressionfactor3VariablesToBeSummedOut), N));
		long expressionfactor3timeB = Timer.time(() -> repeatNtimes(() -> expressionfactor3.sumOut(expressionfactor3VariablesToBeSummedOut), N));
		long expressionfactor3time = min(expressionfactor3timeA, expressionfactor3timeB);
		
		long expressionfactor4timeA = Timer.time(() -> repeatNtimes(() -> expressionfactor4.sumOut(expressionfactor4VariablesToBeSummedOut), N));
		long expressionfactor4timeB = Timer.time(() -> repeatNtimes(() -> expressionfactor4.sumOut(expressionfactor4VariablesToBeSummedOut), N));
		long expressionfactor4time = min(expressionfactor4timeA, expressionfactor4timeB);
		
		
		
		
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
		println("tablefactor SumOut time: " + tablefactortime+"ms");
			println("\t" + tablefactor.sumOut(tablefactorVariablesToBeSummedOut));
		println("expressionfactor1 SumOut time: " + expressionfactor1time+"ms");
			println("\tphi: " + expressionfactor1.sumOut(expressionfactor1VariablesToBeSummedOut));
		println("expressionfactor2 SumOut time: " + expressionfactor2time+"ms");
			println("\tphi: " + expressionfactor2.sumOut(expressionfactor2VariablesToBeSummedOut));
		println("expressionfactor3 SumOut time: " + expressionfactor3time+"ms");
			println("\tphi: " + expressionfactor3.sumOut(expressionfactor3VariablesToBeSummedOut));
		println("expressionfactor4 SumOut time: " + expressionfactor4time+"ms");
			println("\tphi: " + expressionfactor4.sumOut(expressionfactor4VariablesToBeSummedOut));
		println();	
	}
	
	
	public static <T> int repeatNtimes(NullaryFunction<T> procedure, int N) {
		int i = 0;
		for(; i < N; ++i)
		{
			procedure.apply();
		}
		return i;
	}
	
}


