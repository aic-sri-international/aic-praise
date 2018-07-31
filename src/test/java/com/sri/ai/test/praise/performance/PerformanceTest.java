package com.sri.ai.test.praise.performance;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorMaker.makeRandomTableFactor;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.getFirstHalfSubList;
import static com.sri.ai.util.Util.mapIntegersIntoArrayList;
import static com.sri.ai.util.Util.print;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.expresso.helper.Expressions.parse;

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
		boolean includeExpressions = false;
		
		int initialNumberOfVariables = 1;
		int finalNumberOfVariables = includeExpressions? 4 : 12;
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
		

		// ExpressionFactor that uses if/else binary branching  (expressionfactor1)
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(
				"V1", "1..2",
				"V2", "1..2",
				"V3", "1..2",
				"V4", "1..2");
		
		ExpressionFactor expressionfactor1 = new DefaultExpressionFactor(parse(""
															+ "if V1 = 1 then "
																+ "if V2 = 1 then "
																	+ "if V3 = 1 then "
																		+ "if V4 = 1 then 1 else 1 "
																	+ "else "
																		+ "if V4 = 1 then 1 else 1 "
																+ "else "
																	+ "if V3 = 1 then "
																		+ "if V4 = 1 then 1 else 1 "
																	+ "else "
																		+ "if V4 = 1 then 1 else 1 "
															+ "else "
																+ "if V2 = 1 then "
																	+ "if V3 = 1 then "
																		+ "if V4 = 1 then 1 else 1 "
																	+ "else "
																		+ "if V4 = 1 then 1 else 1 "
																+ "else "
																	+ "if V3 = 1 then "
																		+ "if V4 = 1 then 1 else 1 "
																	+ "else "
																		+ "if V4 = 1 then 1 else 1"), context);


		// ExpressionFactor that is effectively a linear table  (expressionfactor2)
		FromTableToExpressionFactorConverter fromTableToExpressionFactorConverter = new FromTableToExpressionFactorConverter(THEORY);
		ExpressionFactor expressionfactor2 = fromTableToExpressionFactorConverter.convert(tablefactor);
		
		
		
		
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
		
		
		
		
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		// TIME SUMMING OUT VARIABLES, REPEATING PROCESS N TIMES  /////////////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		
		final int N = 1;
		
		long tablefactortime = Timer.time(() -> repeatNtimes(() -> tablefactor.sumOut(tablefactorVariablesToBeSummedOut), N));
		long expressionfactor1time = Timer.time(() -> repeatNtimes(() -> expressionfactor1.sumOut(expressionfactor1VariablesToBeSummedOut), N));
		long expressionfactor2time = Timer.time(() -> repeatNtimes(() -> expressionfactor2.sumOut(expressionfactor2VariablesToBeSummedOut), N));
		
		
		
		
		///////////////////////////////////////////////////////////////////////////////////////////////////////////
		// PRINTED REPORT  ////////////////////////////////////////////////////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////////////////////////////////////////

		println("INITIAL FACTORS");
		println("===============");
		println(tablefactor);
		println("expressionfactor1: " + expressionfactor1);
		println("expressionfactor2: " + expressionfactor2);
		println();
		
		println("SUMMING OUT TIMES");
		println("=================");
		println("tablefactor SumOut time: " + tablefactortime+"ms");
			println("\t" + tablefactor.sumOut(tablefactorVariablesToBeSummedOut));
		println("expressionfactor1 SumOut time: " + expressionfactor1time+"ms");
			println("\tphi: " + expressionfactor1.sumOut(expressionfactor1VariablesToBeSummedOut));
		println("expressionfactor2 SumOut time: " + expressionfactor2time+"ms");
			println("\tphi: " + expressionfactor2.sumOut(expressionfactor2VariablesToBeSummedOut));
		println();
	
	}
	
	
	public static <T> boolean repeatNtimes(NullaryFunction<T> procedure, int N) {
		for(int i = 0; i < N; ++i)
		{
			procedure.apply();
		}
		return true;
	}
	
}


