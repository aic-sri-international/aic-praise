package com.sri.ai.test.praise.performance;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorMaker.makeRandomTableFactor;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.getFirstHalfSubList;
import static com.sri.ai.util.Util.mapIntegersIntoArrayList;
import static com.sri.ai.util.Util.print;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;

import org.junit.Test;

import com.google.common.base.Function;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.FromTableToExpressionFactorConverter;
import com.sri.ai.util.Timer;

public class PerformanceTest {


	private static final Theory THEORY = new DifferenceArithmeticTheory(false, true);

	@Test
	public void test() {
		
		Random random = new Random(0);
		
		boolean verbose = false;
		
		boolean includeTables = true;
		boolean includeExpressions = true;
		
		int initialNumberOfVariables = 1;
		int finalNumberOfVariables = includeExpressions? 5 : 25;
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
				print("For eliminating half of " + numberOfVariables + " variables,");
				if (includeTables) {
					print(" table: " + timeForEliminatingHalfOfVariablesFromTableFactor + " ms,");
				}
				if (includeExpressions) {
					print(" expression: " + timeForEliminatingHalfOfVariablesFromExpressionFactor + " ms");
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
}
