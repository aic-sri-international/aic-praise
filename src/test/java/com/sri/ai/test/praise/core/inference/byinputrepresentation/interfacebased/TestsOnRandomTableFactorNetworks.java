package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.helper.RandomTableFactorNetworkMaker.makeRandomTableFactorNetwork;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.repeat;
import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableElimination;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactorNetwork;
import com.sri.ai.util.Timer;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

class TestsOnRandomTableFactorNetworks {

	@Test
	void test() {
		
		// Small models:
		
		int minimumNumberOfVariables = 2;
		int maximumNumberOfVariables = 3;
		
		int minimumCardinality = 2;
		int maximumCardinality = 2;
		
		int minimumNumberOfFactors = 1;
		int maximumNumberOfFactors = 3;

		int minimumNumberOfVariablesPerFactor = 1;
		int maximumNumberOfVariablesPerFactor = 2;

		double minimumPotential = 1.0;
		double maximumPotential = 4.0;
		
//		// Larger models:
//		
//		int minimumNumberOfVariables = 10;
//		int maximumNumberOfVariables = 25;
//		
//		int minimumCardinality = 2;
//		int maximumCardinality = 2;
//		
//		int minimumNumberOfFactors = 5;
//		int maximumNumberOfFactors = 20;
//
//		int minimumNumberOfVariablesPerFactor = 3;
//		int maximumNumberOfVariablesPerFactor = 5;
//
//		double minimumPotential = 1.0;
//		double maximumPotential = 4.0;

		
		Random random = new Random();

		repeat(1000, i ->
		runTestForARandomFactorNetwork(
				i,
				minimumNumberOfVariables,
				maximumNumberOfVariables,
				minimumCardinality,
				maximumCardinality,
				minimumNumberOfFactors,
				maximumNumberOfFactors,
				minimumNumberOfVariablesPerFactor,
				maximumNumberOfVariablesPerFactor,
				minimumPotential,
				maximumPotential,
				random)
				);
		
	}

	public void runTestForARandomFactorNetwork(
			int i,
			int minimumNumberOfVariables,
			int maximumNumberOfVariables,
			int minimumCardinality,
			int maximumCardinality,
			int minimumNumberOfFactors,
			int maximumNumberOfFactors,
			int minimumNumberOfVariablesPerFactor,
			int maximumNumberOfVariablesPerFactor,
			double minimumPotential,
			double maximumPotential,
			Random random) {
		
		TableFactorNetwork factorNetwork = 
				makeRandomTableFactorNetwork(
						minimumNumberOfVariables, maximumNumberOfVariables, 
						minimumCardinality, maximumCardinality, 
						minimumNumberOfFactors, maximumNumberOfFactors, 
						minimumNumberOfVariablesPerFactor, maximumNumberOfVariablesPerFactor, 
						minimumPotential, maximumPotential, 
						random);
		
		Variable query = getFirst(factorNetwork.getFactors()).getVariables().get(0);
		
		println();
		println("********************");
		println("Test #" + i);
		println();
		println("Number of variables: " + factorNetwork.getVariables().size());
		println("Number of factors: " + factorNetwork.getFactors().size());
		println();
		
		ThreadExplanationLogger.setIsActive(false);
		println("Running VE...");
		@SuppressWarnings("unchecked") // TODO: make FactorNetwork generic so we can avoid such unsafe casts
		VariableElimination variableElimination = new VariableElimination(query, 
				new TableFactorNetwork((List<? extends TableFactor>) factorNetwork.getFactors()));
		Pair<Factor, Long> variableEliminationResult = Timer.timeAndGetResult(() -> variableElimination.apply());
		println("Done running  VE. Time: " + variableEliminationResult.second + " ms.");
		ThreadExplanationLogger.setIsActive(false);

		println();

		println("Running EBP...");
		ExactBP exactBP = new ExactBP(query, factorNetwork);
		Pair<Factor, Long> exactBPResult = Timer.timeAndGetResult(() -> exactBP.apply());
		println("Done running EBP. Time: " + exactBPResult.second + " ms.");

		println();
		println("VE : " + resultAndTimeString(variableEliminationResult));
		println("EBP: " + resultAndTimeString(exactBPResult));
		
		var variableEliminationArray = ((TableFactor) variableEliminationResult.first).getEntries();
		var exactBPArray = ((TableFactor) exactBPResult.first).getEntries();
		for (int j  = 0; j != exactBPArray.size(); j++) {
			assertEquals(variableEliminationArray.get(j).doubleValue() / exactBPArray.get(j).doubleValue(), 1.0, 0.001);
		}

		// TODO: commented out because comparison fails for some examples; must be debugged
		// I suspect this is happening when the model is disconnected; EBP may be buggy for that case
		// Generating small models with few variables makes the error more evident, which seems to corroborate that hypothesis.
	}

	public String resultAndTimeString(Pair<Factor, Long> resultAndTime) {
		return resultAndTime.first + ", " + resultAndTime.second + " ms";
	}

}
