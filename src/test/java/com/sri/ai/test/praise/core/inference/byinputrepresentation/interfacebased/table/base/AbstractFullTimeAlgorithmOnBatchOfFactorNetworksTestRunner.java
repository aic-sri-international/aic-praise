package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base;

import static com.sri.ai.util.Util.compareNumbersComponentWise;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration.ConfigurationForBatchOfFactorNetworksTest;
import com.sri.ai.util.Timer;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

/** 
 * Abstract class for running a list of "full-time" algorithms on a batch of problems,
 * where "full-time" means that the whole operation is performed in one shot, without intermediary stages,
 * and returns results which are extensions of {@link TableFactor}.
 * 
 * @author braz
 *
 */
public abstract class AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner<Configuration extends ConfigurationForBatchOfFactorNetworksTest<Factor>> 
extends AbstractBatchOfFactorNetworksTestRunner<Factor, Configuration> {

	protected AbstractFullTimeAlgorithmOnBatchOfFactorNetworksTestRunner(Configuration configuration) {
		super(configuration);
	}

	@Override
	protected Pair<Factor, Long> execute(
			String algorithmName,
			BinaryFunction<Variable, FactorNetwork, Factor> algorithm,
			Variable query,
			FactorNetwork factorNetwork) {
		
		ThreadExplanationLogger.setIsActive(algorithm instanceof VariableEliminationSolver);
		println("Running " + algorithmName + "...");
		var resultAndTime = Timer.timeAndGetResult(() -> algorithm.apply(query, factorNetwork));
		println("Done running  " + algorithmName + ". Time: " + resultAndTime.second + " ms.");
		ThreadExplanationLogger.setIsActive(false);
		println();
		return resultAndTime;
	}

	@Override
	protected void compareResults(ArrayList<Pair<Factor, Long>> resultsAndTimes) {
		for (int i = 0; i != getAlgorithms().size() - 1; i++) {
			var name1 = getAlgorithms().get(i).first;
			var name2 = getAlgorithms().get(i + 1).first;
			var resultAndTime1 = resultsAndTimes.get(i);
			var resultAndTime2 = resultsAndTimes.get(i + 1);
			var array1 = ((TableFactor) resultAndTime1.first).getEntries();
			var array2 = ((TableFactor) resultAndTime2.first).getEntries();
			println("Comparing " + name1 + " and " + name2 + "...");
			compareNumbersComponentWise(array1, array2, 0.001);
		}
		println();
	}

}