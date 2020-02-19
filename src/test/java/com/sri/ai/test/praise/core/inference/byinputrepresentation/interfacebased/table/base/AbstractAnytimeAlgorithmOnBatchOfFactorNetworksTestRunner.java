package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.setDifference;

import java.util.ArrayList;
import java.util.Iterator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base.Simplex;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.intensional.IntensionalPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.intensional.IntensionalPolytopeUtil;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration.ConfigurationForBatchOfFactorNetworksTest;
import com.sri.ai.util.Timer;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

/** 
 * Abstract class for running a list of anytime algorithms on a batch of problems
 * which return a list of (partial result, time) pairs.
 * 
 * @author braz
 *
 */
public abstract class 
AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner
<PartialResult, Configuration extends ConfigurationForBatchOfFactorNetworksTest<Iterator<PartialResult>>> 
extends AbstractBatchOfFactorNetworksTestRunner<Iterator<PartialResult>, Configuration> {

	protected AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner(Configuration configuration) {
		super(configuration);
	}

	@Override
	protected void beforeExecution(String algorithmName, BinaryFunction<Variable, FactorNetwork, Iterator<PartialResult>> algorithm, Variable query, FactorNetwork factorNetwork) {
		ThreadExplanationLogger.setIsActive(false);
		println("Running " + algorithmName + "...");
	}

	@Override
	protected Pair<Iterator<PartialResult>, Long> afterExecution(String algorithmName, BinaryFunction<Variable, FactorNetwork, Iterator<PartialResult>> algorithm, Variable query, FactorNetwork factorNetwork, Pair<Iterator<PartialResult>, Long> resultAndTime) {
		var realResultAndTime = Timer.getResultAndTime(() ->  iterate(resultAndTime.first, algorithmName, algorithm, query, factorNetwork));
		resultAndTime.second = realResultAndTime.second;
		ThreadExplanationLogger.setIsActive(false);
		println("Done running  " + algorithmName + " to completion. Time: " + resultAndTime.second + " ms.");
		println();
		return resultAndTime;
	}

	private PartialResult iterate(Iterator<PartialResult> anytimeIterator, String algorithmName, BinaryFunction<Variable, FactorNetwork, Iterator<PartialResult>> algorithm, Variable query, FactorNetwork factorNetwork) {
		PartialResult current = null;
		while (anytimeIterator.hasNext()) {
			//println();
			current = anytimeIterator.next();
			if (current instanceof Simplex) {
				println("Simplex bound");
			}
			else {
				Polytope currentPolytope = (Polytope) current;
				IntensionalPolytope hull  = (IntensionalPolytope) IntensionalPolytopeUtil.getEquivalentAtomicPolytopeOn(query, currentPolytope);
				var normalizedHullFactor = hull.getFactor().normalize(list(query));
				var allButQuery = setDifference(normalizedHullFactor.getVariables(), list(query));
				var upperBoundPerValue = (ArrayTableFactor) normalizedHullFactor.max(allButQuery);
				var lowerBoundPerValue = (ArrayTableFactor) normalizedHullFactor.min(allButQuery);
				var upperBoundOfLastValue = upperBoundPerValue.getEntries().get(upperBoundPerValue.numberOfEntries() - 1);
				var lowerBoundOfLastValue = lowerBoundPerValue.getEntries().get(lowerBoundPerValue.numberOfEntries() - 1);
				println("Bound length: " + (upperBoundOfLastValue - lowerBoundOfLastValue));
				//println("Upper bound : " + upperBoundOfLastValue);
				//println("Lower bound : " + lowerBoundOfLastValue);
				//println("Hull        : " + hull);
				//println("Normalized hull factor: " + normalizedHullFactor);
			}
		}
		return current;
	}

	@Override
	protected void compareResults(ArrayList<Pair<Iterator<PartialResult>, Long>> resultsAndTimes) {
//		for (int i = 0; i != getAlgorithms().size() - 1; i++) {
//			var name1 = getAlgorithms().get(i).first;
//			var name2 = getAlgorithms().get(i + 1).first;
//			var resultAndTime1 = resultsAndTimes.get(i);
//			var resultAndTime2 = resultsAndTimes.get(i + 1);
//			var anytimeResults1 = resultAndTime1.first;
//			var anytimeResults2 = resultAndTime2.first;
//			println("Comparing " + name1 + " and " + name2 + "...");
//			// compareNumbersComponentWise(array1, array2, 0.001);
//		}
//		println();
	}

}