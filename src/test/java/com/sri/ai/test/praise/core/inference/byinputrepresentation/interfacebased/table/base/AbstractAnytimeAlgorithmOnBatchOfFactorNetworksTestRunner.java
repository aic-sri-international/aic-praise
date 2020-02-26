package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base;

import static com.sri.ai.util.Util.compareNumbersComponentWise;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.setDifference;

import java.util.ArrayList;
import java.util.Iterator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.Simplex;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration.ConfigurationForBatchOfFactorNetworksTest;
import com.sri.ai.util.Timer;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.computation.anytime.api.Approximation;
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
<Configuration extends ConfigurationForBatchOfFactorNetworksTest<Iterator<Approximation<Factor>>>> 
extends AbstractBatchOfFactorNetworksTestRunner<Iterator<Approximation<Factor>>, Configuration> {

	protected AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner(Configuration configuration) {
		super(configuration);
	}

	@Override
	protected void beforeExecution(String algorithmName, BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> algorithm, Variable query, FactorNetwork factorNetwork) {
		ThreadExplanationLogger.setIsActive(false);
		println("Running " + algorithmName + "...");
	}

	@Override
	protected Pair<Iterator<Approximation<Factor>>, Long> afterExecution(String algorithmName, BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> algorithm, Variable query, FactorNetwork factorNetwork, Pair<Iterator<Approximation<Factor>>, Long> resultAndTime) {
		var realResultAndTime = Timer.getResultAndTime(() ->  iterate(resultAndTime.first, algorithmName, algorithm, query, factorNetwork));
		resultAndTime.first = iterator(realResultAndTime.first);
		resultAndTime.second = realResultAndTime.second;
		ThreadExplanationLogger.setIsActive(false);
		println("Done running  " + algorithmName + " to completion. Time: " + resultAndTime.second + " ms.");
		println();
		return resultAndTime;
	}

	private Approximation<Factor> iterate(Iterator<Approximation<Factor>> anytimeIterator, String algorithmName, BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> algorithm, Variable query, FactorNetwork factorNetwork) {
		Approximation<Factor> current = null;
		while (anytimeIterator.hasNext()) {
			//println();
			current = anytimeIterator.next();
			AtomicPolytope atomicPolytope = ((Polytope) current).getEquivalentAtomicPolytopeOn(query);
			
			if (atomicPolytope.getFreeVariables().size() > 1) {
				println("AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner: Final polytope has variable other than query: " + Util.removeNonDestructively(atomicPolytope.getFreeVariables(), query));
			}
			
			if (atomicPolytope instanceof Simplex) {
				println("Simplex bound");
			}
			else {
				FunctionConvexHull hull = getFunctionConvexHull(atomicPolytope, query); 

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
	protected void compareResults(ArrayList<Pair<Iterator<Approximation<Factor>>, Long>> resultsAndTimes, Variable query, FactorNetwork factorNetwork) {
		for (int i = 0; i != getAlgorithms().size() - 1; i++) {
			var name1 = getAlgorithms().get(i).first;
			var name2 = getAlgorithms().get(i + 1).first;
			var resultAndTime1 = resultsAndTimes.get(i);
			var resultAndTime2 = resultsAndTimes.get(i + 1);
			var finalPolytope1 = getFunctionConvexHull(resultAndTime1.first.next(), query);
			var finalPolytope2 = getFunctionConvexHull(resultAndTime2.first.next(), query);
			var array1 = ((TableFactor) finalPolytope1.getFactor()).getEntries();
			var array2 = ((TableFactor) finalPolytope2.getFactor()).getEntries();
			println("Comparing " + name1 + " and " + name2 + "...");
			println(name1 + ": " + array1);
			println(name2 + ": " + array2);
			compareNumbersComponentWise(array1, array2, 0.001);
		}
		println();
	}

	private FunctionConvexHull getFunctionConvexHull(Approximation<Factor> current, Variable query) {
		AtomicPolytope atomicPolytope = ((Polytope) current).getEquivalentAtomicPolytopeOn(query);
		return (FunctionConvexHull) atomicPolytope;
	}

}