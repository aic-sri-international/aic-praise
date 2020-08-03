package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base;

import static com.sri.ai.util.Util.actualFreeMemory;
import static com.sri.ai.util.Util.assertEqualsComponentWise;
import static com.sri.ai.util.Util.humanReadableByMagnitude;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.removeNonDestructively;

import java.util.ArrayList;
import java.util.Iterator;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node.api.AnytimeExactBPNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.Simplex;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.base.configuration.ConfigurationForBatchOfFactorNetworksTest;
import com.sri.ai.util.Timer;
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

	protected AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner(
			Configuration configuration) {
		
		super(configuration, "", false /* do not show result because it is an iterator */);
	}

	@Override
	protected void beforeExecution(
			String algorithmName,
			BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> algorithm,
			Variable query,
			FactorNetwork factorNetwork) {
		
		ThreadExplanationLogger.setIsActive(false);
		println("Running " + algorithmName + "...");
	}

	@Override
	protected Pair<Iterator<Approximation<Factor>>, Long> afterExecution(
			String algorithmName,
			BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> algorithm,
			Variable query,
			FactorNetwork factorNetwork,
			Pair<Iterator<Approximation<Factor>>, Long> resultAndTime) {
		
		var realResultAndTime =
				Timer.timed(
						() -> iterate(resultAndTime.first, algorithmName, algorithm, query, factorNetwork));
		resultAndTime.first = iterator(realResultAndTime.first);
		resultAndTime.second = realResultAndTime.second;
		println("Done running  " + algorithmName + " to completion. Time: " + resultAndTime.second + " ms.");
		println();
		return resultAndTime;
	}

	private Approximation<Factor> iterate(
			Iterator<Approximation<Factor>> anytimeIterator,
			String algorithmName,
			BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> algorithm,
			Variable query,
			FactorNetwork factorNetwork) {

		Polytope latestPolytopeApproximation = null;
		while (anytimeIterator.hasNext()) {
			latestPolytopeApproximation = (Polytope) anytimeIterator.next();
			checkForNonQueryVariables(latestPolytopeApproximation, query);
			println(makeStatusDescription(latestPolytopeApproximation, anytimeIterator));
			
		}
		return latestPolytopeApproximation;
	}

	private void checkForNonQueryVariables(Polytope polytope, Variable query) {
		if (polytope.getFreeVariables().size() > 1) {
			println("Warning: AbstractAnytimeAlgorithmOnBatchOfFactorNetworksTestRunner: "
					+ "Final polytope has variable other than query: "
					+ removeNonDestructively(polytope.getFreeVariables(), query)
					+ ": polytope: " + polytope);
		}
	}

	private String makeStatusDescription(Polytope polytope, Iterator<Approximation<Factor>> anytimeIterator) {
		String status = polytope instanceof Simplex? "Simplex bound" : "Bound length: " + polytope.length();
		status += ", " + actualFreeMemory()/1000000 + " MB of free memory";
		if (anytimeIterator instanceof AnytimeExactBPNode) {
			@SuppressWarnings("unchecked")
			var anytimeExactBPNode = (AnytimeExactBPNode<Variable, Factor>) anytimeIterator;
			status += ", " + humanReadableByMagnitude(anytimeExactBPNode.memory()) + " factor entries in entire tree";
		}
		return status;
	}

	@Override
	protected void compareResults(
			ArrayList<Pair<Iterator<Approximation<Factor>>, Long>> resultsAndTimes, 
			Variable query, 
			FactorNetwork factorNetwork) {
		
		var results = mapIntoArrayList(resultsAndTimes, rt -> rt.first.next());
		
		for (int i = 0; i != getAlgorithms().size() - 1; i++) {
			var name1 = getAlgorithms().get(i).first;
			var name2 = getAlgorithms().get(i + 1).first;
			var result1 = results.get(i);
			var result2 = results.get(i + 1);
			var finalPolytope1 = getFunctionConvexHull(result1, query);
			var finalPolytope2 = getFunctionConvexHull(result2, query);
			// Since final polytopes are singletons, indices are irrelevant, so we get rid of them if there are any:
			var finalFactor1 = finalPolytope1.getFactor().sumOut(finalPolytope1.getIndices()).normalizeBySummingOverThese(list(query));
			var finalFactor2 = finalPolytope2.getFactor().sumOut(finalPolytope2.getIndices()).normalizeBySummingOverThese(list(query));
			var array1 = ((TableFactor) finalFactor1).getEntries();
			var array2 = ((TableFactor) finalFactor2).getEntries();
			try {
			assertEqualsComponentWise(array1, array2, getConfiguration().getMaximumComponentwiseError());
			}
			catch (AssertionError e) {
				var message = 
						"Comparison failed between " + name1 + " and " + name2 + "...\n" +
								name1 + ": " + array1 + "\n" +
								name2 + ": " + array2;
				println("\n" + message);
				throw new AssertionError(message, e);
			}
		}
		println();
	}

	private FunctionConvexHull getFunctionConvexHull(Approximation<Factor> current, Variable query) {
		AtomicPolytope atomicPolytope = ((Polytope) current).getEquivalentAtomicPolytope();
		return (FunctionConvexHull) atomicPolytope;
	}

}