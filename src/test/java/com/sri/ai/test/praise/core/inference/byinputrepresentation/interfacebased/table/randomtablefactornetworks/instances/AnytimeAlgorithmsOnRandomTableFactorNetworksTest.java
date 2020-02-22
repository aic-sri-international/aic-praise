package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.instances;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.pair;

import java.util.Iterator;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.AnytimeExactBPSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.convertersolverwrapper.FromExactToAnytimeSolver;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.base.AnytimeAlgorithmsOnRandomTableFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.instances.configurations.LargeProblems;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.instances.configurations.LargestProblems;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.computation.anytime.api.Approximation;

class AnytimeAlgorithmsOnRandomTableFactorNetworksTest {

	static List<Pair<String, BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>>>> algorithms =
			list( 
					pair("VE", new FromExactToAnytimeSolver(new VariableEliminationSolver())),
					pair("Anytime EBP", new AnytimeExactBPSolver())
//					pair("VE_MI_AL", new VariableEliminationSolver(new MinFillEliminationOrdering()))
//					,
//					pair("VE_MI_ND", new NDArraySolver(new VariableEliminationSolver(new MinFillEliminationOrdering())))
//					//,
//					// pair("VE_DC", new VariableEliminationSolver(new DontCareEliminationOrdering())),
//					//				pair("  EBP_AL", new ArrayListSolver(new ExactBPSolver())),
//					//				pair("  EBP_ND", new NDArraySolver(new ExactBPSolver()))
					);

	@Test
	void test() {
		new AnytimeAlgorithmsOnRandomTableFactorNetworksTestRunner(new LargeProblems<>(algorithms)).run();
	}

	public static void main(String[] args) {
		new AnytimeAlgorithmsOnRandomTableFactorNetworksTestRunner(new LargestProblems<>(algorithms)).run();
	}

}
