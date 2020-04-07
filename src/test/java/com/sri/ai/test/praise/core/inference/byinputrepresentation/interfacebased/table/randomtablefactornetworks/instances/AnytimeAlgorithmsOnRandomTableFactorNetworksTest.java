package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.instances;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.pair;

import java.util.Iterator;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.AnytimeExactBPSolver;
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
					//pair("EBP_AL", new FromExactToAnytimeSolver(new ExactBPSolver())), // TODO: not working, not sure why
					pair("Anytime EBP", new AnytimeExactBPSolver())
					);

	@Test
	void test() {
		new AnytimeAlgorithmsOnRandomTableFactorNetworksTestRunner(new LargeProblems<>(algorithms)).run();
	}

	public static void main(String[] args) {
		new AnytimeAlgorithmsOnRandomTableFactorNetworksTestRunner(new LargestProblems<>(algorithms)).run();
	}

}
