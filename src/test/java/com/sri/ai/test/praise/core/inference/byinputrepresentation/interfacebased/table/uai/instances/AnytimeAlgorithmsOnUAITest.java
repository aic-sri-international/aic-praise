package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.uai.instances;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.pair;

import java.util.Iterator;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.algorithm.AnytimeExactBP;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.convertersolverwrapper.FromExactToAnytimeSolver;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.randomtablefactornetworks.instances.configurations.Defaults;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.uai.base.AnytimeAlgorithmsOnUAITestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.table.uai.configuration.ConfigurationForUAITest;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.computation.anytime.api.Approximation;

public class AnytimeAlgorithmsOnUAITest {

	static List<Pair<String, BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>>>> algorithms =
			arrayList( 
					pair("VE", new FromExactToAnytimeSolver(new VariableEliminationSolver())),
					pair("Anytime EBP", new AnytimeExactBP())
					);

	@Test
	void test() {
		new AnytimeAlgorithmsOnUAITestRunner(
				new ConfigurationForUAITest<>(
						algorithms,
						/* numberOfRuns = */ 1,
						/* maximumComponentwiseError = */ Defaults.MAXIMUM_COMPONENTWISE_ERROR,
						"/UAITests/1akg.uai")
						// "/UAITests/BN_0.uai") // TODO: not memory-efficient enough yet
				        // "/UAITests/grid10x10.f10.uai") // TODO: not time- and memory-efficient enough yet
				).run();
	}

	public static void main(String[] args) {
		new AnytimeAlgorithmsOnUAITestRunner(
				new ConfigurationForUAITest<>(
						algorithms,
						/* numberOfRuns = */ 1, 
						/* maximumComponentwiseError = */ Defaults.MAXIMUM_COMPONENTWISE_ERROR,
						"/UAITests/BN_1.uai") // TODO: not time- and memory-efficient enough yet
				).run();
	}

}
