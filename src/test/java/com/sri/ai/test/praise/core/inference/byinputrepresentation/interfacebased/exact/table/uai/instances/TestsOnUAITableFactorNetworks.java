package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.uai.instances;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.base.Pair.pair;

import java.util.List;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBPSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering.MinFillEliminationOrdering;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.convertersolverwrapper.ArrayListSolver;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.convertersolverwrapper.NDArraySolver;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.uai.runner.UAITableFactorNetworksTestRunner;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.table.uai.runner.configuration.ConfigurationForTestOnUAIFile;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

public class TestsOnUAITableFactorNetworks {

	static List<Pair<String,BinaryFunction<Variable,FactorNetwork,Factor>>> algorithms =
			arrayList( 
					pair("VE_MI_AL", new VariableEliminationSolver(new MinFillEliminationOrdering()))
					,
					//				pair("VE_MI_AL", new ArrayListSolver(new VariableEliminationSolver(new MinFillEliminationOrdering())))
					//				,
					//pair("VE_MI_ND", new VariableEliminationSolver(new MinFillEliminationOrdering()))
					//,
					//pair("VE_DC", new VariableEliminationSolver(new DontCareEliminationOrdering()))
					//,
					pair("  EBP_AL", new ArrayListSolver(new ExactBPSolver()))
					,
					pair("  EBP_ND", new NDArraySolver(new ExactBPSolver()))
					);

	@Test
	void test() {
		new UAITableFactorNetworksTestRunner<>(
				new ConfigurationForTestOnUAIFile<>(
						algorithms,
						/* numberOfRuns = */ 1, 
						"/UAITests/grid10x10.f10.uai")
				).run();
	}

	public static void main(String[] args) {
		new UAITableFactorNetworksTestRunner<>(
				new ConfigurationForTestOnUAIFile<>(
						algorithms,
						/* numberOfRuns = */ 1, 
						"/UAITests/BN_1.uai")
				).run();
	}

}
