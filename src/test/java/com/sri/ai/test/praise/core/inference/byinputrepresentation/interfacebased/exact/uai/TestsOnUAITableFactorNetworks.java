package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.uai;

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
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.tablefactorconverter.ArrayListSolver;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.tablefactorconverter.NDArraySolver;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.uai.configuration.ConfigurationForTestOnUAIFile;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

class TestsOnUAITableFactorNetworks extends AbstractUAITableFactorNetworksTestRunner {

	@Override
	protected List<Pair<String,BinaryFunction<Variable,FactorNetwork,Factor>>> makeAlgorithms() {
		return arrayList( 
				pair("VE_MI_AL", new ArrayListSolver(new VariableEliminationSolver(new MinFillEliminationOrdering())))
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
	}

	@Test
	void test() {
		new TestsOnUAITableFactorNetworks().run(
				new ConfigurationForTestOnUAIFile(
						/* numberOfTests = */ 1, 
						"/UAITests/grid10x10.f10.uai"));
	}

	public static void main(String[] args) {
		new TestsOnUAITableFactorNetworks().run(
				new ConfigurationForTestOnUAIFile(
						/* numberOfTests = */ 1, 
						"/UAITests/BN_1.uai"));
	}

}
