package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.randomgeneration.tablefactornetwork.RandomTableFactorNetworkGenerator.generateRandomTableFactorNetwork;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.base.Pair.pair;

import java.util.List;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering.MinFillEliminationOrdering;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.AbstractTestsOnBatchOfFactorNetworks;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.tablefactorconverter.NDArraySolver;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration.ConfigurationForTestsOnRandomTableFactorNetworks;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration.LargestProblems;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.randomtablefactornetworks.configuration.SmallProblems;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

class TestsOnRandomTableFactorNetworks extends AbstractTestsOnBatchOfFactorNetworks {

	@Test
	void test() {
		new TestsOnRandomTableFactorNetworks().run(new SmallProblems());
	}

	public static void main(String[] args) {
		new TestsOnRandomTableFactorNetworks().run(new LargestProblems());
	}

	@Override
	protected List<Pair<String,BinaryFunction<Variable,FactorNetwork,Factor>>> makeAlgorithms() {
		return list( 
				pair("VE_MI_AL", new VariableEliminationSolver(new MinFillEliminationOrdering()))
				,
				pair("VE_MI_ND", new NDArraySolver(new VariableEliminationSolver(new MinFillEliminationOrdering())))
				//,
				// pair("VE_DC", new VariableEliminationSolver(new DontCareEliminationOrdering())),
//				pair("  EBP_AL", new ArrayListSolver(new ExactBPSolver())),
//				pair("  EBP_ND", new NDArraySolver(new ExactBPSolver()))
		);
	}

	////////////////// DETAIL
	
	@Override
	protected NullaryFunction<Pair<Variable, FactorNetwork>> makeProblemGenerator() {
		return () -> {
			TableFactorNetwork factorNetwork = generateRandomTableFactorNetwork(getConfiguration(), getConfiguration().getRandom());
			Variable query = getFirst(factorNetwork.getFactors()).getVariables().get(0);
			return pair(query, factorNetwork);
		};
	}

	@Override
	protected ConfigurationForTestsOnRandomTableFactorNetworks getConfiguration() {
		return (ConfigurationForTestsOnRandomTableFactorNetworks) super.getConfiguration();
	}
}
