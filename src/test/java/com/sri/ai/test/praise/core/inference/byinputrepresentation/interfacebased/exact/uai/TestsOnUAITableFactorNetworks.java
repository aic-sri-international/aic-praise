package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.uai;

import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromuaitofactors.FromUAIModelToTableFactors.fromUAIModelToTableFactorNetwork;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.base.Pair.pair;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBPSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering.MinFillEliminationOrdering;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.parsing.UAIModelReader;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactorNetwork;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.base.AbstractTestsOnBatchOfFactorNetworks;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.uai.configuration.ConfigurationForTestsOnUAITableFactorNetworks;
import com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exact.uai.configuration.SingleTestUAIFile;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;

class TestsOnUAITableFactorNetworks extends AbstractTestsOnBatchOfFactorNetworks {

	///////////////// USER INTERFACE
	
	private static final String UAI_FILE_RESOURCE_PATH = "/UAITests/grid10x10.f10.uai";

	@Test
	void test() {
		new TestsOnUAITableFactorNetworks().run(new SingleTestUAIFile());
	}

	public static void main(String[] args) {
		new TestsOnUAITableFactorNetworks().run(new SingleTestUAIFile());
	}

	////////////////// ABSTRACT METHODS IMPLEMENTATION
	
	@Override
	protected ArrayList<Pair<String, BinaryFunction<Variable,FactorNetwork,Factor>>> makeAlgorithms() {
		return arrayList( 
				pair("VE_MI", new VariableEliminationSolver(new MinFillEliminationOrdering())),
				// pair("VE_DC", new VariableEliminationSolver(new DontCareEliminationOrdering())),
				pair("  EBP", new ExactBPSolver())
		);
	}

	@Override
	protected NullaryFunction<Pair<Variable, FactorNetwork>> makeProblemGenerator() {
		return () -> {
			String uaiFileResourcePath = UAI_FILE_RESOURCE_PATH;
			TableFactorNetwork factorNetwork = readTableFactorNetwork(uaiFileResourcePath);
			Variable query = getFirst(factorNetwork.getFactors()).getVariables().get(0);
			return pair(query, factorNetwork);
		};
	}

	////////////////// DETAIL
	
	@Override
	protected ConfigurationForTestsOnUAITableFactorNetworks getConfiguration() {
		return (ConfigurationForTestsOnUAITableFactorNetworks) super.getConfiguration();
	}

	private TableFactorNetwork readTableFactorNetwork(String uaiFileResourcePath) throws Error {
		TableFactorNetwork factorNetwork; 
		try {
			InputStream resourceStream = TestsOnUAITableFactorNetworks.class.getResourceAsStream(uaiFileResourcePath);
			InputStreamReader resourceReader = new InputStreamReader(resourceStream);
			UAIModel uaiModel = UAIModelReader.read(resourceReader);
			factorNetwork = fromUAIModelToTableFactorNetwork(uaiModel);
		} catch (IOException e) {
			throw new Error("Could not read UAI file " + uaiFileResourcePath);
		}
		return factorNetwork;
	}
}
