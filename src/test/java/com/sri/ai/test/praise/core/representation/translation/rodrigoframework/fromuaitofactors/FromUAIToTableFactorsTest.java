package com.sri.ai.test.praise.core.representation.translation.rodrigoframework.fromuaitofactors;

import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromuaitofactors.FromUAIModelToTableFactors.fromUAIModelToTableFactorNetwork;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.println;

import java.io.IOException;
import java.io.StringReader;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.parsing.UAIModelReader;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

class FromUAIToTableFactorsTest {

	@Test
	void test() {
		try {
			UAIModel uaiModel = UAIModelReader.read(new StringReader(modelString));
			FactorNetwork network = fromUAIModelToTableFactorNetwork(uaiModel);
			Variable queryVariable = getFirst(network.getVariables());
			ExactBP solver = new ExactBP(queryVariable, network);
			Factor queryResult = solver.apply();
			println(queryResult);
		} catch (IOException e) {
			throw new Error(e);
		}
	}

	private static String modelString =
			"MARKOV\r\n" + 
			"3\r\n" + 
			"2 2 3\r\n" + 
			"3\r\n" + 
			"1 0\r\n" + 
			"2 0 1\r\n" + 
			"2 1 2\r\n" + 
			"\r\n" + 
			"2\r\n" + 
			" 0.436 0.564\r\n" + 
			"\r\n" + 
			"4\r\n" + 
			" 0.128 0.872\r\n" + 
			" 0.920 0.080\r\n" + 
			"\r\n" + 
			"6\r\n" + 
			" 0.210 0.333 0.457\r\n" + 
			" 0.811 0.000 0.189\r\n";
}
