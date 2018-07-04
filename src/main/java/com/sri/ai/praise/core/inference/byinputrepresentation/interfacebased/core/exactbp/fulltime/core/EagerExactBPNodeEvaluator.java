package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core;

import java.util.List;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.computation.treecomputation.api.EagerTreeComputationEvaluator;

public class EagerExactBPNodeEvaluator implements EagerTreeComputationEvaluator<Factor> {

	private Function<List<? extends Factor>, Factor> computeProductOfFactorsAtRootAndIncomingMessages;
	private Function<List<? extends Variable>, List<? extends Variable>> getSummedOutVariables;
	private BinaryFunction<List<? extends Variable>, Factor, Factor> sumOut;
	
	public EagerExactBPNodeEvaluator(
			Function<List<? extends Factor>, Factor> computeProductOfFactorsAtRootAndIncomingMessages,
			Function<List<? extends Variable>, List<? extends Variable>> getSummedOutVariables,
			BinaryFunction<List<? extends Variable>, Factor, Factor> sumOut) {
		
		super();
		this.computeProductOfFactorsAtRootAndIncomingMessages = computeProductOfFactorsAtRootAndIncomingMessages;
		this.getSummedOutVariables = getSummedOutVariables;
		this.sumOut = sumOut;
	}

	@Override
	public Factor function(List<Factor> incomingMessages) {
		Factor product = computeProductOfFactorsAtRootAndIncomingMessages.apply(incomingMessages);
		List<? extends Variable> allFreeVariablesInProduct = product.getVariables();
		List<? extends Variable> variablesToBeSummedOut = getSummedOutVariables.apply(allFreeVariablesInProduct);
		Factor result = sumOut.apply(variablesToBeSummedOut, product);
		return result;
	}

}
