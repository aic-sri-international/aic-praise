package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core;

import static com.sri.ai.praise.core.PRAiSEUtil.conditionOnlyIfDeterministic;
import static com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor.multiply;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.collect.NestedIterator.nestedIterator;

import java.util.Iterator;
import java.util.List;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.DefaultExplanationTree;
import com.sri.ai.util.ExplanationTree;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.computation.treecomputation.api.EagerTreeComputationEvaluator;

public class EagerExactBPNodeEvaluator implements EagerTreeComputationEvaluator<Factor> {

	private NullaryFunction<List<? extends Factor>> getFactorsAtRoot;
	private Function<List<? extends Variable>, List<? extends Variable>> determineVariablesToBeSummedOut;
	private BinaryFunction<List<? extends Variable>, Factor, Factor> sumOutWithBookkeeping;
	
	public EagerExactBPNodeEvaluator(
			NullaryFunction<List<? extends Factor>> getFactorsAtRoot,
			Function<List<? extends Variable>, List<? extends Variable>> determineVariablesToBeSummedOut,
			BinaryFunction<List<? extends Variable>, Factor, Factor> sumOut) {
		
		super();
		this.getFactorsAtRoot = getFactorsAtRoot;
		this.determineVariablesToBeSummedOut = determineVariablesToBeSummedOut;
		this.sumOutWithBookkeeping = sumOut;
	}

	@Override
	public Factor function(List<Factor> incomingMessages) {
		Factor product = computeProductOfFactorsAtRootAndIncomingMessages(incomingMessages);
		List<? extends Variable> variablesToBeSummedOut = determineVariablesToBeSummedOut.apply(product.getVariables());
		Factor result = sumOutWithBookkeeping.apply(variablesToBeSummedOut, product);
		result.setExplanation(makeExplanation(result, incomingMessages));
		return result;
	}

	/**
	 * Returns the product of given incoming messages and the factor at root, if there is any. 
	 * @param incomingMessages
	 * @return
	 */
	private Factor computeProductOfFactorsAtRootAndIncomingMessages(List<? extends Factor> incomingMessages) {
		Iterator<Factor> allFactors = nestedIterator(getFactorsAtRoot.apply(), incomingMessages);
		Factor result = multiply(allFactors);
		return result;
	}

	private ExplanationTree makeExplanation(Factor factor, List<? extends Factor> incomingMessages) {
		List<? extends ExplanationTree> explanationsOfSubs = mapIntoList(incomingMessages, Factor::getExplanation);
		ExplanationTree result;
		if (incomingMessages.size() == 0) {
			result = new DefaultExplanationTree("uniform distribution, since there is no more information on this variable");
		}
		else if (incomingMessages.size() == 1) {
			result = getFirst(explanationsOfSubs);
		}
		else {
			result = new DefaultExplanationTree("<" + conditionOnlyIfDeterministic(factor) + ">, from multiplying:", explanationsOfSubs);
		}
		return result;
	}
}
