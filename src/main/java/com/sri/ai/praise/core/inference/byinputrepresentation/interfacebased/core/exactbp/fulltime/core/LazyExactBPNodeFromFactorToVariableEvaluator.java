package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core;

import static com.sri.ai.praise.core.PRAiSEUtil.conditionOnlyIfDeterministic;
import static com.sri.ai.util.Util.findFirst;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.computation.treecomputation.core.AbstractLazyTreeComputationEvaluator;
import com.sri.ai.util.explanation.tree.DefaultExplanationTree;
import com.sri.ai.util.explanation.tree.ExplanationTree;

public class LazyExactBPNodeFromFactorToVariableEvaluator extends AbstractLazyTreeComputationEvaluator<Factor> {

	private Factor currentProduct;
	private List<ExplanationTree> explanationsOfSubs;
	
	private NullaryFunction<List<? extends Factor>> getFactorsAtRoot;
	private Function<List<? extends Variable>, List<? extends Variable>> determineVariablesToBeSummedOut;
	private BinaryFunction<List<? extends Variable>, Factor, Factor> sumOutWithBookkeeping;
	
	public LazyExactBPNodeFromFactorToVariableEvaluator(
			NullaryFunction<List<? extends Factor>> getFactorsAtRoot,
			Function<List<? extends Variable>, List<? extends Variable>> determineVariablesToBeSummedOut,
			BinaryFunction<List<? extends Variable>, Factor, Factor> sumOutWithBookkeeping) {
		
		super();
		this.getFactorsAtRoot = getFactorsAtRoot;
		this.determineVariablesToBeSummedOut = determineVariablesToBeSummedOut;
		this.sumOutWithBookkeeping = sumOutWithBookkeeping;
		this.explanationsOfSubs = list();
	}

	@SuppressWarnings("unchecked")
	protected ArrayList<? extends ExactBPNodeFromVariableToFactor> getSubs() {
		return (ArrayList<? extends ExactBPNodeFromVariableToFactor>) super.getSubs();
	}

	@Override
	protected void reset() {
		currentProduct = Factor.multiply(getFactorsAtRoot.apply());
	}

	@Override
	protected NullaryFunction<Factor> pickNextSubToBeEvaluated() {
		NullaryFunction<Factor> result = findFirst(getSubs(), this::hasNotBeenEvaluatedAndIsRelevant); 
		return result;
	}

	private boolean hasNotBeenEvaluatedAndIsRelevant(ExactBPNodeFromVariableToFactor sub) {
		boolean result = 
				!hasAlreadyBeenEvaluated(sub) 
				&& 
				isRelevantForCurrentResult(sub);
		return result;
	}

	private boolean isRelevantForCurrentResult(ExactBPNodeFromVariableToFactor subFromVariableToFactor) {
		Variable variable = subFromVariableToFactor.getRoot();
		boolean result = currentProduct.getVariables().contains(variable);
		return result;
	}

	@Override
	protected void simplifyFunctionWithValueForSub(NullaryFunction<Factor> sub, Factor subValue) {
		currentProduct = currentProduct.multiply(subValue);
		explanationsOfSubs.add(subValue.getExplanation());
	}

	@Override
	protected Factor finishComputingResultOnceAllRelevantSubComputationsHaveBeenTakenIntoAccount() {
		List<? extends Variable> variablesToBeSummedOut = determineVariablesToBeSummedOut.apply(currentProduct.getVariables());
		Factor result = sumOutWithBookkeeping.apply(variablesToBeSummedOut, currentProduct);
		result.setExplanation(makeExplanation(result));
		return result;
	}

	private ExplanationTree makeExplanation(Factor factor) {
		ExplanationTree result = 
				new DefaultExplanationTree("<" + conditionOnlyIfDeterministic(factor) + ">, from " + getFirst(getFactorsAtRoot.apply()) + " given:", explanationsOfSubs);
		return result;
	}
}
