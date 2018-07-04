package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core;

import static com.sri.ai.util.Util.findFirst;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.computation.treecomputation.core.AbstractLazyTreeComputationEvaluator;

public class LazyExactBPNodeFromFactorToVariableEvaluator extends AbstractLazyTreeComputationEvaluator<Factor> {

	private Factor currentResult;
	
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
	}

	@SuppressWarnings("unchecked")
	protected ArrayList<? extends ExactBPNodeFromVariableToFactor> getSubs() {
		return (ArrayList<? extends ExactBPNodeFromVariableToFactor>) super.getSubs();
	}

	@Override
	protected void reset() {
		currentResult = Factor.multiply(getFactorsAtRoot.apply());
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
		boolean result = currentResult.getVariables().contains(variable);
		return result;
	}

	@Override
	protected void simplifyFunctionWithValueForSub(NullaryFunction<Factor> sub, Factor subValue) {
		Variable variable = getVariableOfSub(sub);
		Factor productOfCurrentResultAndMessage = currentResult.multiply(subValue);
		currentResult = productOfCurrentResultAndMessage.sumOut(list(variable));
	}

	private Variable getVariableOfSub(NullaryFunction<Factor> sub) {
		ExactBPNodeFromVariableToFactor subFromVariableToFactor = (ExactBPNodeFromVariableToFactor) sub;
		Variable variable = subFromVariableToFactor.getRoot();
		return variable;
	}

	@Override
	protected Factor finishComputingResultOnceAllRelevantSubComputationsHaveBeenTakenIntoAccount() {
		List<? extends Variable> variablesToBeSummedOut = determineVariablesToBeSummedOut.apply(currentResult.getVariables());
		Factor result = sumOutWithBookkeeping.apply(variablesToBeSummedOut, currentResult);
		return result;
	}
}
