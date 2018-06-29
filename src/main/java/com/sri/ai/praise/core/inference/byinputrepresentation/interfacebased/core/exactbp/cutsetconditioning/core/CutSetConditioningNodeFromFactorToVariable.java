package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.cutsetconditioning.core;

import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.function.Predicate;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.eager.api.ExactBPNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.livesets.api.LiveSet;
import com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet;

public class CutSetConditioningNodeFromFactorToVariable extends AbstractCutsetConditioning<Factor, Variable>  {
	protected CutSetConditioningNodeFromFactorToVariable(
			Factor root, 
			Variable parent, 
			LiveSet<Factor> excludedFactors, 
			RedirectingLiveSet<Factor> includedFactors, 
			FactorNetwork model, 
			Predicate<Variable> isParameterPredicate,
			LinkedHashSet<Variable> cutSet) {
		
		super(root, parent, excludedFactors, includedFactors, model, isParameterPredicate,cutSet);
	}

	@Override
	protected ExactBPNode<Variable,Factor> makeSubExactBP(Variable subRoot, LiveSet<Factor> subExcludedFactors, RedirectingLiveSet<Factor> subIncludedFactors) {
		return new CutSetConditioningNodeFromVariableToFactor(subRoot, getRoot(), subExcludedFactors, subIncludedFactors, factorNetwork, isParameterPredicate,cutSet);
	}

	@Override
	protected ArrayList<? extends Variable> makeSubsRoots() {
		ArrayList<? extends Variable> result = collectToArrayList(getRootNeighbors(), n -> ! n.equals(parent));
		return result;
	}

	protected Collection<? extends Variable> getRootNeighbors() {
		return getFactorNetwork().getNeighbors(getRoot());
	}

	@Override
	public List<Factor> getFactorsAtRoot() {
		return list(getRoot());
	}

	@Override
	public Variable getMessageVariable() {
		return getParent();
	}
}
