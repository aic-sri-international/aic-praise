package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.cutsetconditioning.core;

import static com.sri.ai.util.Util.collectToList;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.function.Predicate;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.eager.core.AbstractExactBPNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.livesets.api.LiveSet;
import com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet;

/**
 * This is probably a temporary code. I am using it to test CutSetConditioning Vs Exact BP. 
 * The latter is obviously faster than the former, but it might be worth it to see how much faster.
 * 
 * I did copy much of the code, which is extremely wrong. TODO refactor
 *  
 * @author gabriel
 *
 * @param <RootType>
 * @param <SubRootType>
 */
public abstract class AbstractCutsetConditioning<RootType, SubRootType> extends AbstractExactBPNode<RootType, SubRootType>{
	
	protected LinkedHashSet<Variable> cutSet;

	protected AbstractCutsetConditioning(RootType root, 
			SubRootType parent, 
			LiveSet<Factor> excludedFactors,
			RedirectingLiveSet<Factor> includedFactors,
			FactorNetwork factorNetwork,
			Predicate<Variable> isParameterPredicate,
			LinkedHashSet<Variable> cutSet) {
		super(root, parent, excludedFactors, includedFactors, factorNetwork, isParameterPredicate);
		this.cutSet = cutSet;
	}

	@Override
	public List<? extends Variable> getSummedOutVariables(Collection<? extends Variable> allFreeVariablesInSummand) {
		List<? extends Variable> variablesToBeSummedOut = collectToList(allFreeVariablesInSummand, n -> ! isFreeVariableOrCutSet((Variable) n));
		return variablesToBeSummedOut;
	}

	private boolean isFreeVariableOrCutSet(Variable n) {
		return this.isFreeVariable(n) || isInCutSet(n);
	}

	private boolean isInCutSet(Variable n) {
		return cutSet.contains(n);
	}
}
