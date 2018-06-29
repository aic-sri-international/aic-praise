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

public class CutSetConditioningNodeFromVariableToFactor extends AbstractCutsetConditioning<Variable, Factor>  {
	
	protected CutSetConditioningNodeFromVariableToFactor(
			Variable root, 
			Factor parent, 
			LiveSet<Factor> excludedFactors, 
			RedirectingLiveSet<Factor> includedFactors, 
			FactorNetwork factorNetwork, 
			Predicate<Variable> isParameterPredicate,
			LinkedHashSet<Variable> cutSet) {
		
		super(root, parent, excludedFactors, includedFactors, factorNetwork, isParameterPredicate, cutSet);
	}

	
	@Override
	public Factor function(List<Factor> incomingMessages) {	
		if(this.isGeneratingCycle()) {
			Variable v = this.getRoot();
			cutSet.add(v);
		}
		
		return super.function(incomingMessages);
	}
	
	private boolean isGeneratingCycle() {
		Variable v =  this.getRoot();
		Factor parent = this.getParent();
		boolean result = excludedFactors.thereIsAnElementSatisfying(f -> ((!parent.equals(f)) && f.contains(v)));
		return result;
	}
	
	//
	
	@Override
	protected ExactBPNode<Factor,Variable> makeSubExactBP(Factor subRoot, LiveSet<Factor> subExcludedFactors, RedirectingLiveSet<Factor> subIncludedFactors) {
		return new CutSetConditioningNodeFromFactorToVariable(subRoot, getRoot(), subExcludedFactors, subIncludedFactors, factorNetwork, isParameterPredicate,cutSet);
	}
	
	@Override
	protected ArrayList<? extends Factor> makeSubsRoots() {
		ArrayList<? extends Factor> result = collectToArrayList(getRootNeighbors(), n -> ! excludedFactors.contains((Factor)n));
		return result;
	}

	private Collection<? extends Factor> getRootNeighbors() {
		return getFactorNetwork().getNeighbors(getRoot());
	}

	@Override
	public List<Factor> getFactorsAtRoot() {
		return list();
	}

	@Override
	public Variable getMessageVariable() {
		return getRoot();
	}
}

