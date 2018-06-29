package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.cutsetconditioning.core;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.livesets.core.lazy.memoryless.ExtensionalLiveSet.liveSet;
import static com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet.redirectingTo;

import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.livesets.core.lazy.memoryless.ExtensionalLiveSet;
import com.sri.ai.util.livesets.core.lazy.memoryless.RedirectingLiveSet;

public class CutSetConditioning extends CutSetConditioningNodeFromVariableToFactor {

	public CutSetConditioning(Variable query, FactorNetwork factorNetwork) {
		this(query, factorNetwork, v -> false /* default is "no uninterpreted constants" */);
	}

	public CutSetConditioning(Problem problem) {
		this(problem.getQueryVariable(), problem.getModel(), problem.getIsParameterPredicate());
	}
	
	private CutSetConditioning(Variable query, FactorNetwork factorNetwork, Predicate<Variable> isParameterPredicate) {
		super(
				query,
				makeParent(),
				makeExcludedFactors(),
				makeIncludedFactors(),
				factorNetwork,
				isParameterPredicate,
				new LinkedHashSet<Variable>());
	}

	private static Factor makeParent() {
		return null;  // there is none, as the message on the query is the final computation
	}
	
	private static ExtensionalLiveSet<Factor> makeExcludedFactors() {
		return liveSet(list()); // there is no "exterior" to this ExactBPNode, so there are no excluded factors
	}

	private static RedirectingLiveSet<Factor> makeIncludedFactors() {
		return redirectingTo(makeExcludedFactors()); // the search initially starts with no included factors having been included yet
	}
	
	@Override
	public Factor function(List<Factor> incomingMessages) {
		// Compute the message and then 
		Factor preResult =  super.function(incomingMessages);
		// Since this is the root of the tree: sum the cut-set variables
		List<? extends Variable> cutSetVariables = new LinkedList<>(this.cutSet);
		Factor result = sumOut(cutSetVariables, preResult);
		return result;
	}
}


