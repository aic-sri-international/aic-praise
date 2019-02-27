package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.assertType;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;
import com.sri.ai.util.planning.api.Plan;
import com.sri.ai.util.planning.api.Rule;
import com.sri.ai.util.planning.api.State;
import com.sri.ai.util.planning.core.AbstractAtomicPlan;
import com.sri.ai.util.tree.DefaultTree;
import com.sri.ai.util.tree.Tree;

public class SamplingRule extends AbstractAtomicPlan implements Rule<SamplingGoal> {
	
	public static final double MAXIMUM_ESTIMATED_SUCCESS_WEIGHT = Plan.MAXIMUM_ESTIMATED_SUCCESS_WEIGHT;

	private SamplingFactor sampler;

	private Collection<? extends SamplingGoal> antecedents;

	private Collection<? extends SamplingGoal> consequents;

	private boolean hasFired;
	
	public static SamplingRule samplingRuleFromGoals(
			SamplingFactor sampler, 
			Collection<? extends SamplingGoal> consequents, 
			Collection<? extends SamplingGoal> antecedents, 
			double estimatedSuccessWeight) {
		
		return new SamplingRule(sampler, consequents, antecedents, estimatedSuccessWeight);
	}

	/**
	 * Convenience creator taking variables to stand for {@link VariableIsDefined} goals.
	 * @param sampler
	 * @param consequentVariables
	 * @param antecedentVariables
	 * @param estimatedSuccessWeight
	 * @return
	 */
	public static SamplingRule samplingRuleFromVariables(
			SamplingFactor sampler, 
			Collection<? extends Variable> consequentVariables, 
			Collection<? extends Variable> antecedentVariables, 
			double estimatedSuccessWeight) {
		
		List<? extends SamplingGoal> antecedents = mapIntoList(antecedentVariables, v -> new VariableIsDefinedGoal(v));
		List<? extends SamplingGoal> consequents = mapIntoList(consequentVariables, v -> new VariableIsDefinedGoal(v));
		
		return new SamplingRule(sampler, consequents, antecedents, estimatedSuccessWeight);
	}

	/**
	 * Convenience deterministic sampling rule creator taking variables to stand for {@link VariableIsDefined} goals.
	 * @param sampler
	 * @param consequentVariables
	 * @param antecedentVariables
	 * @return
	 */
	public static SamplingRule deterministicSamplingRuleFromVariables(
			SamplingFactor sampler, 
			Collection<? extends Variable> consequentVariables, 
			Collection<? extends Variable> antecedentVariables) {
		
		return samplingRuleFromVariables(sampler, consequentVariables, antecedentVariables, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}

	public static SamplingRule deterministicSamplingRuleFromGoals(
			SamplingFactor sampler, 
			Collection<? extends SamplingGoal> consequentVariables, 
			Collection<? extends SamplingGoal> antecedentVariables) {
		
		return samplingRuleFromGoals(sampler, consequentVariables, antecedentVariables, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}

	public SamplingRule(SamplingFactor sampler, Collection<? extends SamplingGoal> consequents, Collection<? extends SamplingGoal> antecedents, double estimatedSuccessWeight) {
		super(estimatedSuccessWeight);
		this.sampler = sampler;
		this.antecedents = antecedents;
		this.consequents =  consequents;
		this.hasFired = false;
	}

	public SamplingFactor getSamplingFactor() {
		return sampler;
	}
	
	public boolean hasFired() {
		return hasFired;
	}
	
	@Override
	public Collection<? extends SamplingGoal> getConsequents() {
		return consequents;
	}

	@Override
	public Collection<? extends SamplingGoal> getAntecendents() {
		return antecedents;
	}

	public void reset() {
		hasFired = false;
	}

	public SamplingRule replaceFactor(SamplingFactor newSamplingFactor) {
		SamplingRule result = new SamplingRule(newSamplingFactor, getConsequents(), getAntecendents(), getEstimatedSuccessWeight());
		return result;
	}

	public SamplingRule replaceGoals(Function<SamplingGoal, SamplingGoal> replacement) {
		List<SamplingGoal> newAntecedents = mapIntoList(getAntecendents(), replacement);
		List<SamplingGoal> newConsequents = mapIntoList(getConsequents(), replacement);
		SamplingRule result = new SamplingRule(getSamplingFactor(), newConsequents, newAntecedents, getEstimatedSuccessWeight());
		return result;
	}
	
	@Override
	public void execute(State state) {
		SamplingState sampleState = assertType(state, SamplingState.class, getClass());
		sampleOrWeigh(sampleState.getSample());
		hasFired = true;
	}

	public void sampleOrWeigh(Sample sample) {
		getSamplingFactor().sampleOrWeigh(sample);
	}
	
	@Override
	public Tree<String> stringTree() {
		return new DefaultTree<String>(toString());
	}

	@Override
	public String toString() {
		String consequentsString = join(consequents);
		String antecedentsString = join(antecedents);
		SamplingFactor factorString = getSamplingFactor();
		String result = consequentsString + " <= " + antecedentsString + " with " + factorString;
		return result;
	}

	public static final Comparator<? super SamplingRule> SUCCESS_COMPARATOR = new Comparator<SamplingRule>() {
	
		@Override
		public int compare(SamplingRule rule1, SamplingRule rule2) {
			return Double.compare(rule2.getEstimatedSuccessWeight(), rule1.getEstimatedSuccessWeight());
			// note the inversion, as we want descending order
		}
		
	};

	public boolean isSatisfied(Sample sample) {
		return forAll(getAntecendents(), a -> a.isSatisfied(sample));
	}
}
