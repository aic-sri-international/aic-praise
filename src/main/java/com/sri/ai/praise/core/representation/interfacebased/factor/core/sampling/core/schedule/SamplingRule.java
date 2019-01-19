package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.assertType;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.util.planning.api.Goal;
import com.sri.ai.util.planning.api.Plan;
import com.sri.ai.util.planning.api.Rule;
import com.sri.ai.util.planning.api.State;
import com.sri.ai.util.planning.core.AbstractAtomicPlan;

public class SamplingRule extends AbstractAtomicPlan implements Rule<Goal> {
	
	public static final double MAXIMUM_ESTIMATED_SUCCESS_WEIGHT = Plan.MAXIMUM_ESTIMATED_SUCCESS_WEIGHT;

	private SamplingFactor samplingFactor;

	private Collection<? extends Goal> antecedents;

	private Collection<? extends Goal> consequents;

	private boolean hasFired;
	
	public static SamplingRule samplingRuleFromGoals(
			SamplingFactor samplingFactor, 
			Collection<? extends Goal> consequents, 
			Collection<? extends Goal> antecedents, 
			double estimatedSuccessWeight) {
		
		return new SamplingRule(samplingFactor, consequents, antecedents, estimatedSuccessWeight);
	}

	/**
	 * Convenience creator taking variables to stand for {@link VariableIsDefined} goals.
	 * @param samplingFactor
	 * @param consequentVariables
	 * @param antecedentVariables
	 * @param estimatedSuccessWeight
	 * @return
	 */
	public static SamplingRule samplingRuleFromVariables(
			SamplingFactor samplingFactor, 
			Collection<? extends Variable> consequentVariables, 
			Collection<? extends Variable> antecedentVariables, 
			double estimatedSuccessWeight) {
		
		List<? extends Goal> antecedents = mapIntoList(antecedentVariables, v -> new VariableIsDefinedGoal(v));
		List<? extends Goal> consequents = mapIntoList(consequentVariables, v -> new VariableIsDefinedGoal(v));
		
		return new SamplingRule(samplingFactor, consequents, antecedents, estimatedSuccessWeight);
	}

	/**
	 * Convenience deterministic sampling rule creator taking variables to stand for {@link VariableIsDefined} goals.
	 * @param samplingFactor
	 * @param consequentVariables
	 * @param antecedentVariables
	 * @return
	 */
	public static SamplingRule deterministicSamplingRuleFromVariables(
			SamplingFactor samplingFactor, 
			Collection<? extends Variable> consequentVariables, 
			Collection<? extends Variable> antecedentVariables) {
		
		return samplingRuleFromVariables(samplingFactor, consequentVariables, antecedentVariables, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}

	public static SamplingRule deterministicSamplingRuleFromGoals(
			SamplingFactor samplingFactor, 
			Collection<? extends Goal> consequentVariables, 
			Collection<? extends Goal> antecedentVariables) {
		
		return samplingRuleFromGoals(samplingFactor, consequentVariables, antecedentVariables, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}

	public SamplingRule(SamplingFactor samplingFactor, Collection<? extends Goal> consequents, Collection<? extends Goal> antecedents, double estimatedSuccessWeight) {
		super(estimatedSuccessWeight);
		this.samplingFactor = samplingFactor;
		this.antecedents = antecedents;
		this.consequents =  consequents;
		this.hasFired = false;
	}

	public SamplingFactor getSamplingFactor() {
		return samplingFactor;
	}
	
	public boolean hasFired() {
		return hasFired;
	}
	
	@Override
	public Collection<? extends Goal> getConsequents() {
		return consequents;
	}

	@Override
	public Collection<? extends Goal> getAntecendents() {
		return antecedents;
	}

	public void reset() {
		hasFired = false;
	}

	public SamplingRule replaceFactor(SamplingFactor newSamplingFactor) {
		SamplingRule result = new SamplingRule(newSamplingFactor, getConsequents(), getAntecendents(), getEstimatedSuccessWeight());
		return result;
	}
	
	@Override
	public void execute(State state) {
		SamplingState sampleState = assertType(state, SamplingState.class, getClass());
		getSamplingFactor().sampleOrWeigh(sampleState.getSample());
		hasFired = true;
	}
	
	@Override
	public String toString() {
		return join(consequents) + " <= " + join(antecedents) + " with " + getSamplingFactor();
	}

}
