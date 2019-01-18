package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.assertType;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.util.planning.api.Plan;
import com.sri.ai.util.planning.api.Rule;
import com.sri.ai.util.planning.api.State;
import com.sri.ai.util.planning.core.AbstractAtomicPlan;

public class SamplingRule extends AbstractAtomicPlan implements Rule<VariableGoal> {
	
	public static final double MAXIMUM_ESTIMATED_SUCCESS_WEIGHT = Plan.MAXIMUM_ESTIMATED_SUCCESS_WEIGHT;

	private SamplingFactor samplingFactor;

	private Collection<? extends VariableGoal> antecedents;

	private Collection<? extends VariableGoal> consequents;

	private boolean hasFired;
	
	public static SamplingRule samplingRule(
			SamplingFactor samplingFactor, 
			Collection<? extends Variable> consequentVariables, 
			Collection<? extends Variable> antecedentVariables, 
			double estimatedSuccessWeight) {
		
		List<? extends VariableGoal> antecedents = mapIntoList(antecedentVariables, v -> new VariableGoal(v));
		List<? extends VariableGoal> consequents = mapIntoList(consequentVariables, v -> new VariableGoal(v));
		
		return new SamplingRule(samplingFactor, consequents, antecedents, estimatedSuccessWeight);
	}

	public static SamplingRule deterministicSamplingRule(
			SamplingFactor samplingFactor, 
			Collection<? extends Variable> consequentVariables, 
			Collection<? extends Variable> antecedentVariables) {
		
		return samplingRule(samplingFactor, consequentVariables, antecedentVariables, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}

	public SamplingRule(SamplingFactor samplingFactor, Collection<? extends VariableGoal> consequents, Collection<? extends VariableGoal> antecedents, double estimatedSuccessWeight) {
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
	public Collection<? extends VariableGoal> getConsequents() {
		return consequents;
	}

	@Override
	public Collection<? extends VariableGoal> getAntecendents() {
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
