package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.ContingentSamplingGoal;

public class VariableEqualsSomethingDifferentFrom extends AbstractSingleVariableRelatedGoal implements ContingentSamplingGoal {

	private Object value;
	
	public VariableEqualsSomethingDifferentFrom(Variable variable, Object value) {
		super(variable);
		this.value = value;
	}
	
	public Object getValue() {
		return value;
	}

	@Override
	public boolean isSatisfiedBySampleWithoutModifyingIt(Sample sample) {
		Object variableValue = sample.getAssignment().get(getVariable());
		boolean result = variableValue != null && !variableValue.equals(value);
		return result;
	}

	@Override
	public String toString() {
		return "Goal[" + super.toString() + " instantiated but different from " + getValue() + "]";
	}
	
	@Override
	protected String getGoalName() {
		return "different from " + value;
	}
	
}
