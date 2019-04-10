package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.ContingentSamplingGoal;

public class VariableEqualsGoal extends AbstractVariablesRelatedGoal implements ContingentSamplingGoal {

	private Object value;
	
	public VariableEqualsGoal(Variable variable, Object value) {
		super(list(variable));
		this.value = value;
	}
	
	Variable getVariable() {
		return getFirst(getVariables());
	}
	
	public Object getValue() {
		return value;
	}

	@Override
	public boolean isSatisfiedBySampleWithoutModifyingIt(Sample sample) {
		Object variableValue = sample.getAssignment().get(getVariable());
		boolean result = variableValue != null && variableValue.equals(value);
		return result;
	}

	@Override
	public String toString() {
		return getVariable() + " = " + getValue();
	}

	@Override
	protected String getGoalName() {
		return "equal to " + value;
	}
	
}
