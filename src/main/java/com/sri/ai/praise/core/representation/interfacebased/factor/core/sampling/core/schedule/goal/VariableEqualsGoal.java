package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public class VariableEqualsGoal extends AbstractVariablesRelatedGoal {

	private Object value;
	
	public VariableEqualsGoal(Variable variable, Object value) {
		super(list(variable));
	}
	
	Variable getVariable() {
		return getFirst(getVariables());
	}
	
	public Object getValue() {
		return value;
	}

	@Override
	public boolean isSatisfied(Sample sample) {
		Object variableValue = sample.getAssignment().get(getVariable());
		boolean result = variableValue != null && variableValue.equals(value);
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		VariableEqualsGoal other = (VariableEqualsGoal) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}
	

	@Override
	public String toString() {
		return "Goal[" + super.toString() + " = " + getValue() + "]";
	}

	@Override
	protected String getGoalName() {
		return getClass().getSimpleName();
	}
	
}
