package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public class VariableEqualsSomethingDifferentFrom extends AbstractSingleVariableRelatedGoal {

	private Object value;
	
	public VariableEqualsSomethingDifferentFrom(Variable variable, Object value) {
		super(variable);
	}
	
	public Object getValue() {
		return value;
	}

	@Override
	public boolean isSatisfied(Sample sample) {
		Object variableValue = sample.getAssignment().get(getVariable());
		boolean result = variableValue != null && !variableValue.equals(value);
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
		VariableEqualsSomethingDifferentFrom other = (VariableEqualsSomethingDifferentFrom) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}
	

	@Override
	public String toString() {
		return "Goal[" + super.toString() + " instantiated but different from " + getValue() + "]";
	}
	
	@Override
	protected String getGoalName() {
		return getClass().getSimpleName();
	}
	
}
