package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.planning.api.Goal;

public class VariableGoal implements Goal {

	private Variable variable;

	public VariableGoal(Variable variable) {
		this.variable = variable;
	}

	public Variable getVariable() {
		return variable;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((variable == null) ? 0 : variable.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		VariableGoal other = (VariableGoal) obj;
		if (variable == null) {
			if (other.variable != null)
				return false;
		} else if (!variable.equals(other.variable))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return getVariable().toString();
	}
	
}
