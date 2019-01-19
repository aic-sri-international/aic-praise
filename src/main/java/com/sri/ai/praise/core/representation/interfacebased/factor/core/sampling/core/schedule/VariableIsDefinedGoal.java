package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.VariableDependentGoal;

public class VariableIsDefinedGoal implements VariableDependentGoal {

	private Variable variable;

	public VariableIsDefinedGoal(Variable variable) {
		this.variable = variable;
	}

	public Variable getVariable() {
		return variable;
	}

	List<Variable> cachedDependencies;
	
	@Override
	public Collection<? extends Variable> dependencies() {
		if (cachedDependencies == null) {
			cachedDependencies = list(variable);
		}
		return cachedDependencies;
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
		VariableIsDefinedGoal other = (VariableIsDefinedGoal) obj;
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
