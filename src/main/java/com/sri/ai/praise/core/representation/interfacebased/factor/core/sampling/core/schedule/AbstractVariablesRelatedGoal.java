package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.join;

import java.util.Collection;
import java.util.Collections;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;

public abstract class AbstractVariablesRelatedGoal implements SamplingGoal {

	@Override
	abstract public boolean isSatisfied(Sample sample);

	abstract protected String getGoalName();
	
    /////////////////
	
	private Collection<? extends Variable> variables;

	public AbstractVariablesRelatedGoal(Collection<? extends Variable> variables) {
		this.variables = variables;
	}

    /////////////////
	
	@Override
	public Collection<? extends Variable> getVariables() {
		return Collections.unmodifiableCollection(variables);
	}

    /////////////////

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((variables == null) ? 0 : variables.hashCode());
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
		AbstractVariablesRelatedGoal other = (AbstractVariablesRelatedGoal) obj;
		if (variables == null) {
			if (other.variables != null)
				return false;
		} else if (!variables.equals(other.variables))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return getGoalName() + "(" + join(getVariables()) + ")";
	}
	
}
