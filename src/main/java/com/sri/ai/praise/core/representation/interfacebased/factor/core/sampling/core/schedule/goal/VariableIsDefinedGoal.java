package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public class VariableIsDefinedGoal extends AbstractSingleVariableRelatedGoal {

	public VariableIsDefinedGoal(Variable variable) {
		super(variable);
	}

	@Override
	public boolean isSatisfied(Sample sample) {
		boolean result = sample.getAssignment().get(getVariable()) != null;
		return result;
	}

	@Override
	protected String getGoalName() {
		return getClass().getSimpleName();
	}
}
