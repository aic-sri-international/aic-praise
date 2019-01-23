package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public abstract class AbstractSingleVariableRelatedGoal extends AbstractVariablesRelatedGoal {

	public AbstractSingleVariableRelatedGoal(Variable variable) {
		super(list(variable));
	}
	
	public Variable getVariable() {
		return getFirst(getVariables());
	}

	@Override
	public String toString() {
		return getVariable().toString();
	}
}
