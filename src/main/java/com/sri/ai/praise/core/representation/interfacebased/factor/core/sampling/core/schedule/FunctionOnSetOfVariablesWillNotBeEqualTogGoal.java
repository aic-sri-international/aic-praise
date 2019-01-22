package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public class FunctionOnSetOfVariablesWillNotBeEqualTogGoal<T> extends AbstractVariablesRelatedGoal {

	private Function<Collection<T>, T> function;
	private Object forbiddenValue;
	private String goalName;
	
	public FunctionOnSetOfVariablesWillNotBeEqualTogGoal(String goalName, Collection<? extends Variable> variables, Function<Collection<T>, T> function, Object forbiddenValue) {
		super(variables);
		this.function = function;
		this.forbiddenValue = forbiddenValue;
		this.goalName = goalName;
	}

	@Override
	public boolean isSatisfied(Sample sample) {
		@SuppressWarnings("unchecked")
		ArrayList<T> values = mapIntoArrayList(getVariables(), v -> (T) sample.getAssignment().get(v));
		boolean result;
		if (values.contains(null)) {
			result = false; // we cannot be sure applying the function to the variables once they are all defined is not going to be the forbidden value.
		}
		else {
			T valueFromSample = function.apply(values);
			result = ! valueFromSample.equals(forbiddenValue);
		}
		return result;
	}
	
	@Override
	protected String getGoalName() {
		return goalName;
	}
	
}