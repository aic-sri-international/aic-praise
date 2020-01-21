package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal;

import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.ContingentSamplingGoal;

public class FunctionOnSetOfVariablesSatisfiesCondition<T> extends AbstractVariablesRelatedGoal implements ContingentSamplingGoal {

	private Function<Collection<T>, T> function;
	private Predicate<T> condition;
	private String description;
	private String goalName;
	
	public FunctionOnSetOfVariablesSatisfiesCondition(
			String description,
			String goalName, 
			Collection<? extends Variable> variables, 
			Function<Collection<T>, T> function,
			Predicate<T> condition) {
		
		super(variables);
		this.function = function;
		this.condition = condition;
		this.description = description;
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
			result = condition.test(valueFromSample);
		}
		return result;
	}
	
	public String getDescription() {
		return description;
	}
	
	@Override
	public String toString() {
		return description;
	}
	
	@Override
	protected String getGoalName() {
		return goalName;
	}
	
}