package com.sri.ai.praise.learning.parameterlearning;

import java.util.List;

public interface Datapoint {
	
	public Object getValueOfVariable(Variable variable);
	
	public List<? extends Object> getValueOfVariables(List<? extends Variable> variables);
}
