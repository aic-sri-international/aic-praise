package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample;

import java.util.Collection;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public interface Assignment {
	
	Assignment copy();
	
	Object get(Variable variable);
	
	void set(Variable variable, Object value);
	
	boolean contains(Variable variable);
	
	int size();

	Map<Variable, Object> mapValue();
	
	void setMapValue(Map<Variable, Object> map);

	Collection<? extends Variable> getVariables();
	
}
