package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public interface Assignment {
	
	Assignment copy();
	
	Object get(Variable variable);
	
	void set(Variable variable, Object value);
	
	boolean contains(Variable variable);
	
	int size();

}
