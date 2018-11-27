package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Assignment;

public class DefaultAssignment implements Assignment {
	
	private Map<Variable, Object> fromVariablesToValues = map();
	
	@Override
	public Object get(Variable variable) {
		return fromVariablesToValues.get(variable);
	}
	
	@Override
	public void set(Variable variable, Object value) {
		fromVariablesToValues.put(variable, value);
	}
	
	@Override
	public String toString() {
		return fromVariablesToValues.toString();
	}

	@Override
	public boolean contains(Variable variable) {
		return fromVariablesToValues.containsKey(variable);
	}

	@Override
	public int size() {
		return fromVariablesToValues.size();
	}

	@Override
	public Assignment copy() {
		DefaultAssignment copy = new DefaultAssignment();
		copy.fromVariablesToValues.putAll(fromVariablesToValues);
		return copy;
	}

}
