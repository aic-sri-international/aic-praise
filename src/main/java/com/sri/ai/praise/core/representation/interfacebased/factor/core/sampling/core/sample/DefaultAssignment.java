package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import static com.sri.ai.util.Util.map;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Assignment;

public class DefaultAssignment implements Assignment {
	
	private Map<Variable, Object> fromVariablesToValues;
	
	public DefaultAssignment() {
		this(map());
	}

	public DefaultAssignment(Map<Variable, Object> assignmentMap) {
		this.fromVariablesToValues = assignmentMap;
	}

	@Override
	public Collection<? extends Variable> getVariables() {
		return Collections.unmodifiableCollection(fromVariablesToValues.keySet());
	}

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

	@Override
	public Map<Variable, Object> mapValue() {
		return fromVariablesToValues;
	}
	
	@Override
	public void setMapValue(Map<Variable, Object> map) {
		fromVariablesToValues = map;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((fromVariablesToValues == null) ? 0 : fromVariablesToValues.hashCode());
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
		DefaultAssignment other = (DefaultAssignment) obj;
		if (fromVariablesToValues == null) {
			if (other.fromVariablesToValues != null)
				return false;
		} else if (!fromVariablesToValues.equals(other.fromVariablesToValues))
			return false;
		return true;
	}

}
