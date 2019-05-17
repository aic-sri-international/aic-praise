package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import java.util.Collection;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Assignment;
import com.sri.ai.util.Enclosing;

public class UnmodifiableAssignment implements Assignment {
	
	private Assignment base;
	
	public UnmodifiableAssignment(Assignment baseAssignment) {
		this.base = baseAssignment;
	}

	@Override
	public Assignment copy() {
		return new UnmodifiableAssignment(base);
	}

	@Override
	public Object get(Variable variable) {
		return base.get(variable);
	}

	@Override
	public void set(Variable variable, Object value) {
		throw new Error((new Enclosing(){}).methodName() + " cannot modify " + this.getClass());
	}

	@Override
	public boolean contains(Variable variable) {
		return base.contains(variable);
	}

	@Override
	public int size() {
		return base.size();
	}

	@Override
	public Map<Variable, Object> mapValue() {
		throw new Error((new Enclosing(){}).methodName() + " cannot provide map value because it is unmodifiable");
	}

	@Override
	public void setMapValue(Map<Variable, Object> map) {
		throw new Error((new Enclosing(){}).methodName() + " cannot modify " + this.getClass());
	}

	@Override
	public Collection<? extends Variable> getVariables() {
		return base.getVariables();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((base == null) ? 0 : base.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		UnmodifiableAssignment other = (UnmodifiableAssignment) obj;
		if (base == null) {
			if (other.base != null) {
				return false;
			}
		} else if (!base.equals(other.base)) {
			return false;
		}
		return true;
	}
}
