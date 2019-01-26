package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class BoundVariable implements Variable {

	private Object value;
	
	private BoundVariable(Object value) {
		this.value = value;
	}

	public Object getValue() {
		return value;
	}

	@Override
	public List<? extends Object> getValues() {
		throw new Error("getValues not implemented for " + getClass().getSimpleName());
	}

}
