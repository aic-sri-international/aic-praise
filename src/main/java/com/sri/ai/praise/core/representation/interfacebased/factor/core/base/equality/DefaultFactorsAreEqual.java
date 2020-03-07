package com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsAreEqual;

public class DefaultFactorsAreEqual<F> extends AbstractFactorsEqualityCheck<F> implements FactorsAreEqual<F> {

	public DefaultFactorsAreEqual(F first, F second) {
		super(first, second);
	}

	@Override
	public boolean areEqual() {
		return true;
	}
	
	@Override
	public String toString() {
		return "Factors are equal";
	}

}
