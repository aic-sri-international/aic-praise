package com.sri.ai.praise.core.representation.interfacebased.factor.api.equality;

public interface FactorsAreDifferent<F> extends FactorsEqualityCheck<F> {
	
	@Override
	default boolean areEqual() {
		return false;
	}

}
