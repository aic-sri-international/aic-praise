package com.sri.ai.praise.core.representation.interfacebased.factor.api.equality;

public interface FactorsAreEqual<F> extends FactorsEqualityCheck<F> {
	
	@Override
	default boolean areEqual() {
		return true;
	}

}
