package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

public interface PolytopesAreEqual extends PolytopesEqualityCheck {

	@Override
	default boolean areEqual() {
		return true;
	}
}
