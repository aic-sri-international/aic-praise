package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

public interface PolytopesAreDifferent extends PolytopesEqualityCheck {

	@Override
	default boolean areEqual() {
		return false;
	}
}
