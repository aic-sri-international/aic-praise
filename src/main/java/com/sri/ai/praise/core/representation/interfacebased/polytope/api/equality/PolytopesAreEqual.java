package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

public interface PolytopesAreEqual extends PolytopeEqualityCheck {

	@Override
	default boolean areEqual() {
		return true;
	}
}
