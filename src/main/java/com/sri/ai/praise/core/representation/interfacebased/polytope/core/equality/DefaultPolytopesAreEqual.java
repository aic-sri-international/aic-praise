package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesAreEqual;

public class DefaultPolytopesAreEqual extends AbstractPolytopesEqualityCheck implements PolytopesAreEqual {

	public DefaultPolytopesAreEqual(Polytope first, Polytope second) {
		super(first, second);
	}

	@Override
	public String toString() {
		return "Polytopes are equal";
	}

}
