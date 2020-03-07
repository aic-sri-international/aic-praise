package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesAreOfIncomparableClasses;

public class DefaultPolytopesAreOfIncomparableClasses 
extends AbstractPolytopesEqualityCheck
implements PolytopesAreOfIncomparableClasses {

	public DefaultPolytopesAreOfIncomparableClasses(Polytope first, Polytope second) {
		super(first, second);
	}
	
	@Override
	public String toString() {
		return "Polytopes are of incomparable classes " + getFirst().getClass().getSimpleName() + " and " + getSecond().getClass().getSimpleName();
	}

}
