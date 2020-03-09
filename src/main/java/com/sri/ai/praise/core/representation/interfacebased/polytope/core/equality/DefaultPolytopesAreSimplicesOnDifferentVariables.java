package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesAreSimplicesOnDifferentVariables;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.Simplex;

public class DefaultPolytopesAreSimplicesOnDifferentVariables 
extends AbstractPolytopesEqualityCheck
implements PolytopesAreSimplicesOnDifferentVariables {

	public DefaultPolytopesAreSimplicesOnDifferentVariables(Polytope first, Polytope second) {
		super(first, second);
	}
	
	@Override
	public String toString() {
		return "Polytopes are simplicies on different variables: " + ((Simplex) getFirst()).getVariable() + " and " + ((Simplex) getSecond()).getVariable();
	}
}
