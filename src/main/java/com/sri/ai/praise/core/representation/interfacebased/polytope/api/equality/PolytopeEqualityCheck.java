package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;

public interface PolytopeEqualityCheck {

	Polytope getFirst();
	Polytope getSecond();
	boolean areEqual();
	
}
