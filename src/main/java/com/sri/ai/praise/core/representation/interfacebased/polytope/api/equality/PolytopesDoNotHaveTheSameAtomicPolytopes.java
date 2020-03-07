package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;

public interface PolytopesDoNotHaveTheSameAtomicPolytopes extends PolytopesAreDifferent {
	
	Collection<? extends AtomicPolytope> getAtomicPolytopesInFirstButNotInSecond();
	Collection<? extends AtomicPolytope> getAtomicPolytopesInSecondButNotInFirst();

}
