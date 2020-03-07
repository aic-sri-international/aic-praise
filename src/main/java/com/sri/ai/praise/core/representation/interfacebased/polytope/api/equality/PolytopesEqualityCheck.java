package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultPolytopesAreEqual;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultPolytopesAreOfIncomparableClasses;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality.DefaultPolytopesHaveDifferentAtomicPolytopes;

public interface PolytopesEqualityCheck {

	Polytope getFirst();
	
	Polytope getSecond();
	
	boolean areEqual();

	/////////////// STATIC BUILDERS
	
	static DefaultPolytopesAreEqual polytopesAreEqual(Polytope first, Polytope second) {
		return new DefaultPolytopesAreEqual(first, second);
	}
	
	static DefaultPolytopesAreOfIncomparableClasses polytopesAreOfIncomparableClasses(Polytope first, Polytope second) {
		return new DefaultPolytopesAreOfIncomparableClasses(first, second);
	}
	
	static DefaultPolytopesHaveDifferentAtomicPolytopes polytopesHaveDifferentAtomicPolytopes(
			Polytope first, 
			Polytope second, 
			Set<? extends AtomicPolytope> atomicPolytopesInFirstButNotInSecond,
			Set<? extends AtomicPolytope> atomicPolytopesInSecondButNotInFirst) {
		
		return new DefaultPolytopesHaveDifferentAtomicPolytopes(first, second, atomicPolytopesInFirstButNotInSecond, atomicPolytopesInSecondButNotInFirst);
	}
	
}
