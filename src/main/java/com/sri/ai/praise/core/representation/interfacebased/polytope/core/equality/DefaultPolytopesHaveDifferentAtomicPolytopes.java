package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import java.util.Collection;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesHaveDifferentAtomicPolytopes;

public class DefaultPolytopesHaveDifferentAtomicPolytopes 
extends AbstractPolytopesEqualityCheck
implements PolytopesHaveDifferentAtomicPolytopes {
	
	private Set<? extends AtomicPolytope> atomicPolytopesInFirstButNotInSecond;
	private Set<? extends AtomicPolytope> atomicPolytopesInSecondButNotInFirst;

	public DefaultPolytopesHaveDifferentAtomicPolytopes(
			Polytope first, 
			Polytope second, 
			Set<? extends AtomicPolytope> atomicPolytopesInFirstButNotInSecond,
			Set<? extends AtomicPolytope> atomicPolytopesInSecondButNotInFirst) {
		
		super(first, second);
		this.atomicPolytopesInFirstButNotInSecond = atomicPolytopesInFirstButNotInSecond;
		this.atomicPolytopesInSecondButNotInFirst = atomicPolytopesInSecondButNotInFirst;
	}

	@Override
	public Collection<? extends AtomicPolytope> getAtomicPolytopesInFirstButNotInSecond() {
		return atomicPolytopesInFirstButNotInSecond;
	}

	@Override
	public Collection<? extends AtomicPolytope> getAtomicPolytopesInSecondButNotInFirst() {
		return atomicPolytopesInSecondButNotInFirst;
	}
	
	@Override
	public String toString() {
		return "Polytopes do not have the same atomic polytopes. " +
				"These appear in first but not second: " + 
				getAtomicPolytopesInFirstButNotInSecond() + 
				" and these apear in second but not first: " + 
				getAtomicPolytopesInSecondButNotInFirst();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result =
				prime * result
						+ ((atomicPolytopesInFirstButNotInSecond == null)
								? 0
								: atomicPolytopesInFirstButNotInSecond.hashCode());
		result =
				prime * result
						+ ((atomicPolytopesInSecondButNotInFirst == null)
								? 0
								: atomicPolytopesInSecondButNotInFirst.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		DefaultPolytopesHaveDifferentAtomicPolytopes other = (DefaultPolytopesHaveDifferentAtomicPolytopes) obj;
		if (atomicPolytopesInFirstButNotInSecond == null) {
			if (other.atomicPolytopesInFirstButNotInSecond != null) {
				return false;
			}
		} else if (!atomicPolytopesInFirstButNotInSecond.equals(other.atomicPolytopesInFirstButNotInSecond)) {
			return false;
		}
		if (atomicPolytopesInSecondButNotInFirst == null) {
			if (other.atomicPolytopesInSecondButNotInFirst != null) {
				return false;
			}
		} else if (!atomicPolytopesInSecondButNotInFirst.equals(other.atomicPolytopesInSecondButNotInFirst)) {
			return false;
		}
		return true;
	}

}
