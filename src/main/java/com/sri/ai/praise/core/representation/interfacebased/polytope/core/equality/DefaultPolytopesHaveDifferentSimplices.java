package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesHaveDifferentSimplices;

public class DefaultPolytopesHaveDifferentSimplices 
extends AbstractPolytopesEqualityCheck
implements PolytopesHaveDifferentSimplices {
	
	private Set<? extends Variable> simplexVariablesInFirstButNotInSecond;
	private Set<? extends Variable> simplexVariablesInSecondButNotInFirst;

	public DefaultPolytopesHaveDifferentSimplices(
			Polytope first, 
			Polytope second, 
			Set<? extends Variable> simplexVariablesInFirstButNotInSecond,
			Set<? extends Variable> simplexVariablesInSecondButNotInFirst) {
		
		super(first, second);
		this.simplexVariablesInFirstButNotInSecond = simplexVariablesInFirstButNotInSecond;
		this.simplexVariablesInSecondButNotInFirst = simplexVariablesInSecondButNotInFirst;
	}

	@Override
	public Set<? extends Variable> getSimplexVariablesInFirstButNotInSecond() {
		return simplexVariablesInFirstButNotInSecond;
	}

	@Override
	public Set<? extends Variable> getSimplexVariablesInSecondButNotInFirst() {
		return simplexVariablesInSecondButNotInFirst;
	}
	
	@Override
	public String toString() {
		return "Polytopes do not have the same simplices. " +
				"Simplices on these variables appear in first but not second: " + 
				getSimplexVariablesInFirstButNotInSecond() + 
				" and simplices on these variables apear in second but not first: " + 
				getSimplexVariablesInSecondButNotInFirst();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result =
				prime * result
						+ ((simplexVariablesInFirstButNotInSecond == null)
								? 0
								: simplexVariablesInFirstButNotInSecond.hashCode());
		result =
				prime * result
						+ ((simplexVariablesInSecondButNotInFirst == null)
								? 0
								: simplexVariablesInSecondButNotInFirst.hashCode());
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
		DefaultPolytopesHaveDifferentSimplices other = (DefaultPolytopesHaveDifferentSimplices) obj;
		if (simplexVariablesInFirstButNotInSecond == null) {
			if (other.simplexVariablesInFirstButNotInSecond != null) {
				return false;
			}
		} else if (!simplexVariablesInFirstButNotInSecond.equals(other.simplexVariablesInFirstButNotInSecond)) {
			return false;
		}
		if (simplexVariablesInSecondButNotInFirst == null) {
			if (other.simplexVariablesInSecondButNotInFirst != null) {
				return false;
			}
		} else if (!simplexVariablesInSecondButNotInFirst.equals(other.simplexVariablesInSecondButNotInFirst)) {
			return false;
		}
		return true;
	}

}
