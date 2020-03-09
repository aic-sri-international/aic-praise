package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.FunctionConvexHullsHaveDifferentIndices;

public class DefaultFunctionConvexHullsHaveDifferentIndices 
extends AbstractPolytopesEqualityCheck
implements FunctionConvexHullsHaveDifferentIndices {
	
	private Set<? extends Variable> indicesInFirstButNotInSecond;
	private Set<? extends Variable> indicesInSecondButNotInFirst;

	public DefaultFunctionConvexHullsHaveDifferentIndices(
			Polytope first, 
			Polytope second, 
			Set<? extends Variable> indicesInFirstButNotInSecond,
			Set<? extends Variable> indicesInSecondButNotInFirst) {
		
		super(first, second);
		this.indicesInFirstButNotInSecond = indicesInFirstButNotInSecond;
		this.indicesInSecondButNotInFirst = indicesInSecondButNotInFirst;
	}

	@Override
	public Set<? extends Variable> getIndicesInFirstButNotInSecond() {
		return indicesInFirstButNotInSecond;
	}

	@Override
	public Set<? extends Variable> getIndicesInSecondButNotInFirst() {
		return indicesInSecondButNotInFirst;
	}

	@Override
	public String toString() {
		return "Function convex hulls do not have the same indices or free variables. " +
				"These indices appear in first but not second: " + 
				getIndicesInFirstButNotInSecond() + 
				" and these indices appear in second but not first: " + 
				getIndicesInSecondButNotInFirst();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result =
				prime * result + ((indicesInFirstButNotInSecond == null) ? 0 : indicesInFirstButNotInSecond.hashCode());
		result =
				prime * result + ((indicesInSecondButNotInFirst == null) ? 0 : indicesInSecondButNotInFirst.hashCode());
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
		DefaultFunctionConvexHullsHaveDifferentIndices other = (DefaultFunctionConvexHullsHaveDifferentIndices) obj;
		if (indicesInFirstButNotInSecond == null) {
			if (other.indicesInFirstButNotInSecond != null) {
				return false;
			}
		} else if (!indicesInFirstButNotInSecond.equals(other.indicesInFirstButNotInSecond)) {
			return false;
		}
		if (indicesInSecondButNotInFirst == null) {
			if (other.indicesInSecondButNotInFirst != null) {
				return false;
			}
		} else if (!indicesInSecondButNotInFirst.equals(other.indicesInSecondButNotInFirst)) {
			return false;
		}
		return true;
	}

}
