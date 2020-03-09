package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import static com.sri.ai.util.Util.join;

import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.ThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches;

public class DefaultThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches
extends AbstractPolytopesEqualityCheck 
implements ThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches {

	private Set<? extends FunctionConvexHull> leftOvers;

	public DefaultThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches(
			Polytope first, 
			Polytope second,
			Set<? extends FunctionConvexHull> leftOvers) {
		
		super(first, second);
		this.leftOvers = leftOvers;
	}
	
	@Override
	public Set<? extends FunctionConvexHull> getLeftOvers() {
		return leftOvers;
	}

	@Override
	public String toString() {
		return "Second polytope has function convex hulls without a match: " + join(leftOvers) + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((leftOvers == null) ? 0 : leftOvers.hashCode());
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
		DefaultThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches other =
				(DefaultThereAreFunctionConvexHullsInSecondPolytopeWithoutMatches) obj;
		if (leftOvers == null) {
			if (other.leftOvers != null) {
				return false;
			}
		} else if (!leftOvers.equals(other.leftOvers)) {
			return false;
		}
		return true;
	}
	
}
