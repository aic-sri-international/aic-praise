package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesHaveADifferentNumberOfFunctionConvexHulls;

public class DefaultPolytopesHaveADifferentNumberOfFunctionConvexHulls extends AbstractPolytopesEqualityCheck implements PolytopesHaveADifferentNumberOfFunctionConvexHulls {

	public DefaultPolytopesHaveADifferentNumberOfFunctionConvexHulls(Polytope first, Polytope second) {
		super(first, second);
	}

	@Override
	public String toString() {
		return "Polytopes have a different number of function convex hulls: " + getFirst() + " and " + getSecond();
	}
}
