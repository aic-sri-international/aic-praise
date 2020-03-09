package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;

public interface FirstPolytopeHasFunctionConvexHullWithoutMatchInSecond extends PolytopesAreDifferent {
	
	FunctionConvexHull getFunctionConvexHullInFirst();
	
	Collection<? extends PolytopesEqualityCheck> getEqualityChecksAgainstRemainingFunctionConvexHullsInSecond();

}
