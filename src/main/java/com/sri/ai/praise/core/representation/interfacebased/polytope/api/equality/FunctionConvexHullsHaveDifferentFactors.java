package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck;

public interface FunctionConvexHullsHaveDifferentFactors extends PolytopesAreDifferent {
	
	FactorsEqualityCheck getFactorsEqualityCheck();

}
