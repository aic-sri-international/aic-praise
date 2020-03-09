package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.FunctionConvexHullsHaveDifferentFactors;

public class DefaultFunctionConvexHullsHaveDifferentFactors 
extends AbstractPolytopesEqualityCheck
implements FunctionConvexHullsHaveDifferentFactors {
	
	private FactorsEqualityCheck factorsEqualityCheck;
	
	public DefaultFunctionConvexHullsHaveDifferentFactors(
			Polytope first, 
			Polytope second, 
			FactorsEqualityCheck factorsEqualityCheck) {
		
		super(first, second);
		this.factorsEqualityCheck = factorsEqualityCheck;
	}

	@Override
	public FactorsEqualityCheck getFactorsEqualityCheck() {
		return factorsEqualityCheck;
	}
	
	@Override
	public String toString() {
		return "Function convex hulls have different factors: " + getFactorsEqualityCheck();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((factorsEqualityCheck == null) ? 0 : factorsEqualityCheck.hashCode());
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
		DefaultFunctionConvexHullsHaveDifferentFactors other = (DefaultFunctionConvexHullsHaveDifferentFactors) obj;
		if (factorsEqualityCheck == null) {
			if (other.factorsEqualityCheck != null) {
				return false;
			}
		} else if (!factorsEqualityCheck.equals(other.factorsEqualityCheck)) {
			return false;
		}
		return true;
	}
}
