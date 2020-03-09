package com.sri.ai.praise.core.representation.interfacebased.polytope.core.equality;

import static com.sri.ai.util.Util.join;

import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.FirstPolytopeHasFunctionConvexHullWithoutMatchInSecond;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality.PolytopesEqualityCheck;

public class DefaultFirstPolytopeHasFunctionConvexHullWithoutMatchInSecond 
extends AbstractPolytopesEqualityCheck
implements FirstPolytopeHasFunctionConvexHullWithoutMatchInSecond {

	private FunctionConvexHull functionConvexHullInFirst;
	
	private Set<? extends PolytopesEqualityCheck> equalityChecksAgainstRemainingFunctionConvexHullsInSecond;
	
	public DefaultFirstPolytopeHasFunctionConvexHullWithoutMatchInSecond(
			Polytope first, 
			Polytope second,
			FunctionConvexHull functionConvexHullInFirst,
			Set<? extends PolytopesEqualityCheck> equalityChecksAgainstRemainingFunctionConvexHullsInSecond) {
		
		super(first, second);
		this.functionConvexHullInFirst = functionConvexHullInFirst;
		this.equalityChecksAgainstRemainingFunctionConvexHullsInSecond =
				equalityChecksAgainstRemainingFunctionConvexHullsInSecond;
	}

	@Override
	public FunctionConvexHull getFunctionConvexHullInFirst() {
		return functionConvexHullInFirst;
	}

	@Override
	public Set<? extends PolytopesEqualityCheck> getEqualityChecksAgainstRemainingFunctionConvexHullsInSecond() {
		return equalityChecksAgainstRemainingFunctionConvexHullsInSecond;
	}
	
	@Override
	public String toString() {
		return "FunctionConvexHull in first polytope does not have a match in second one: " 
				+ getFunctionConvexHullInFirst() 
				+ ". Here are its equality checks against remaining function convex hulls in second polytope: " 
				+ join(getEqualityChecksAgainstRemainingFunctionConvexHullsInSecond());
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result =
				prime * result
						+ ((equalityChecksAgainstRemainingFunctionConvexHullsInSecond == null)
								? 0
								: equalityChecksAgainstRemainingFunctionConvexHullsInSecond.hashCode());
		result = prime * result + ((functionConvexHullInFirst == null) ? 0 : functionConvexHullInFirst.hashCode());
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
		DefaultFirstPolytopeHasFunctionConvexHullWithoutMatchInSecond other =
				(DefaultFirstPolytopeHasFunctionConvexHullWithoutMatchInSecond) obj;
		if (equalityChecksAgainstRemainingFunctionConvexHullsInSecond == null) {
			if (other.equalityChecksAgainstRemainingFunctionConvexHullsInSecond != null) {
				return false;
			}
		} else if (!equalityChecksAgainstRemainingFunctionConvexHullsInSecond
				.equals(other.equalityChecksAgainstRemainingFunctionConvexHullsInSecond)) {
			return false;
		}
		if (functionConvexHullInFirst == null) {
			if (other.functionConvexHullInFirst != null) {
				return false;
			}
		} else if (!functionConvexHullInFirst.equals(other.functionConvexHullInFirst)) {
			return false;
		}
		return true;
	}

}
