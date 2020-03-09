package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public interface FunctionConvexHullsHaveDifferentIndices extends PolytopesAreDifferent {
	
	Set<? extends Variable> getIndicesInFirstButNotInSecond();
	
	Set<? extends Variable> getIndicesInSecondButNotInFirst();

}
