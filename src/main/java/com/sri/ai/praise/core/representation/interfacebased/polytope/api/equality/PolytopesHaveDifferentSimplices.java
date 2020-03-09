package com.sri.ai.praise.core.representation.interfacebased.polytope.api.equality;

import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public interface PolytopesHaveDifferentSimplices extends PolytopesAreDifferent {
	
	Set<? extends Variable> getSimplexVariablesInFirstButNotInSecond();
	
	Set<? extends Variable> getSimplexVariablesInSecondButNotInFirst();

}
