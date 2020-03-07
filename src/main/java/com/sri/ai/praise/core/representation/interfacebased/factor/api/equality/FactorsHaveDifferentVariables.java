package com.sri.ai.praise.core.representation.interfacebased.factor.api.equality;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public interface FactorsHaveDifferentVariables<T> extends FactorsAreDifferent<T> {
	
	Collection<? extends Variable> getVariablesInFirstButNotInSecond();
	
	Collection<? extends Variable> getVariablesInSecondButNotInFirst();

}
