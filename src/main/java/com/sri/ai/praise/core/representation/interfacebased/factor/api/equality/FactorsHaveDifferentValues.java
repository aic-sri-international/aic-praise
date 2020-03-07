package com.sri.ai.praise.core.representation.interfacebased.factor.api.equality;

import java.util.Collection;

public interface FactorsHaveDifferentValues<F, V> extends FactorsAreDifferent<F> {
	
	/** Returns the values of variables for the violating assignment in the order of <code>getFirst().getVariables()</code>. */
	Collection<? extends V> getViolatingAssignment();
	
	double getValueOfFirst();
	
	double getValueOfSecond();

}
