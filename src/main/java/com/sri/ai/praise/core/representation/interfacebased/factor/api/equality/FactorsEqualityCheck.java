package com.sri.ai.praise.core.representation.interfacebased.factor.api.equality;

public interface FactorsEqualityCheck<F> {
	
	F getFirst();
	
	F getSecond();
	
	boolean areEqual();

}
