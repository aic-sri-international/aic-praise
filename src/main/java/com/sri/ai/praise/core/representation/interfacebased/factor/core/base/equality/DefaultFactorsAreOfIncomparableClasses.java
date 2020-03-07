package com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsAreOfIncomparableClasses;

public class DefaultFactorsAreOfIncomparableClasses<F> 
extends AbstractFactorsEqualityCheck<F>
implements FactorsAreOfIncomparableClasses<F> {

	public static <F> DefaultFactorsAreOfIncomparableClasses<F> factorsAreOfIncomparableClasses(F first, F second) {
		return new DefaultFactorsAreOfIncomparableClasses<F>(first, second);
	}

	public DefaultFactorsAreOfIncomparableClasses(F first, F second) {
		super(first, second);
	}
	
	@Override
	public String toString() {
		return "Factors are of incomparable classes";
	}

}
