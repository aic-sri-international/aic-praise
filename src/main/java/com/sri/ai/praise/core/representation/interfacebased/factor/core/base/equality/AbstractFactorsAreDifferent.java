package com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsAreDifferent;

public abstract class AbstractFactorsAreDifferent<F> extends AbstractFactorsEqualityCheck<F> implements FactorsAreDifferent<F>{

	public AbstractFactorsAreDifferent(F first, F second) {
		super(first, second);
	}

}
