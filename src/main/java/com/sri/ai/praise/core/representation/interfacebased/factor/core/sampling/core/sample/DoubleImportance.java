package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Importance;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;
import com.sri.ai.util.number.representation.core.ArithmeticDouble;

public class DoubleImportance extends ArithmeticDouble implements Importance {

	public DoubleImportance(ArithmeticDouble another) {
		super(another);
	}

	private DoubleImportance wrapper(ArithmeticDouble another) {
		return new DoubleImportance(another);
	}
	
	@Override
	public DoubleImportance add(ArithmeticNumber another) {
		return wrapper(super.add(another));
	}

	@Override
	public DoubleImportance subtract(ArithmeticNumber another) {
		return wrapper(super.subtract(another));
	}

	@Override
	public DoubleImportance multiply(ArithmeticNumber another) {
		return wrapper(super.multiply(another));
	}

	@Override
	public DoubleImportance divide(ArithmeticNumber another) {
		return wrapper(super.divide(another));
	}

	@Override
	public DoubleImportance pow(ArithmeticNumber another) {
		return wrapper(super.pow(another));
	}

}
