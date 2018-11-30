package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample;

import com.sri.ai.util.number.representation.api.ArithmeticNumber;

public interface Potential extends ArithmeticNumber {

	@Override
	Potential add(ArithmeticNumber another);
	
	@Override
	Potential subtract(ArithmeticNumber another);
	
	@Override
	Potential multiply(ArithmeticNumber another);
	
	@Override
	Potential divide(ArithmeticNumber another);
	
	@Override
	Potential pow(ArithmeticNumber another);

}
