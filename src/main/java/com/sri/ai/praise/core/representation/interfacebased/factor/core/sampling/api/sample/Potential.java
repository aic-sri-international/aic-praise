package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample;

import com.sri.ai.util.number.representation.api.ArithmeticNumber;

public interface Potential extends ArithmeticNumber {

	Potential add(ArithmeticNumber another);
	
	Potential subtract(ArithmeticNumber another);
	
	Potential multiply(ArithmeticNumber another);
	
	Potential divide(ArithmeticNumber another);
	
	Potential pow(ArithmeticNumber another);

}
