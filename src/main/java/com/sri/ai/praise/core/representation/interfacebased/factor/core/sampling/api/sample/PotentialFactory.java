package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample;

import com.sri.ai.util.number.representation.api.ArithmeticNumberFactory;

public interface PotentialFactory extends ArithmeticNumberFactory {
	
	Potential make(double value);

}
