package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.util.number.representation.core.ArithmeticDoubleFactory;

public class DoublePotentialFactory implements PotentialFactory {

	@Override
	public Potential make(double value) {
		DoublePotential result = new DoublePotential(ArithmeticDoubleFactory.INSTANCE.make(value));
		return result;
	}

}
