package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Importance;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.ImportanceFactory;
import com.sri.ai.util.number.representation.core.ArithmeticDoubleFactory;

public class DoubleImportanceFactory implements ImportanceFactory {

	@Override
	public Importance make(double value) {
		DoubleImportance result = new DoubleImportance(ArithmeticDoubleFactory.INSTANCE.make(value));
		return result;
	}

}
