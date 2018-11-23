package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;

public class SamplingFactorNetwork extends DefaultFactorNetwork {

	public SamplingFactorNetwork(List<? extends Factor> factors) {
		super(factors);
	}

}
