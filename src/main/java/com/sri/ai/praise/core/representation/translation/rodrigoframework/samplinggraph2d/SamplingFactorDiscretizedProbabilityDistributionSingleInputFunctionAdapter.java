package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import com.sri.ai.util.function.api.variables.Assignment;
import com.sri.ai.util.function.core.functions.FunctionToSingleInputFunctionAdapter;
import com.sri.ai.util.graph2d.api.GraphSetMaker;

/**
 * Adds {@link com.sri.ai.util.function.api.functions.Function} functionality
 * to {@link SamplingFactorDiscretizedProbabilityDistribution},
 * equipping it with {@link #evaluate(Assignment)} method.
 * Among other things, this allows it to be used by {@link GraphSetMaker}.
 * <p>
 * It also defines a {@link #sample()} method that further samples the inner {@link SamplingFactorDiscretizedProbabilityDistribution}.
 */ 
public class SamplingFactorDiscretizedProbabilityDistributionSingleInputFunctionAdapter extends FunctionToSingleInputFunctionAdapter {
	
	public SamplingFactorDiscretizedProbabilityDistributionSingleInputFunctionAdapter(
			SamplingFactorDiscretizedProbabilityDistributionFunction base) {
		
		super(base);
	}
	
	@Override
	public SamplingFactorDiscretizedProbabilityDistributionFunction getBase() {
		return (SamplingFactorDiscretizedProbabilityDistributionFunction) super.getBase();
	}
	
	public void sample() {
		getBase().sample();
	}

}
