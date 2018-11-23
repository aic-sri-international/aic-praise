package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

/**
 * A sampling factor is a factor that represents its potential function by a set of samples.
 * 
 * @author braz
 *
 */
public interface SamplingFactor extends Factor {
	
	void sample(Sample sample);

}
