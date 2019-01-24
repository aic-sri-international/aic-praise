package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.util.Util.myAssert;

import java.util.Random;
import java.util.function.Supplier;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;

/**
 * A sampling factor for a uniform distribution over a given discrete set.
 *  
 * @author braz
 *
 */
public class UniformDiscreteSamplingFactor<T> extends UniformSamplingFactor<T> {

	private int size;
	
	public UniformDiscreteSamplingFactor(
			Variable variable, 
			Supplier<T> sampler,
			int size,
			PotentialFactory potentialFactory,
			Random random) {
		
		super(
				variable, 
				sampler, 
				makePotentialForEachValue(size, potentialFactory), 
				makeEstimatedSuccessWeight(size), 
				random);
	
		this.size = size;
		
		myAssert( size != 0, () -> getClass() + " requires non-empty discrete set, but got one of size " + size);
		
	}

	private static <T1> Potential makePotentialForEachValue(int size, PotentialFactory potentialFactory) {
		return potentialFactory.make(1.0/size);
	}

	private static <T1> double makeEstimatedSuccessWeight(int size) {
		return 1.0/(size);
	}

	@Override
	public String toString() {
		return getVariables().get(0) + " = Uniform from discrete set of size " + size + " with black box sampler";
	}

}
