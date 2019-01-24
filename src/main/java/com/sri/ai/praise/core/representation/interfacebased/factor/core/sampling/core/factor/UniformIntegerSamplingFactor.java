package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.util.Util.myAssert;

import java.util.Random;
import java.util.function.Supplier;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;

/**
 * A sampling factor for a uniform distribution over an integer interval.
 *  
 * @author braz
 *
 */
public class UniformIntegerSamplingFactor extends UniformSamplingFactor<Integer> {

	private int first;
	private int last;
	
	public UniformIntegerSamplingFactor(
			Variable variable, 
			int first,
			int last,
			PotentialFactory potentialFactory,
			Random random) {
		
		super(
				variable, 
				makeSampler(first, last, random), 
				makePotentialForEachValue(first, last, potentialFactory), 
				makeEstimatedSuccessWeight(first, last), 
				random);
		
		this.first = first;
		this.last = last;
		
		myAssert(last >= first, () -> getClass() + " requires non-empty integer interval, but got " + first + ".." + last);
		
	}

	private static Supplier<Integer> makeSampler(int first, int last, Random random) {
		return () -> random.nextInt(last - first) + first;
	}

	private static Potential makePotentialForEachValue(int first, int last, PotentialFactory potentialFactory) {
		return potentialFactory.make(1.0/(last - first + 1));
	}

	private static double makeEstimatedSuccessWeight(int first, int last) {
		return 1.0/(last - first + 1);
	}

	@Override
	public String toString() {
		return getVariables().get(0) + " = Uniform(" + first + ".." + last + ")";
	}

}
