package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.util.Util.myAssert;

import java.util.Random;
import java.util.function.Supplier;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;

/**
 * A sampling factor for a uniform distribution over a real interval.
 *  
 * @author braz
 *
 */
public class UniformRealSamplingFactor extends UniformSamplingFactor<Double> {

	private double first;
	private double last;

	/**
	 * Defines a uniform distribution for given variable in interval [first; last[.
	 * @param variable
	 * @param first
	 * @param last
	 * @param potentialFactory
	 * @param random
	 */
	public UniformRealSamplingFactor(
			Variable variable, 
			double first,
			double last,
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
		
		myAssert(last >= first, () -> getClass() + " requires non-empty real interval, but got [" + first + ";" + last + "[");
		
	}

	private static Supplier<Double> makeSampler(double first, double last, Random random) {
		return () -> random.nextDouble()*(last - first) + first;
	}

	private static Potential makePotentialForEachValue(double first, double last, PotentialFactory potentialFactory) {
		return potentialFactory.make(1.0/(last - first));
	}

	private static double makeEstimatedSuccessWeight(double first, double last) {
		return 1.0/(last - first);
	}

	@Override
	public String toString() {
		return getVariables().get(0) + " = Uniform([" + first + ";" + last + "[)";
	}

}
