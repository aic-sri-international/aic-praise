package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution;

import static com.sri.ai.util.Util.arrayList;

import java.util.Random;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.random.JDKRandomGenerator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractSamplingFactor;

public class Normal extends AbstractSamplingFactor {

	private Variable variable;
	
	private NormalDistribution normalDistribution;
	
	private PotentialFactory potentialFactory;
	
	public Normal(Variable variable, double mean, double standardDeviation, PotentialFactory potentialFactory, Random random) {
		super(arrayList(variable), random);
		this.variable = variable;
		this.potentialFactory = potentialFactory;
		this.normalDistribution = new NormalDistribution(new JDKRandomGenerator(random.nextInt()), mean, standardDeviation);
	}

	@Override
	public void sample(Sample sample) {
		Double value = getValue(sample);
		Potential potentialUpdate = getPotentialUpdate(value);
		sample.updatePotential(potentialUpdate);
	}

	private Double getValue(Sample sample) {
		Double value = ((Double) sample.getAssignment().get(variable));
		if (value == null) {
			value = normalDistribution.sample();
			sample.getAssignment().set(variable, value);
		}
		return value;
	}

	private Potential getPotentialUpdate(Double value) {
		double density = normalDistribution.density(value.doubleValue());
		Potential potentialFactor = potentialFactory.make(density);
		return potentialFactor;
	}

	@Override
	public String toString() {
		return "Normal(" + normalDistribution.getMean() + ", " + normalDistribution.getStandardDeviation() + ")";
	}
}
