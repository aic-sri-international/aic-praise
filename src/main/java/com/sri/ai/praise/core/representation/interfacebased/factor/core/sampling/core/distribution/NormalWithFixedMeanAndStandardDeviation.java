package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRule;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import java.util.Random;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.random.JDKRandomGenerator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRules;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.samplingrules.DefaultSamplingRules;

public class NormalWithFixedMeanAndStandardDeviation extends AbstractSamplingFactor {

	private Variable variable;
	
	private NormalDistribution normalDistribution;
	
	private PotentialFactory potentialFactory;
	
	public NormalWithFixedMeanAndStandardDeviation(Variable variable, double mean, double standardDeviation, PotentialFactory potentialFactory, Random random) {
		super(arrayList(variable), random);
		this.variable = variable;
		this.potentialFactory = potentialFactory;
		this.normalDistribution = new NormalDistribution(new JDKRandomGenerator(random.nextInt()), mean, standardDeviation);
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		Double value = getValue(sample);
		if (value == null) {
			sample(sample);
		}
		else {
			weigh(value, sample);
		}
	}

	private void sample(Sample sample) {
		Double value = normalDistribution.sample();
		setValue(sample, value);
	}

	private void weigh(Double value, Sample sample) {
		Potential potentialUpdate = getPotentialUpdate(value);
		sample.updatePotential(potentialUpdate);
	}

	private Potential getPotentialUpdate(Double value) {
		double density = normalDistribution.density(value.doubleValue());
		Potential potentialFactor = potentialFactory.make(density);
		return potentialFactor;
	}

	private Double getValue(Sample sample) {
		return (Double) sample.getAssignment().get(variable);
	}

	private void setValue(Sample sample, Double value) {
		sample.getAssignment().set(variable, value);
	}

	@Override
	public SamplingRules makeSamplingRules() {
		SamplingRule samplingRule = samplingRule(this, getVariables(), list(), 0.5);
		return new DefaultSamplingRules(samplingRule);
	}

	@Override
	public String toString() {
		return variable + " = Normal(" + normalDistribution.getMean() + ", " + Math.pow(normalDistribution.getStandardDeviation(), 2) + ")";
	}
}
