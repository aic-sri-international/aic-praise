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

public class NormalWithFixedStandardDeviation extends AbstractSamplingFactor {

	private Variable variable;
	
	private Variable mean;
	
	private double standardDeviation;
	
	private PotentialFactory potentialFactory;
	
	private JDKRandomGenerator randomGenerator;
	
	public NormalWithFixedStandardDeviation(Variable variable, Variable mean, double standardDeviation, PotentialFactory potentialFactory, Random random) {
		super(arrayList(variable, mean), random);
		this.variable = variable;
		this.mean = mean;
		this.standardDeviation = standardDeviation;
		this.potentialFactory = potentialFactory;
		this.randomGenerator = new JDKRandomGenerator(random.nextInt());
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		Double meanValue = getValue(sample, mean);
		if (meanValue != null) {
			sampleOrWeightGivenMean(variable, meanValue, sample);
		}
		else {
			sampleOrWeightWithUnknownMean(sample);
		}
	}

	private void sampleOrWeightGivenMean(Variable sampledOrWeighedVariable, double meanValue, Sample sample) {
		Double value = getValue(sample, sampledOrWeighedVariable);
		if (value == null) {
			sample(sampledOrWeighedVariable, meanValue, sample);
		}
		else {
			weigh(value, meanValue, sample);
		}
	}

	private void sample(Variable sampledOrWeighedVariable, double meanValue, Sample sample) {
		Double value = sampleFromMean(meanValue);
		setValue(sample, sampledOrWeighedVariable, value);
	}

	private void weigh(Double sampledValue, double meanValue, Sample sample) {
		double density = densityFromMean(sampledValue, meanValue);
		weight(sample, density);
	}

	private void sampleOrWeightWithUnknownMean(Sample sample) {
		Double variableValue = getValue(sample, variable);
		if (variableValue != null) {
			sampleOrWeightMeanInInvertedProblem(variableValue, sample);
		}
		// else do nothing as we have no information to sample
		// TODO: when engagement rules are in place, we should never get to this point and throw an error
	}

	private void sampleOrWeightMeanInInvertedProblem(Double variableValue, Sample sample) {
		Double meanValueInInvertedProblem = variableValue;
		Variable sampledOrWeighedVariableInInvertedProblem = mean;
		sampleOrWeightGivenMean(sampledOrWeighedVariableInInvertedProblem, meanValueInInvertedProblem, sample);
	}
	
	private void weight(Sample sample, double density) {
		Potential potentialFactor = potentialFactory.make(density);
		sample.updatePotential(potentialFactor);
	}

	private double sampleFromMean(double meanValue) {
		return getNormalDistribution(meanValue).sample();
	}

	private double densityFromMean(Double sampledValue, double meanValue) {
		return getNormalDistribution(meanValue).density(sampledValue);
	}

	protected NormalDistribution getNormalDistribution(double meanValue) {
		return new NormalDistribution(randomGenerator, meanValue, standardDeviation);
	}

	private Double getValue(Sample sample, Variable variable) {
		Double result = (Double) sample.getAssignment().get(variable);
		return result;
	}

	private void setValue(Sample sample, Variable variable, Double value) {
		sample.getAssignment().set(variable, value);
	}

	@Override
	public SamplingRules makeSamplingRules() {
		SamplingRule fromMeanToDependent = samplingRule(this, list(variable), list(mean), 0.5);
		SamplingRule fromDependentToMean = samplingRule(this, list(mean), list(variable), 0.5);
		return new DefaultSamplingRules(fromDependentToMean, fromMeanToDependent);
	}

	@Override
	public String toString() {
		return variable + " = Normal(" + mean + ", " + Math.pow(standardDeviation, 2) + ")";
	}
}