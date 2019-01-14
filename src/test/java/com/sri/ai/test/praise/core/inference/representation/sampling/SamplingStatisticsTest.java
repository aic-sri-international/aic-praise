package com.sri.ai.test.praise.core.inference.representation.sampling;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.ImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedMeanAndStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;
import com.sri.ai.util.number.representation.core.ArithmeticDoubleFactory;
import com.sri.ai.util.number.statistics.api.Statistic;
import com.sri.ai.util.number.statistics.core.CompoundStatistic;
import com.sri.ai.util.number.statistics.core.DefaultMean;
import com.sri.ai.util.number.statistics.core.MeanAndVariance;
import com.sri.ai.util.number.statistics.core.Variance;

public class SamplingStatisticsTest {

	private static Random random = new Random();
	
	int numberOfSamples = 10000000;

	SamplingFactor factor;
	SamplingFactor normal;
	Variable variable;
	Sample sample;
	ImportanceFactory importanceFactory = new DoubleImportanceFactory();
	PotentialFactory potentialFactory = new DoublePotentialFactory();
	ArithmeticDoubleFactory numberFactory = new ArithmeticDoubleFactory();
	DefaultFactorNetwork network;
	ExactBP solver;
	MeanAndVariance meanAndVariance;
	Statistic<ArithmeticNumber, ArithmeticNumber> varianceOfMean;

	@Test
	void testNormalWithFixedMeanAndStandardDeviation() {

		variable = new DefaultVariable("x");
		normal = new NormalWithFixedMeanAndStandardDeviation(variable, 10, 2, random);
		network = new DefaultFactorNetwork(list(normal));
		solver = new ExactBP(variable, network);
		factor = (SamplingFactor) solver.apply();

		println(factor);

		meanAndVariance = new MeanAndVariance(numberFactory);
		varianceOfMean = CompoundStatistic.<ArithmeticNumber, ArithmeticNumber, ArithmeticNumber>chain(
				new DefaultMean(numberFactory), 
				new Variance(numberFactory));
		
		for (int i = 0; i != numberOfSamples; i++) {
			sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			Double xValue = (Double) sample.getAssignment().get(variable);
			meanAndVariance.add(numberFactory.make(xValue), sample.getPotential());
			varianceOfMean.add(numberFactory.make(xValue), sample.getPotential());
		}

		println("# samples: " + numberOfSamples + ", Mean: " + meanAndVariance.getMean() + ", Variance: " + meanAndVariance.getVariance() + ", Variance of mean: " + varianceOfMean.getValue());
		assertTrue(meanAndVariance.getMean().doubleValue() > 9.8 && meanAndVariance.getMean().doubleValue() < 10.2);
		assertTrue(meanAndVariance.getVariance().doubleValue() > 3.8 && meanAndVariance.getVariance().doubleValue() < 4.2);

	}
}
