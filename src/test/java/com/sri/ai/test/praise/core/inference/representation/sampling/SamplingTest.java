package com.sri.ai.test.praise.core.inference.representation.sampling;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.Normal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.number.algorithm.MeanAndVariance;
import com.sri.ai.util.number.representation.core.ArithmeticDoubleFactory;

public class SamplingTest {

	@Test
	void test() {
		int numberOfSamples = 1024000;
		SamplingFactor factor;
		SamplingFactor normal;
		Variable variable;
		Sample sample;
		ImportanceFactory importanceFactory = new DoubleImportanceFactory();
		PotentialFactory potentialFactory = new DoublePotentialFactory();
		ArithmeticDoubleFactory numberFactory = new ArithmeticDoubleFactory();
		DefaultFactorNetwork network;
		ExactBP solver;
		MeanAndVariance statistics;
		
		variable = new DefaultVariable("x");
		normal = new Normal(variable, 10, 2, new DoublePotentialFactory(), new Random(0));
		network = new DefaultFactorNetwork(list(normal));
		solver = new ExactBP(variable, network);
		factor = (SamplingFactor) solver.apply();
		println(factor);
		statistics = new MeanAndVariance(numberFactory);
		for (numberOfSamples = 5000; numberOfSamples >= 5000; numberOfSamples /= 2) {
			for (int i = 0; i != numberOfSamples; i++) {
				sample = new DefaultSample(importanceFactory, potentialFactory);
				factor.sample(sample);
				statistics.add((Double) sample.getAssignment().get(variable));
			}
			println("# samples: " + numberOfSamples + ", Mean: " + statistics.getMean() + ", Variance: " + statistics.getVariance());
		}
	}
}
