package com.sri.ai.test.praise.core.representation.interfacebased.factor.core.sampling;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.Random;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.random.JDKRandomGenerator;
import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.ImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ConstantSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SamplingProductFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.statistics.NormalWithFixedMeanAndStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;

public class ConstantSamplingFactorTest {

	private ImportanceFactory importanceFactory = new DoubleImportanceFactory();
	private PotentialFactory potentialFactory = new DoublePotentialFactory();
	private DefaultFactorNetwork network;
	private ExactBP solver;
	private SamplingFactor marginalOfX;
	
	@Test
	public void testConstantSamplingFactor() {
		
		long numberOfSamples = 10000;
		
		Variable x = new DefaultVariable("x");

		ConstantSamplingFactor factorOnX = new ConstantSamplingFactor(x, "a", new Random());

		runConstantSamplingFactorTest(numberOfSamples, x, factorOnX);

		network = new DefaultFactorNetwork(list(factorOnX));
		solver = new ExactBP(x, network);
		marginalOfX = (SamplingFactor) solver.apply();

		runConstantSamplingFactorTest(numberOfSamples, x, marginalOfX);

	}

	private void runConstantSamplingFactorTest(long numberOfSamples, Variable x, SamplingFactor factor) {
		println("Working with x = \"a\"");
		println("Generating " + numberOfSamples + " samples from nothing");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			// println(sample);
			assertEquals("a", sample.getAssignment().get(x));
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
		}

		println("Generating " + numberOfSamples + " samples from x = \"a\"");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "a");
			factor.sampleOrWeigh(sample);
			// println(sample);
			assertEquals("a", sample.getAssignment().get(x));
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
		}

		println("Generating " + numberOfSamples + " samples from x = \"b\"");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "b");
			factor.sampleOrWeigh(sample);
			// println(sample);
			assertEquals("b", sample.getAssignment().get(x));
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
		}
	}
	
	@Test
	public void testNormallyDistributedVariableWithDeterministicObservation() {
		
		long numberOfSamples = 5000;
		Random random = new Random();
		
		Variable x = new DefaultVariable("x");

		SamplingFactor xIsNormallyDistributed = new NormalWithFixedMeanAndStandardDeviation(x, 10, 0.5, random);
		SamplingFactor xEquals8 = new ConstantSamplingFactor(x, 8.0, new Random());

		SamplingFactor xIsNormallyDistributedAndEquals8 = new SamplingProductFactor(arrayList(xIsNormallyDistributed, xEquals8), random);
		println(xIsNormallyDistributedAndEquals8.nestedString(true));

		runNormallyDistributedVariableWithDeterministicObservationTest(numberOfSamples, x, xIsNormallyDistributedAndEquals8, random);

	}

	private void runNormallyDistributedVariableWithDeterministicObservationTest(long numberOfSamples, Variable x, SamplingFactor factor, Random random) {

		println("Working with x = Normal(10, 0.5) and x = 8.0");

		Sample sample = new DefaultSample(importanceFactory, potentialFactory);
		
		println("Generating " + numberOfSamples + " samples from nothing");
		for (int i = 0; i != numberOfSamples; i++) {
			sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			assertEquals(densityOf8GivenMean10AndStandardDeviation10(random), sample.getPotential().doubleValue(), 0.0000001);
			assertEquals(8.0, sample.getAssignment().get(x));
		}
	}

	public double densityOf8GivenMean10AndStandardDeviation10(Random random) {
		return new NormalDistribution(new JDKRandomGenerator(random.nextInt()), 10, 0.5).density(8.0);
	}
	
}
