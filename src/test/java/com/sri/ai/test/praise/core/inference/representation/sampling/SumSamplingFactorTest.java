package com.sri.ai.test.praise.core.inference.representation.sampling;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.ImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SumSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.number.representation.core.ArithmeticDouble;

public class SumSamplingFactorTest {

	private ImportanceFactory importanceFactory = new DoubleImportanceFactory();
	private PotentialFactory potentialFactory = new DoublePotentialFactory();
	private DefaultFactorNetwork network;
	private ExactBP solver;
	private SamplingFactor marginalOfX;
	
	@Test
	public void testSumOfThreeSampling() {
		
		long numberOfSamples = 5;
		
		Variable x = new DefaultVariable("x");
		Variable y = new DefaultVariable("y");
		Variable z = new DefaultVariable("z");

		SumSamplingFactor xEqualsYPlusZ = new SumSamplingFactor(x, list(y,z), new DoublePotentialFactory());

		runSumOfThreeTest(numberOfSamples, x, y, z, xEqualsYPlusZ);

		network = new DefaultFactorNetwork(list(xEqualsYPlusZ));
		solver = new ExactBP(x, network);
		marginalOfX = (SamplingFactor) solver.apply();

		runSumOfThreeTest(numberOfSamples, x, y, z, marginalOfX);

	}

	private void runSumOfThreeTest(long numberOfSamples, Variable x, Variable y, Variable z, SamplingFactor factor) {
		println("Generating " + numberOfSamples + " samples from Y = 1.0 and Z = 2.0");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(y, new ArithmeticDouble(1.0));
			sample.getAssignment().set(z, new ArithmeticDouble(2.0));
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(3.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(y));
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 and Z = 2.0");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			sample.getAssignment().set(z, new ArithmeticDouble(2.0));
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(-1.0), sample.getAssignment().get(y));
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 and nothing else");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(null, sample.getAssignment().get(y));
			assertEquals(null, sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0, Y = -1.0 and Z = 2.0 (consistent sum)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			sample.getAssignment().set(y, new ArithmeticDouble(-1.0));
			sample.getAssignment().set(z, new ArithmeticDouble(2.0));
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(-1.0), sample.getAssignment().get(y));
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0, Y = 1.0 and Z = 2.0 (inconsistent sum)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			sample.getAssignment().set(y, new ArithmeticDouble(1.0));
			sample.getAssignment().set(z, new ArithmeticDouble(2.0));
			factor.sampleOrWeigh(sample);
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(y));
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(z));
		}
	}

	
	@Test
	public void testSumOfNoneSampling() {
		
		long numberOfSamples = 5;
		
		Variable x = new DefaultVariable("x");

		SumSamplingFactor xEqualsSumOfNone = new SumSamplingFactor(x, list(), new DoublePotentialFactory());

		runSumOfNoneTest(numberOfSamples, x, xEqualsSumOfNone);

		network = new DefaultFactorNetwork(list(xEqualsSumOfNone));
		solver = new ExactBP(x, network);
		marginalOfX = (SamplingFactor) solver.apply();

		runSumOfNoneTest(numberOfSamples, x, marginalOfX);

	}

	private void runSumOfNoneTest(long numberOfSamples, Variable x, SamplingFactor factor) {
		println("Generating " + numberOfSamples + " samples from nothing");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(0.0), sample.getAssignment().get(x));
		}

		println("Generating " + numberOfSamples + " samples from X = 0.0 (consistent sum)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(0.0), sample.getAssignment().get(x));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 (inconsistent sum)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
		}
	}
}
