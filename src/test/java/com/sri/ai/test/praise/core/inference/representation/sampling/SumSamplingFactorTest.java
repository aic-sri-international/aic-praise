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
	public void testSumOfDistinctThreeSampling() {
		
		long numberOfSamples = 5;
		
		Variable x = new DefaultVariable("x");
		Variable y = new DefaultVariable("y");
		Variable z = new DefaultVariable("z");

		SumSamplingFactor xEqualsYPlusZ = new SumSamplingFactor(x, list(y,z));

		runSumOfDistinctThreeTest(numberOfSamples, x, y, z, xEqualsYPlusZ);

		network = new DefaultFactorNetwork(list(xEqualsYPlusZ));
		solver = new ExactBP(x, network);
		marginalOfX = (SamplingFactor) solver.apply();

		runSumOfDistinctThreeTest(numberOfSamples, x, y, z, marginalOfX);

	}

	private void runSumOfDistinctThreeTest(long numberOfSamples, Variable x, Variable y, Variable z, SamplingFactor factor) {
		println("Working with x = y + z");
		println("Generating " + numberOfSamples + " samples from nothing");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(null, sample.getAssignment().get(x));
			assertEquals(null, sample.getAssignment().get(y));
			assertEquals(null, sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 and nothing else");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(null, sample.getAssignment().get(y));
			assertEquals(null, sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from Y = 1.0 and nothing else");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(y, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(null, sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(y));
			assertEquals(null, sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from Y = 1.0 and Z = 2.0");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(y, new ArithmeticDouble(1.0));
			sample.getAssignment().set(z, new ArithmeticDouble(2.0));
			factor.sampleOrWeigh(sample);
			println(sample);
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
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(-1.0), sample.getAssignment().get(y));
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0, Y = -1.0 and Z = 2.0 (consistent sum)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			sample.getAssignment().set(y, new ArithmeticDouble(-1.0));
			sample.getAssignment().set(z, new ArithmeticDouble(2.0));
			factor.sampleOrWeigh(sample);
			println(sample);
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
			println(sample);
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(y));
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(z));
		}
	}

	@Test
	public void testSumOfRepeatingArgumentThreeSampling() {
		
		long numberOfSamples = 5;
		
		Variable x = new DefaultVariable("x");
		Variable y = new DefaultVariable("y");

		SumSamplingFactor xEqualsYPlusY = new SumSamplingFactor(x, list(y,y));

		runSumOfRepeatingArgumentTest(numberOfSamples, x, y, xEqualsYPlusY);

		network = new DefaultFactorNetwork(list(xEqualsYPlusY));
		solver = new ExactBP(x, network);
		marginalOfX = (SamplingFactor) solver.apply();

		runSumOfRepeatingArgumentTest(numberOfSamples, x, y, marginalOfX);

	}

	private void runSumOfRepeatingArgumentTest(long numberOfSamples, Variable x, Variable y, SamplingFactor factor) {
		println("Working with x = y + y");
		println("Generating " + numberOfSamples + " samples from nothing");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(null, sample.getAssignment().get(x));
			assertEquals(null, sample.getAssignment().get(y));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 and nothing else");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(null, sample.getAssignment().get(y));
		}

		println("Generating " + numberOfSamples + " samples from Y = 1.0 and nothing else");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(y, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(y));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 and Y = 2.0");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			sample.getAssignment().set(y, new ArithmeticDouble(2.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(y));
		}

		println("Generating " + numberOfSamples + " samples from X = 2.0 and Y = 1.0");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(2.0));
			sample.getAssignment().set(y, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(y));
		}
	}

	@Test
	public void testSumOfRepeatingSumVariableThreeSampling() {
		
		long numberOfSamples = 5;
		
		Variable x = new DefaultVariable("x");
		Variable y = new DefaultVariable("y");

		SumSamplingFactor xEqualsXPlusY = new SumSamplingFactor(x, list(x,y));

		runSumOfRepeatingSumVariableTest(numberOfSamples, x, y, xEqualsXPlusY);

		network = new DefaultFactorNetwork(list(xEqualsXPlusY));
		solver = new ExactBP(x, network);
		marginalOfX = (SamplingFactor) solver.apply();

		runSumOfRepeatingSumVariableTest(numberOfSamples, x, y, marginalOfX);

	}

	private void runSumOfRepeatingSumVariableTest(long numberOfSamples, Variable x, Variable y, SamplingFactor factor) {
		println("Working with x = x + y");
		println("Generating " + numberOfSamples + " samples from nothing");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(null, sample.getAssignment().get(x));
			assertEquals(null, sample.getAssignment().get(y));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 and nothing else");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(0.0), sample.getAssignment().get(y));
		}

		println("Generating " + numberOfSamples + " samples from Y = 1.0 and nothing else (consistent but requires equation-solving, so does nothing)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(y, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(null, sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(y));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 and Y = 2.0 (inconsistent)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			sample.getAssignment().set(y, new ArithmeticDouble(2.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(2.0), sample.getAssignment().get(y));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 and Y = 0.0 (consistent)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			sample.getAssignment().set(y, new ArithmeticDouble(0.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(0.0), sample.getAssignment().get(y));
		}

		println("Generating " + numberOfSamples + " samples from X = 0.0 and Y = 1.0 (inconsistent)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(0.0));
			sample.getAssignment().set(y, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(0.0), sample.getAssignment().get(x));
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(y));
		}
	}

	@Test
	public void testSumOfNoneSampling() {
		
		long numberOfSamples = 5;
		
		Variable x = new DefaultVariable("x");

		SumSamplingFactor xEqualsSumOfNone = new SumSamplingFactor(x, list());

		runSumOfNoneTest(numberOfSamples, x, xEqualsSumOfNone);

		network = new DefaultFactorNetwork(list(xEqualsSumOfNone));
		solver = new ExactBP(x, network);
		marginalOfX = (SamplingFactor) solver.apply();

		runSumOfNoneTest(numberOfSamples, x, marginalOfX);

	}

	private void runSumOfNoneTest(long numberOfSamples, Variable x, SamplingFactor factor) {
		println("Working with x = +()");
		println("Generating " + numberOfSamples + " samples from nothing");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(0.0), sample.getAssignment().get(x));
		}

		println("Generating " + numberOfSamples + " samples from X = 0.0 (consistent sum)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(0.0), sample.getAssignment().get(x));
		}

		println("Generating " + numberOfSamples + " samples from X = 1.0 (inconsistent sum)");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, new ArithmeticDouble(1.0));
			factor.sampleOrWeigh(sample);
			println(sample);
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(new ArithmeticDouble(1.0), sample.getAssignment().get(x));
		}
	}
	
//	@Test
//	public void testSamplingRules() {
//		
//		long numberOfSamples = 5;
//
//		Random random = new Random();
//		Variable x = new DefaultVariable("x");
//		Variable y = new DefaultVariable("y");
//		
//
//		SumSamplingFactor xEqualsY = new SumSamplingFactor(x, list(y), new DoublePotentialFactory());
//		NormalWithFixedMeanAndStandardDeviation normalOnY = new NormalWithFixedMeanAndStandardDeviation(y, 10, 0.1, new DoublePotentialFactory(), random);
//
//		network = new DefaultFactorNetwork(list(xEqualsY, normalOnY));
//		solver = new ExactBP(x, network);
//		marginalOfX = (SamplingFactor) solver.apply();
//	}
}
