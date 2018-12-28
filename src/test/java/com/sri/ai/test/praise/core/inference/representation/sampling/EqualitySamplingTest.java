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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.Equality;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;

public class EqualitySamplingTest {

	private ImportanceFactory importanceFactory = new DoubleImportanceFactory();
	private PotentialFactory potentialFactory = new DoublePotentialFactory();
	private DefaultFactorNetwork network;
	private ExactBP solver;
	private SamplingFactor marginalOfX;
	
	@Test
	public void testEquality() {
		
		long numberOfSamples = 5;
		
		Variable x = new DefaultVariable("x");
		Variable y = new DefaultVariable("y");

		Equality xEqualsY = new Equality(x, y, new DoublePotentialFactory());

		runEqualityTest(numberOfSamples, x, y, xEqualsY);

		network = new DefaultFactorNetwork(list(xEqualsY));
		solver = new ExactBP(x, network);
		marginalOfX = (SamplingFactor) solver.apply();

		runEqualityTest(numberOfSamples, x, y, marginalOfX);

	}

	private void runEqualityTest(long numberOfSamples, Variable x, Variable y, SamplingFactor factor) {
		println("Generating " + numberOfSamples + " samples from X = 'A string value'");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "A string value");
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals("A string value", sample.getAssignment().get(x));
			assertEquals("A string value", sample.getAssignment().get(y));
		}
		
		println("Generating " + numberOfSamples + " samples from Y = 'A string value'");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(y, "A string value");
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals("A string value", sample.getAssignment().get(x));
			assertEquals("A string value", sample.getAssignment().get(y));
		}
		
		println("Generating " + numberOfSamples + " samples from X = 'A string value' and Y = 'A string value'");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "A string value");
			sample.getAssignment().set(y, "A string value");
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals("A string value", sample.getAssignment().get(x));
			assertEquals("A string value", sample.getAssignment().get(y));
		}
		
		println("Generating " + numberOfSamples + " samples from X = 'A string value' and Y = 'Another string value'");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "A string value");
			sample.getAssignment().set(y, "Another string value");
			factor.sampleOrWeigh(sample);
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals("A string value", sample.getAssignment().get(x));
			assertEquals("Another string value", sample.getAssignment().get(y));
		}
		
		println("Generating " + numberOfSamples + " samples from empty sample");
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(null, sample.getAssignment().get(x));
			assertEquals(null, sample.getAssignment().get(y));
		}
	}
}
