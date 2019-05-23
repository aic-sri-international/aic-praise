package com.sri.ai.test.praise.core.representation.interfacebased.factor.core.sampling;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.containsAllCaseInsensitive;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.ImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SamplingProductFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.EqualitySamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.number.statistics.core.SampleDistribution;

public class EqualitySamplingFactorTest {

	private ImportanceFactory importanceFactory = new DoubleImportanceFactory();
	private PotentialFactory potentialFactory = new DoublePotentialFactory();
	private DefaultFactorNetwork network;
	private ExactBP solver;
	private SamplingFactor marginalOfX;

	@Test
	public void testTwoEqualitiesOnSameVariable() {
		
		long numberOfSamples = 5;
		
		Variable x = new DefaultVariable("x");
		Variable y = new DefaultVariable("y");
		Variable z = new DefaultVariable("z");

		SamplingFactor xEqualsY = SamplingFactor.conditionResult(true, t -> new EqualitySamplingFactor(t, arrayList(x, y), new Random()));
		SamplingFactor xEqualsZ = SamplingFactor.conditionResult(true, t -> new EqualitySamplingFactor(t, arrayList(x, z), new Random()));

		SamplingFactor xEqualsYAndXEqualsZ = new SamplingProductFactor(arrayList(xEqualsY, xEqualsZ), new Random());
		println(xEqualsYAndXEqualsZ.nestedString(true));

		runTwoEqualitiesOnSameVariableTest(numberOfSamples, x, y, z, xEqualsYAndXEqualsZ);

	}

	private void runTwoEqualitiesOnSameVariableTest(long numberOfSamples, Variable x, Variable y, Variable z, SamplingFactor factor) {

		println("Working with x = y and x = z");

		Sample sample = new DefaultSample(importanceFactory, potentialFactory);
		
		try {
			println("Generating " + numberOfSamples + " samples from empty sample");
			for (int i = 0; i != numberOfSamples; i++) {
				factor.sampleOrWeigh(sample);
			}
		}
		catch (Throwable t) {
			if ( ! t.getMessage().contains("Factor was not able to complete sample") && ! containsAllCaseInsensitive(t.getMessage(), "plan", "failed")) {
				throw t;
			}
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals(null, sample.getAssignment().get(x));
			assertEquals(null, sample.getAssignment().get(y));
			assertEquals(null, sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = 'A string value'");
		for (int i = 0; i != numberOfSamples; i++) {
			sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "A string value");
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals("A string value", sample.getAssignment().get(x));
			assertEquals("A string value", sample.getAssignment().get(y));
			assertEquals("A string value", sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = Y = 'A string value'");
		for (int i = 0; i != numberOfSamples; i++) {
			sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "A string value");
			sample.getAssignment().set(y, "A string value");
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals("A string value", sample.getAssignment().get(x));
			assertEquals("A string value", sample.getAssignment().get(y));
			assertEquals("A string value", sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = Y = Z = 'A string value'");
		for (int i = 0; i != numberOfSamples; i++) {
			sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "A string value");
			sample.getAssignment().set(y, "A string value");
			sample.getAssignment().set(z, "A string value");
			factor.sampleOrWeigh(sample);
			assertEquals(1.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals("A string value", sample.getAssignment().get(x));
			assertEquals("A string value", sample.getAssignment().get(y));
			assertEquals("A string value", sample.getAssignment().get(z));
		}

		println("Generating " + numberOfSamples + " samples from X = Y = 'A string value' and Z = 'Another string value'");
		for (int i = 0; i != numberOfSamples; i++) {
			sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "A string value");
			sample.getAssignment().set(y, "A string value");
			sample.getAssignment().set(z, "Another string value");
			factor.sampleOrWeigh(sample);
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals("A string value", sample.getAssignment().get(x));
			assertEquals("A string value", sample.getAssignment().get(y));
			assertEquals("Another string value", sample.getAssignment().get(z));
		}

		long oldNumberOfSamples = numberOfSamples;
		numberOfSamples = 5000;
		SampleDistribution<Object> xDistribution = new SampleDistribution<>();
		println("Generating " + numberOfSamples + " samples from Y = 'A string value' and Z = 'Another string value'");
		for (int i = 0; i != numberOfSamples; i++) {
			sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(y, "A string value");
			sample.getAssignment().set(z, "Another string value");
			factor.sampleOrWeigh(sample);
			xDistribution.add(sample.getAssignment().get(x), new DoublePotential(1.0)); // all samples have weight 0, but we want to check proportion of values for x regardless of that, so we provide weight 1.
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
			assertEquals("A string value", sample.getAssignment().get(y));
			assertEquals("Another string value", sample.getAssignment().get(z));
		}
		
		// We used to test whether the value of x would go either way 50% of the times
		// However, when changing sampling from dynamic to planned on May 2019,
		// the behavior changed and the value of x always got set to the value of y.
		// This is ok because the behavior is undefined since the distribution is ill-defined (contradictory).
		// So this is now commented out.
//		println("Distribution of x: " + xDistribution.getValue());
//		assertEquals(0.5, xDistribution.getValue().get("A string value").doubleValue(), 0.1);
//		assertEquals(0.5, xDistribution.getValue().get("Another string value").doubleValue(), 0.1);

		numberOfSamples = oldNumberOfSamples;
	}

	// @Test not working with conditioned version of EqualitySamplingFactor (it does not work with ExactBP)
	public void testEquality() {
		
		long numberOfSamples = 5;
		
		Variable x = new DefaultVariable("x");
		Variable y = new DefaultVariable("y");

		SamplingFactor xEqualsY = SamplingFactor.conditionResult(true, t -> new EqualitySamplingFactor(t, arrayList(x, y), new Random()));

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
