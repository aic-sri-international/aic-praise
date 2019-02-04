package com.sri.ai.test.praise.core.representation.sampling;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.TableSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.number.statistics.core.SampleDistribution;

public class TableSamplingFactorTest {

	private ImportanceFactory importanceFactory = new DoubleImportanceFactory();
	private PotentialFactory potentialFactory = new DoublePotentialFactory();
	private DefaultFactorNetwork network;
	private ExactBP solver;
	private SamplingFactor marginalOfX;
	
	@Test
	public void testTableSamplingFactor() {
		
		long numberOfSamples = 10000;
		
		Variable x = new DefaultVariable("x");

		TableSamplingFactor factorOnX = 
				new TableSamplingFactor(x, 
						arrayList("a", "b", "c"), 
						arrayList(0.1, 0.2, 0.7), 
						new Random());

		runTableSamplingFactorTest(numberOfSamples, x, factorOnX);

		network = new DefaultFactorNetwork(list(factorOnX));
		solver = new ExactBP(x, network);
		marginalOfX = (SamplingFactor) solver.apply();

		runTableSamplingFactorTest(numberOfSamples, x, marginalOfX);

	}

	private void runTableSamplingFactorTest(long numberOfSamples, Variable x, SamplingFactor factor) {
		println("Working with x in a, b, c with probabilities 0.1, 0.2, 0.7");
		println("Generating " + numberOfSamples + " samples from nothing");
		SampleDistribution<Object> sampleDistribution = new SampleDistribution<>();
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			factor.sampleOrWeigh(sample);
			// println(sample);
			sampleDistribution.add(sample.getAssignment().get(x), sample.getPotential());
		}
		println("Sample distribution: " + sampleDistribution);
		assertEquals(0.1, sampleDistribution.getValue().get("a").doubleValue(), 0.1);
		assertEquals(0.2, sampleDistribution.getValue().get("b").doubleValue(), 0.1);
		assertEquals(0.7, sampleDistribution.getValue().get("c").doubleValue(), 0.1);

		println("Generating " + numberOfSamples + " samples from X = \"a\"");
		sampleDistribution = new SampleDistribution<>();
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "a");
			factor.sampleOrWeigh(sample);
			// println(sample);
			sampleDistribution.add(sample.getAssignment().get(x), sample.getPotential());
		}
		println("Sample distribution: " + sampleDistribution);
		assertEquals(1.0, sampleDistribution.getValue().get("a").doubleValue(), 0.0);
		assertEquals(null, sampleDistribution.getValue().get("b"));
		assertEquals(null, sampleDistribution.getValue().get("c"));

		println("Generating " + numberOfSamples + " samples from X = \"d\" (invalid value)");
		sampleDistribution = new SampleDistribution<>();
		for (int i = 0; i != numberOfSamples; i++) {
			Sample sample = new DefaultSample(importanceFactory, potentialFactory);
			sample.getAssignment().set(x, "d");
			factor.sampleOrWeigh(sample);
			// println(sample);
			sampleDistribution.add(sample.getAssignment().get(x), sample.getPotential());
			assertEquals("d", sample.getAssignment().get(x));
			assertEquals(0.0, sample.getPotential().doubleValue(), 0.0);
		}
		println("Sample distribution: " + sampleDistribution);
	}
}
