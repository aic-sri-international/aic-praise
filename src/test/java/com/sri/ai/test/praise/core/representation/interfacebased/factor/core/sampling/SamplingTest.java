package com.sri.ai.test.praise.core.representation.interfacebased.factor.core.sampling;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.set;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Random;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.random.JDKRandomGenerator;
import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.ImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SamplingProductFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.statistics.NormalWithFixedMeanAndStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.statistics.NormalWithFixedStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;
import com.sri.ai.util.number.representation.api.ArithmeticNumberFactory;
import com.sri.ai.util.number.representation.core.ArithmeticDoubleFactory;
import com.sri.ai.util.number.statistics.api.Statistic;
import com.sri.ai.util.number.statistics.core.CompoundStatistic;
import com.sri.ai.util.number.statistics.core.DefaultMean;
import com.sri.ai.util.number.statistics.core.MeanAndVariance;
import com.sri.ai.util.number.statistics.core.Variance;
import com.sri.ai.util.planning.core.OrPlan;

public class SamplingTest {

	private static Random random = new Random();
	
	private SamplingFactor normalOnX;
	private SamplingFactor normalOnY;
	private SamplingFactor normalOnZ;
	private Variable x;
	private Variable y;
	private Variable z;
	private ImportanceFactory importanceFactory = new DoubleImportanceFactory();
	private PotentialFactory potentialFactory = new DoublePotentialFactory();
	private ArithmeticDoubleFactory numberFactory = new ArithmeticDoubleFactory();
	private DefaultFactorNetwork network;
	private ExactBP solver;
	private SamplingFactor marginalOfX;
	private SamplingFactor marginalOfZ;
	private double meanOfZ;
	private double varianceOfZ;
	private double expectedVarianceOfMarginalOfX;

	public void setXYZModel() {
		x = new DefaultVariable("x");
		y = new DefaultVariable("y");
		z = new DefaultVariable("z");
		meanOfZ = 10;
		varianceOfZ = 4;
		expectedVarianceOfMarginalOfX = 12;
		normalOnX = new NormalWithFixedStandardDeviation(x, y, 2, random);
		normalOnY = new NormalWithFixedStandardDeviation(y, z, 2, random);
		normalOnZ = new NormalWithFixedMeanAndStandardDeviation(z, meanOfZ, Math.pow(varianceOfZ, 0.5), random);
		network = new DefaultFactorNetwork(list(normalOnY, normalOnX, normalOnZ));
	}
	
	@Test
	public void testNormalWithFixedStandardDeviation() {
		
		for (int j = 0; j != 1; j++) {

			long numberOfSamples = 5000;
			
			setXYZModel();

			solver = new ExactBP(x, network);
			marginalOfX = (SamplingFactor) solver.apply();
			runTest(numberOfSamples, x, marginalOfX, meanOfZ, 0.1, expectedVarianceOfMarginalOfX, 0.1, importanceFactory, potentialFactory, numberFactory);

			solver = new ExactBP(z, network);
			marginalOfZ = (SamplingFactor) solver.apply();
			runTest(numberOfSamples, z, marginalOfZ, meanOfZ, 0.1, varianceOfZ, 0.1, importanceFactory, potentialFactory, numberFactory);
			
		}
	}

	@Test
	public void testProductOfTwoNormalsWithSameStandardDeviation() {

		int numberOfTests = 1;
		long numberOfSamples = 50000;

		int mean1 = 5;
		double standardDeviation1 = 1;
		int mean2 = 10;
		int standardDeviation2 = 1;

		double meanTolerance = 0.1;
		double varianceTolerance = 0.2;

		testTwoNormals(
				numberOfTests, 
				numberOfSamples, 
				mean1, 
				standardDeviation1, 
				mean2, 
				standardDeviation2,
				meanTolerance, 
				varianceTolerance);

	}

	@Test
	public void testProductOfTwoNormalsWithQuiteDifferentStandardDeviations() {

		int numberOfTests = 1;
		long numberOfSamples = 5000;

		int mean1 = 5;
		double standardDeviation1 = 0.1;
		int mean2 = 10;
		int standardDeviation2 = 1;

		double meanTolerance = 0.1;
		double varianceTolerance = 0.2;

		testTwoNormals(
				numberOfTests, 
				numberOfSamples, 
				mean1, 
				standardDeviation1, 
				mean2, 
				standardDeviation2,
				meanTolerance, 
				varianceTolerance);

	}

	private void testTwoNormals(
			int numberOfTests, long numberOfSamples, 
			int mean1, double standardDeviation1,
			int mean2, double standardDeviation2, 
			double meanTolerance, double varianceTolerance) {
		
		for (int j = 0; j != numberOfTests; j++) {

			x = new DefaultVariable("x");
			NormalWithFixedMeanAndStandardDeviation normal1OnX = new NormalWithFixedMeanAndStandardDeviation(x, mean1, standardDeviation1, random);
			NormalWithFixedMeanAndStandardDeviation normal2OnX = new NormalWithFixedMeanAndStandardDeviation(x, mean2, standardDeviation2, random);
			network = new DefaultFactorNetwork(list(normal1OnX, normal2OnX));

			solver = new ExactBP(x, network);
			marginalOfX = (SamplingFactor) solver.apply();

			// Brute-force calculation:
			double value = 0.0;
			double sum_of_weighted_values = 0.0;
			double sum_of_weighted_variances = 0;
			double sum_of_weights = 0;
			while (value < 15.0) {
				double weight = 
						new NormalDistribution(new JDKRandomGenerator(random.nextInt()), mean1, standardDeviation1).density(value)
						*
						new NormalDistribution(new JDKRandomGenerator(random.nextInt()), mean2, standardDeviation2).density(value);
				sum_of_weighted_values += value * weight;
				sum_of_weights += weight;
				value += 0.01;
			}
			
			double mean = sum_of_weighted_values / sum_of_weights;
			
			value = 0.0;
			while (value < 15.0) {
				double weight = 
						new NormalDistribution(new JDKRandomGenerator(random.nextInt()), mean1, standardDeviation1).density(value)
						*
						new NormalDistribution(new JDKRandomGenerator(random.nextInt()), mean2, standardDeviation2).density(value);
				double weighted_variance = Math.pow(mean - value, 2.0) * weight;
				sum_of_weighted_variances += weighted_variance;
				value += 0.01;
			}

			double variance = sum_of_weighted_variances/sum_of_weights;
			println("Brute force mean: " + mean);
			println("Brute force variance: " + variance);

			double analyticMean = mean2 + Math.pow(standardDeviation2, 2)/(Math.pow(standardDeviation1, 2) + Math.pow(standardDeviation2, 2)) * (mean1 - mean2);
			double analyticVariance = Math.pow(standardDeviation2, 2) - Math.pow(standardDeviation2, 4)/(Math.pow(standardDeviation1, 2) + Math.pow(standardDeviation2, 2));
			println("Analytic mean    : " + analyticMean);
			println("Analytic variance: " + analyticVariance);
			
			println(
					"density of " + mean2 + " given mean " + mean1 + ": " 
							+ new NormalDistribution(new JDKRandomGenerator(random.nextInt()), mean1, standardDeviation1).density(mean2));
			
			println(
					"density of " + mean1 + " given mean " + mean2 + ": " 
							+ new NormalDistribution(new JDKRandomGenerator(random.nextInt()), mean2, standardDeviation2).density(mean1));
			
			println("Sampling plan:");
			println(((SamplingProductFactor) marginalOfX).getSamplingPlan(set()).nestedString());
			
			runTest(numberOfSamples, x, marginalOfX, analyticMean, meanTolerance, analyticVariance, varianceTolerance, importanceFactory, potentialFactory, numberFactory);
		}
	}

	private void runTest(
			long numberOfSamples, 
			Variable variable, 
			SamplingFactor marginal,
			double expectedMean, 
			double meanTolerance,
			double expectedVariance, 
			double varianceTolerance, 
			ImportanceFactory importanceFactory, PotentialFactory potentialFactory, ArithmeticDoubleFactory numberFactory) {
		
		println("-------------------------");
		println("Factor to sample from: " + marginal);
		println("Variable: " + variable);

		Sample sample;
		MeanAndVariance meanAndVariance = new MeanAndVariance(numberFactory);
		CompoundStatistic<ArithmeticNumber, ArithmeticNumber, ArithmeticNumber> varianceOfMean = 
				chain(potentialFactory, new DefaultMean(numberFactory), new Variance(numberFactory));

		println("Generating " + numberOfSamples + " samples");
		for (int i = 0; i != numberOfSamples; i++) {
			sample = new DefaultSample(importanceFactory, potentialFactory);
			marginal.sampleOrWeigh(sample);
//			if (i % 10000 == 0) {
//				println(percentageWithTwoDecimalPlaces(i, numberOfSamples) + "%");
//				println(sample);
//			}
			Double variableValue = (Double) sample.getAssignment().get(variable);
			meanAndVariance.add(numberFactory.make(variableValue), sample.getPotential());
			varianceOfMean.add(numberFactory.make(variableValue), sample.getPotential());
		}
		
		println("# samples: " + numberOfSamples);
		println("Total weight: " + meanAndVariance.getTotalWeight());
		println("Mean: " + meanAndVariance.getMean());
		println("Expected mean: " + expectedMean);
		println("Variance: " + meanAndVariance.getVariance());
		println("Expected variance: " + expectedVariance);
		println("Variance of mean: " + varianceOfMean.getValue());
		
		try {
			OrPlan samplingPlan = (OrPlan) ((SamplingProductFactor) marginalOfX).getSamplingPlan(set());
			println("Sampling plan probability distribution: " + samplingPlan.getDistribution());
			println("Number of sub-plan executions: " + samplingPlan.getNumberOfSubPlanExecutions());
		}
		catch (ClassCastException e) {
			// nothing to do; code simply does not apply
		}
		
		assertTrue(meanAndVariance.getMean().doubleValue() > expectedMean*(1 - meanTolerance) && meanAndVariance.getMean().doubleValue() < expectedMean*(1 + meanTolerance));
		assertTrue(meanAndVariance.getVariance().doubleValue() > expectedVariance*(1 - varianceTolerance) && meanAndVariance.getVariance().doubleValue() < expectedVariance*(1 + varianceTolerance));
	}

	private CompoundStatistic<ArithmeticNumber, ArithmeticNumber, ArithmeticNumber> chain(ArithmeticNumberFactory numberFactory, Statistic... statistics) {
		return CompoundStatistic.<ArithmeticNumber, ArithmeticNumber, ArithmeticNumber>chain(statistics);
	}
}
