package com.sri.ai.test.praise.core.inference.representation.sampling;

import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Random;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.random.JDKRandomGenerator;
import org.junit.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.ImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedMeanAndStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SamplingMarginalizingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SamplingProductFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;
import com.sri.ai.util.number.representation.api.ArithmeticNumberFactory;
import com.sri.ai.util.number.representation.core.ArithmeticDoubleFactory;
import com.sri.ai.util.number.statistics.api.Statistic;
import com.sri.ai.util.number.statistics.core.DefaultMean;
import com.sri.ai.util.number.statistics.core.MeanAndVariance;
import com.sri.ai.util.number.statistics.core.StatisticsChain;
import com.sri.ai.util.number.statistics.core.Variance;
import com.sri.ai.util.planning.core.OrPlan;

public class SamplingTest {

	private static Random random = new Random();
	
	SamplingFactor normalOnX;
	SamplingFactor normalOnY;
	SamplingFactor normalOnZ;
	Variable x;
	Variable y;
	Variable z;
	ImportanceFactory importanceFactory = new DoubleImportanceFactory();
	PotentialFactory potentialFactory = new DoublePotentialFactory();
	ArithmeticDoubleFactory numberFactory = new ArithmeticDoubleFactory();
	DefaultFactorNetwork network;
	ExactBP solver;
	SamplingFactor marginalOfX;
	SamplingFactor marginalOfZ;
	SamplingFactor factor;
	double meanOfZ;
	double varianceOfZ;
	double expectedVarianceOfMarginalOfX;
	String expectedString;

	public void setXYZModel() {
		x = new DefaultVariable("x");
		y = new DefaultVariable("y");
		z = new DefaultVariable("z");
		meanOfZ = 10;
		varianceOfZ = 4;
		expectedVarianceOfMarginalOfX = 12;
		normalOnX = new NormalWithFixedStandardDeviation(x, y, 2, new DoublePotentialFactory(), random);
		normalOnY = new NormalWithFixedStandardDeviation(y, z, 2, new DoublePotentialFactory(), random);
		normalOnZ = new NormalWithFixedMeanAndStandardDeviation(z, meanOfZ, Math.pow(varianceOfZ, 0.5), new DoublePotentialFactory(), random);
		network = new DefaultFactorNetwork(list(normalOnY, normalOnX, normalOnZ));
	}

	// @Test : order variation issues make this unreliable for automated testing, but it is good for manual testing.
	void testXYZModelSamplingRules() {

		setXYZModel();

		solver = new ExactBP(z, network);
		marginalOfZ = (SamplingFactor) solver.apply();
		println(marginalOfZ.nestedString(true));

		factor = marginalOfZ;
		expectedString = "z <= ";
		println(samplingRulesString(factor));
		assertEquals(expectedString, samplingRulesString(marginalOfZ));

		// message from y to z
		SamplingMarginalizingFactor messageFromYToZ = (SamplingMarginalizingFactor) ((SamplingProductFactor) marginalOfZ).getInputFactors().get(0);
		factor = messageFromYToZ;
		expectedString = "";
		runSamplingRulesTest();

		SamplingFactor priorOfZ = ((SamplingProductFactor) marginalOfZ).getInputFactors().get(1);
		factor = priorOfZ;
		expectedString = "z <= ";
		runSamplingRulesTest();

		SamplingProductFactor incomingToY = (SamplingProductFactor) messageFromYToZ.getMarginalizedFactor();
		factor = incomingToY;
		expectedString = "z <= y; y <= z";
		runSamplingRulesTest();

		SamplingFactor YIsNormalZ = incomingToY.getInputFactors().get(0);
		factor = YIsNormalZ;
		expectedString = "z <= y; y <= z";
		runSamplingRulesTest();

		SamplingFactor messageFromXToY = incomingToY.getInputFactors().get(1);
		factor = messageFromXToY;
		expectedString = "";
		runSamplingRulesTest();

	}

	private void runSamplingRulesTest() {
		println(samplingRulesString(factor));
		assertEquals(expectedString, samplingRulesString(factor));
	}

	private String samplingRulesString(SamplingFactor samplingFactor) {
		SamplingRuleSet samplingRules = samplingFactor.getSamplingRuleSet();
		List<String> ruleStrings = mapIntoList(samplingRules.getSamplingRules(), this::ruleString);
		String samplingRulesString = join("; ", ruleStrings);
		return samplingRulesString;
	}
	
	private String ruleString(SamplingRule rule) {
		return join(rule.getConsequents()) + " <= " + join(rule.getAntecendents());
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
		long numberOfSamples = 5000000;

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

	private void testTwoNormals(int numberOfTests, long numberOfSamples, int mean1, double standardDeviation1,
			int mean2, int standardDeviation2, double meanTolerance, double varianceTolerance) {
		for (int j = 0; j != numberOfTests; j++) {

			x = new DefaultVariable("x");
			NormalWithFixedMeanAndStandardDeviation normal1OnX = new NormalWithFixedMeanAndStandardDeviation(x, mean1, standardDeviation1, new DoublePotentialFactory(), random);
			NormalWithFixedMeanAndStandardDeviation normal2OnX = new NormalWithFixedMeanAndStandardDeviation(x, mean2, standardDeviation2, new DoublePotentialFactory(), random);
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
			println(((SamplingProductFactor) marginalOfX).getSamplingPlan().nestedString());
			
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
		MeanAndVariance meanAndVariance;
		Statistic<ArithmeticNumber> varianceOfMean;
		meanAndVariance = new MeanAndVariance(numberFactory);
		
		varianceOfMean = chain(potentialFactory, new DefaultMean(numberFactory), new Variance(numberFactory));

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
			OrPlan samplingPlan = (OrPlan) ((SamplingProductFactor) marginalOfX).getSamplingPlan();
			println("Sampling plan probability distribution: " + samplingPlan.getDistribution());
			println("Number of sub-plan executions: " + samplingPlan.getNumberOfSubPlanExecutions());
		}
		catch (ClassCastException e) {
			// nothing to do; code simply does not apply
		}
		
		assertTrue(meanAndVariance.getMean().doubleValue() > expectedMean*(1 - meanTolerance) && meanAndVariance.getMean().doubleValue() < expectedMean*(1 + meanTolerance));
		assertTrue(meanAndVariance.getVariance().doubleValue() > expectedVariance*(1 - varianceTolerance) && meanAndVariance.getVariance().doubleValue() < expectedVariance*(1 + varianceTolerance));
	}

	private Statistic<ArithmeticNumber> chain(ArithmeticNumberFactory numberFactory, Statistic... statistics) {
		return 
				StatisticsChain.<ArithmeticNumber>chain(numberFactory, statistics);
	}
}
