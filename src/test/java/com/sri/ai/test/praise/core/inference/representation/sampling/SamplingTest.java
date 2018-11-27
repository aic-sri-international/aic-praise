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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;
import com.sri.ai.util.number.representation.core.ArithmeticDoubleFactory;
import com.sri.ai.util.number.statistics.api.Statistic;
import com.sri.ai.util.number.statistics.core.DefaultMean;
import com.sri.ai.util.number.statistics.core.MeanAndVariance;
import com.sri.ai.util.number.statistics.core.StatisticsChain;
import com.sri.ai.util.number.statistics.core.Variance;

public class SamplingTest {

	private static Random random = new Random();
	
	int numberOfSamples = 5000;

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
	double meanOfZ;
	double varianceOfZ;
	double expectedVarianceOfMarginalOfX;

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

	@Test
	void testSamplingRules() {
		
			setXYZModel();

			solver = new ExactBP(z, network);
			marginalOfZ = (SamplingFactor) solver.apply();
			println(marginalOfZ.nestedString(true));
	}

	//@Test
	void testNormalWithFixedStandardDeviation() {
		
		for (int j = 0; j != 1000; j++) {

			setXYZModel();

			solver = new ExactBP(x, network);
			marginalOfX = (SamplingFactor) solver.apply();
			runTest(numberOfSamples, x, marginalOfX, meanOfZ, expectedVarianceOfMarginalOfX, importanceFactory, potentialFactory, numberFactory);

			solver = new ExactBP(z, network);
			marginalOfZ = (SamplingFactor) solver.apply();
			runTest(numberOfSamples, z, marginalOfZ, meanOfZ, varianceOfZ, importanceFactory, potentialFactory, numberFactory);
			
		}
	}

	private void runTest(
			int numberOfSamples, 
			Variable variable, 
			SamplingFactor marginal,
			double expectedMean, 
			double expectedVariance,
			ImportanceFactory importanceFactory, 
			PotentialFactory potentialFactory, 
			ArithmeticDoubleFactory numberFactory) {
		
		println("-------------------------");
		println("Factor to sample from: " + marginal);
		println("Variable: " + variable);

		Sample sample;
		MeanAndVariance meanAndVariance;
		Statistic<ArithmeticNumber> varianceOfMean;
		meanAndVariance = new MeanAndVariance(numberFactory);
		
		varianceOfMean = chain(new DefaultMean(numberFactory), new Variance(numberFactory));

		for (int i = 0; i != numberOfSamples; i++) {
			sample = new DefaultSample(importanceFactory, potentialFactory);
			marginal.sampleOrWeigh(sample);
			Double variableValue = (Double) sample.getAssignment().get(variable);
			meanAndVariance.add(variableValue);
			varianceOfMean.add(numberFactory.make(variableValue));
		}
		
		println("# samples: " + numberOfSamples + ", Mean: " + meanAndVariance.getMean() + ", Variance: " + meanAndVariance.getVariance() + ", Variance of mean: " + varianceOfMean.getValue());
		println("Expected mean: " + expectedMean);
		println("Expected variance: " + expectedVariance);
		assertTrue(meanAndVariance.getMean().doubleValue() > expectedMean*0.9 && meanAndVariance.getMean().doubleValue() < expectedMean*1.1);
		assertTrue(meanAndVariance.getVariance().doubleValue() > expectedVariance*0.9 && meanAndVariance.getVariance().doubleValue() < expectedVariance*1.1);
	}

	private Statistic<ArithmeticNumber> chain(Statistic... statistics) {
		return 
				(Statistic<ArithmeticNumber>) 
				StatisticsChain.<ArithmeticNumber>chain(statistics);
	}
}
