package com.sri.ai.test.praise.core.representation.interfacebased.factor.core.sampling;

import static com.sri.ai.expresso.helper.Expressions.areEqualUpToNumericProportion;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleBasedSample.doubleBasedSample;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.listFrom;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.function.api.variables.Assignment.assignments;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ConditionedSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.logic.IfThenElseSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.statistics.NormalWithFixedMeanAndStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.statistics.NormalWithFixedStandardDeviation;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.DefaultSamplingFactorDiscretizedProbabilityDistributionFunction;
import com.sri.ai.util.function.api.functions.Function;
import com.sri.ai.util.function.api.values.Value;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Unit;
import com.sri.ai.util.function.core.variables.DefaultSetOfVariables;
import com.sri.ai.util.function.core.variables.EnumVariable;
import com.sri.ai.util.function.core.variables.RealVariable;

class IfThenElseSamplingFactorTest {

	Variable rainy = new DefaultVariable("rainy");
	Variable x = new DefaultVariable("x");
	Variable y = new DefaultVariable("y");
	Random random = new Random();

	@Test
	void samplingRulesTest() {
		IfThenElseSamplingFactor ifThenElseSamplingFactor;
		String expectedSamplingRulesString;
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);
		expectedSamplingRulesString = 
				"x <= rainy, rainy = true        with sampling factor: if (rainy) then x = Normal(0.0, 1.0) else x = Normal(5.0, 1.0) and weight 1.0\n" + 
				"x <= rainy, rainy = false        with sampling factor: if (rainy) then x = Normal(0.0, 1.0) else x = Normal(5.0, 1.0) and weight 1.0\n" + 
				"rainy <= x        with sampling factor: if (rainy) then x = Normal(0.0, 1.0) else x = Normal(5.0, 1.0) and weight 1.0";
		runSamplingRulesTest(ifThenElseSamplingFactor, expectedSamplingRulesString);

		
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(y, 5.0, 1.0, random),
						random);
		expectedSamplingRulesString = 
				"x <= rainy, rainy = true        with sampling factor: if (rainy) then x = Normal(0.0, 1.0) else y = Normal(5.0, 1.0) and weight 1.0\n" + 
				"y <= rainy, rainy = false        with sampling factor: if (rainy) then x = Normal(0.0, 1.0) else y = Normal(5.0, 1.0) and weight 1.0\n" + 
				"rainy <= x, y        with sampling factor: if (rainy) then x = Normal(0.0, 1.0) else y = Normal(5.0, 1.0) and weight 1.0";
		runSamplingRulesTest(ifThenElseSamplingFactor, expectedSamplingRulesString);

		
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedStandardDeviation(x, y, 1.0, random),
						new NormalWithFixedStandardDeviation(y, x, 1.0, random),
						random);
		expectedSamplingRulesString = 
				"y <= rainy, rainy = true, x        with sampling factor: if (rainy) then x = Normal(y, 1.0) else y = Normal(x, 1.0) and weight 1.0\n" + 
				"x <= rainy, rainy = true, y        with sampling factor: if (rainy) then x = Normal(y, 1.0) else y = Normal(x, 1.0) and weight 1.0\n" + 
				"x <= rainy, rainy = false, y        with sampling factor: if (rainy) then x = Normal(y, 1.0) else y = Normal(x, 1.0) and weight 1.0\n" + 
				"y <= rainy, rainy = false, x        with sampling factor: if (rainy) then x = Normal(y, 1.0) else y = Normal(x, 1.0) and weight 1.0\n" + 
				"rainy <= x, y        with sampling factor: if (rainy) then x = Normal(y, 1.0) else y = Normal(x, 1.0) and weight 1.0";
		runSamplingRulesTest(ifThenElseSamplingFactor, expectedSamplingRulesString);
	}

	private void runSamplingRulesTest(IfThenElseSamplingFactor ifThenElseSamplingFactor, String expectedSamplingRulesString) {
		String actualSamplingRulesString = join("\n", ifThenElseSamplingFactor.getSamplingRuleSet().getSamplingRules());
		println("Expected:\n" + expectedSamplingRulesString);
		println("Actual  :\n" + actualSamplingRulesString);
		assertEquals(expectedSamplingRulesString, actualSamplingRulesString);
	}
	
	@Test
	void samplingTest() {
		IfThenElseSamplingFactor ifThenElseSamplingFactor;
		SamplingFactor testedSamplingFactor;
		SetOfVariables variablesWithRange;
		String expectedValuesTupleString;
		Sample conditioningSample;
		int queryIndex;
		
		int numberOfSamples = 1000;
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);

		conditioningSample = doubleBasedSample(rainy, true);
		testedSamplingFactor = ConditionedSamplingFactor.build(conditioningSample, ifThenElseSamplingFactor);
		variablesWithRange = new DefaultSetOfVariables(new RealVariable("x", Unit.NONE, "-5", "1", "5"));
		queryIndex = 0;
		expectedValuesTupleString = "(0.0, 0.0, 0.003, 0.067, 0.258, 0.363, 0.242, 0.066, 0.001, 0.0, 0.0)";
		runSamplingTest(testedSamplingFactor, variablesWithRange, queryIndex, expectedValuesTupleString, numberOfSamples);
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);

		////////////////////////
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);

		conditioningSample = doubleBasedSample(rainy, false);
		testedSamplingFactor = ConditionedSamplingFactor.build(conditioningSample, ifThenElseSamplingFactor);
		variablesWithRange = new DefaultSetOfVariables(new RealVariable("x", Unit.NONE, "0", "1", "10"));
		queryIndex = 0;
		expectedValuesTupleString = "(0.0, 0.0, 0.003, 0.067, 0.258, 0.363, 0.242, 0.066, 0.001, 0.0, 0.0)";
		runSamplingTest(testedSamplingFactor, variablesWithRange, queryIndex, expectedValuesTupleString, numberOfSamples);

		////////////////////////
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);

		conditioningSample = doubleBasedSample(x, 0.0);
		testedSamplingFactor = ConditionedSamplingFactor.build(conditioningSample, ifThenElseSamplingFactor);
		variablesWithRange = new DefaultSetOfVariables(new EnumVariable("rainy", "false", "true"));
		queryIndex = 0;
		expectedValuesTupleString = "(0.0, 1.0)";
		runSamplingTest(testedSamplingFactor, variablesWithRange, queryIndex, expectedValuesTupleString, numberOfSamples);

		////////////////////////
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);

		conditioningSample = doubleBasedSample(x, 0.0);
		testedSamplingFactor = ConditionedSamplingFactor.build(conditioningSample, ifThenElseSamplingFactor);
		variablesWithRange = new DefaultSetOfVariables(new EnumVariable("rainy", "false", "true"));
		queryIndex = 0;
		expectedValuesTupleString = "(0.0, 1.0)";
		runSamplingTest(testedSamplingFactor, variablesWithRange, queryIndex, expectedValuesTupleString, numberOfSamples);

		////////////////////////
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);

		conditioningSample = doubleBasedSample(x, 5.0);
		testedSamplingFactor = ConditionedSamplingFactor.build(conditioningSample, ifThenElseSamplingFactor);
		variablesWithRange = new DefaultSetOfVariables(new EnumVariable("rainy", "false", "true"));
		queryIndex = 0;
		expectedValuesTupleString = "(1.0, 0.0)";
		runSamplingTest(testedSamplingFactor, variablesWithRange, queryIndex, expectedValuesTupleString, numberOfSamples);

		////////////////////////
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);

		conditioningSample = doubleBasedSample(x, 2.5);
		testedSamplingFactor = ConditionedSamplingFactor.build(conditioningSample, ifThenElseSamplingFactor);
		variablesWithRange = new DefaultSetOfVariables(new EnumVariable("rainy", "false", "true"));
		queryIndex = 0;
		expectedValuesTupleString = "(0.5, 0.5)";
		runSamplingTest(testedSamplingFactor, variablesWithRange, queryIndex, expectedValuesTupleString, numberOfSamples);

		////////////////////////
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 4.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 4.0, random),
						random);

		conditioningSample = doubleBasedSample(x, 1.0);
		testedSamplingFactor = ConditionedSamplingFactor.build(conditioningSample, ifThenElseSamplingFactor);
		variablesWithRange = new DefaultSetOfVariables(new EnumVariable("rainy", "false", "true"));
		queryIndex = 0;
		expectedValuesTupleString = "(0.366, 0.634)";
		runSamplingTest(testedSamplingFactor, variablesWithRange, queryIndex, expectedValuesTupleString, numberOfSamples);

		////////////////////////
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedStandardDeviation(x, y, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);

		conditioningSample = doubleBasedSample(rainy, true, y, 0.0);
		testedSamplingFactor = ConditionedSamplingFactor.build(conditioningSample, ifThenElseSamplingFactor);
		variablesWithRange = new DefaultSetOfVariables(new RealVariable("x", Unit.NONE, "-5", "1", "5"));
		queryIndex = 0;
		expectedValuesTupleString = "(0.0, 0.0, 0.003, 0.067, 0.258, 0.363, 0.242, 0.066, 0.001, 0.0, 0.0)";
		runSamplingTest(testedSamplingFactor, variablesWithRange, queryIndex, expectedValuesTupleString, numberOfSamples);

		////////////////////////
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedStandardDeviation(x, y, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);

		conditioningSample = doubleBasedSample(x, 2.5, y, 0.0);
		testedSamplingFactor = ConditionedSamplingFactor.build(conditioningSample, ifThenElseSamplingFactor);
		variablesWithRange = new DefaultSetOfVariables(new EnumVariable("rainy", "false", "true"));
		queryIndex = 0;
		expectedValuesTupleString = "(0.5, 0.5)";
		runSamplingTest(testedSamplingFactor, variablesWithRange, queryIndex, expectedValuesTupleString, numberOfSamples);

	}

	private void runSamplingTest(
			SamplingFactor testedSamplingFactor,
			SetOfVariables variablesWithRange,
			int queryIndex,
			String expectedValuesTupleString,
			int numberOfSamples) {
		
		Function distribution = new DefaultSamplingFactorDiscretizedProbabilityDistributionFunction(
				testedSamplingFactor,
				variablesWithRange,
				queryIndex,
				numberOfSamples);
		List<Value> distributionValues = listFrom(functionIterator(assignments(variablesWithRange), distribution::evaluate));
		Expression actualValues = parse("(" + join(distributionValues) + ")");
		Expression expectedValues = parse(expectedValuesTupleString);
		String comparison = areEqualUpToNumericProportion(expectedValues, actualValues, 0.8, 0.1);
		if (!comparison.isEmpty()) {
			println("Expected: " + expectedValuesTupleString);
			println("Actual  : " + actualValues);
			println(comparison);
			fail(comparison);
		}
	}

	@Test
	void weighingTest() {
		IfThenElseSamplingFactor ifThenElseSamplingFactor;
		double expectedWeight;
		Sample sample;
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);
	
		sample = doubleBasedSample(rainy, true, x, 2.0);
		expectedWeight = 0.053990966513188056;
		runWeighingTest(ifThenElseSamplingFactor, sample, expectedWeight);

		//////////////////
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);
	
		sample = doubleBasedSample(rainy, false, x, 2.0);
		expectedWeight = 0.004431848411938009;
		runWeighingTest(ifThenElseSamplingFactor, sample, expectedWeight);
	}

	private void runWeighingTest(IfThenElseSamplingFactor ifThenElseSamplingFactor, Sample sample, double expectedWeight) {
		ifThenElseSamplingFactor.sampleOrWeigh(sample);
		println("Expected weight: " + expectedWeight);
		println("Actual   weight: " + sample.getPotential());
		assertEquals(expectedWeight, sample.getPotential().doubleValue(), 0.0);
	}

}
