package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sandbox;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable.expressionVariable;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRuleFromVariables;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntegersIntoList;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.random.JDKRandomGenerator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.util.distribution.Distributions;

public class RandomWalkGibbs extends AbstractSamplingFactor implements SamplingFactor {

	private int length;
	private double standardDeviation;
	private Sample currentSample;
	
	public RandomWalkGibbs(int length, double standardDeviation, Random random) {
		super(makeVariables(length), random);
		this.length = length;
		this.standardDeviation = standardDeviation;
		this.currentSample = new DefaultSample(new DoubleImportanceFactory(), new DoublePotentialFactory());
		new RandomWalkMonteCarlo(length, standardDeviation, random).sampleOrWeigh(currentSample);
	}

	private static List<? extends Variable> makeVariables(int length) {
		return mapIntegersIntoList(length, i -> expressionVariable("x" + i));
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		ArrayList<SamplingRule> arrayList = 
				mapIntoArrayList(
						getVariables(), 
						v -> samplingRuleFromVariables(this, list(v), list(), 1.0));
		return new DefaultSamplingRuleSet(arrayList);
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		sampleOrWeigh();
		sample.copyToSameInstance(currentSample);
	}

	private void sampleOrWeigh() {
		//println("Current sample: " + currentSample);
		int flippedVariablesIndex = 1 + getRandom().nextInt(getVariables().size() - 1);
		//println("Chose variable " + flippedVariablesIndex);
		NormalDistribution flippedVariableConditionalDistribution = determineConditionalDistribution(flippedVariablesIndex);
		double newValue = flippedVariableConditionalDistribution.sample();
		currentSample.set(getVariable(flippedVariablesIndex), newValue);
		//println("Conditional distribution: " + Distributions.toString(flippedVariableConditionalDistribution));
		//println("New value: " + newValue);
		//println("New sample: " + currentSample);
	}

	private NormalDistribution determineConditionalDistribution(int flippedVariablesIndex) {
		double valueOfPrevious = getValue(currentSample, flippedVariablesIndex - 1);
		NormalDistribution normalFromBack = getNormalDistribution(valueOfPrevious);
		//println("Value of previous variable: " + valueOfPrevious);
		NormalDistribution flippedVariableConditionalDistribution;
		if (flippedVariablesIndex < length - 1) {
			double valueOfNext = getValue(currentSample, flippedVariablesIndex + 1);
			//println("Value of next variable: " + valueOfNext);
			NormalDistribution normalFromFront = getNormalDistribution(valueOfNext);
			flippedVariableConditionalDistribution = Distributions.product(normalFromBack, normalFromFront, getRandom());
		}
		else {
			flippedVariableConditionalDistribution = normalFromBack;
		}
		return flippedVariableConditionalDistribution;
	}

	private Variable getVariable(int i) {
		return getVariables().get(i);
	}

	private double getValue(Sample sample, int index) {
		return (double) sample.get(getVariables().get(index));
	}

	protected NormalDistribution getNormalDistribution(double meanValue) {
		return new NormalDistribution(new JDKRandomGenerator(getRandom().nextInt()), meanValue, standardDeviation);
	}

}