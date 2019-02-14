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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;

public class RandomWalkMonteCarlo extends AbstractSamplingFactor implements SamplingFactor {

	private double standardDeviation;
	
	public RandomWalkMonteCarlo(int length, double standardDeviation, Random random) {
		super(makeVariables(length), random);
		this.standardDeviation = standardDeviation;
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
		sample.set(getVariables().get(0), 0.0);
		for (int i = 1; i != getVariables().size(); i++) {
			double value = getNormalDistribution(getValue(sample, i - 1)).sample();
			sample.set(getVariable(i), value);
		}
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