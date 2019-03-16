package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sandbox;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable.expressionVariable;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRuleFromVariables;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntegersIntoList;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.random.JDKRandomGenerator;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

public class RandomWalkWeighedGibbs extends AbstractSamplingFactor implements SamplingFactor {

	private int length;
	private double standardDeviation;
	private Sample currentSample;
	@SuppressWarnings("unused")
	private Potential productOfPs;
	private Map<Variable, Double> fromVariableToQ;
	
	public RandomWalkWeighedGibbs(int length, double standardDeviation, Random random) {
		super(makeVariables(length), random);
		this.length = length;
		this.standardDeviation = standardDeviation;
		this.currentSample = new DefaultSample(new DoubleImportanceFactory(), new DoublePotentialFactory());
		new RandomWalkMonteCarlo(length, standardDeviation, random).sampleOrWeigh(currentSample);
		this.productOfPs = currentSample.getPotential().one();
		this.fromVariableToQ = Util.mapIntegersIntoMap(1, length, i -> Pair.make(getVariable(i), getNormalDistribution(getValue(currentSample, i - 1)).density(getValue(currentSample, i))));
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
		println("Original sample: " + currentSample);
		int flippedVariablesIndex = 1 + getRandom().nextInt(getVariables().size() - 1);
		println("Flipping: " + flippedVariablesIndex);
		double oldValue = getValue(currentSample, flippedVariablesIndex);
		NormalDistribution normalFromBack = getNormalDistribution(getValue(currentSample, flippedVariablesIndex - 1));
		double previousP = normalFromBack.density(oldValue);
		double newValue = normalFromBack.sample();
		currentSample.set(getVariable(flippedVariablesIndex), newValue);
		double previousQ = fromVariableToQ.get(getVariable(flippedVariablesIndex));
		fromVariableToQ.put(getVariable(flippedVariablesIndex), normalFromBack.density(newValue));
		if (flippedVariablesIndex < length - 1) {
			NormalDistribution normal = getNormalDistribution(getValue(currentSample, flippedVariablesIndex + 1));
			double oldForwardPotential = normal.density(oldValue);
			double newForwardPotential = normal.density(newValue);
			// println("Removing old forward potential " + oldForwardPotential);
			currentSample.removePotential(new DoublePotential(oldForwardPotential));
			// println("Multiplying new forward potential " + newForwardPotential);
			currentSample.updatePotential(new DoublePotential(newForwardPotential));
		}

		// println("Removing old backward potential " + previousP);
		currentSample.removePotential(new DoublePotential(previousP));
		// println("Multiplying (removing) old q " + previousQ);
		currentSample.updatePotential(new DoublePotential(previousQ));

		sample.copyToSameInstance(currentSample);
		println("Final sample: " + currentSample);
		println();
		
		double productOfPs = 1.0;
		for (int i = 1; i != length; i++) {
			productOfPs *= getNormalDistribution(getValue(currentSample, i - 1)).density(getValue(currentSample, i));
		}
		double productOfQs = Util.fold(fromVariableToQ.values(), (p1, p2) -> p1*p2, 1.0);
		double weight = productOfPs/productOfQs;
		//println("Sample: " + currentSample);
		// println("Sample weight: " + currentSample.getPotential());
		// println("P's: " + mapIntegersIntoMap(1, length, i -> Pair.make(getVariable(i), getNormalDistribution(getValue(currentSample, i - 1)).density(getValue(currentSample, i)))));
		// println("Product of p's: " + productOfPs);
		// println("Q's: " + fromVariableToQ);
		// println("Product of q's: " + productOfQs);
		// println("p/q weight: " + weight);
		// println("");
		if (Math.abs(weight - currentSample.getPotential().doubleValue()) > 0.0000001) {
			System.exit(0);
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