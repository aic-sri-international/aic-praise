package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRule;
import static com.sri.ai.util.Util.getOrUseDefault;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapFromLists;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.Map;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.util.Util;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;

/**
 * A sampling factor binding a variable to a list of possible values, each with its probability.
 *  
 * @author braz
 *
 */
public class TableSamplingFactor extends AbstractSamplingFactor {

	private Variable variable;
	private ArrayList<Object> values;
	private ArrayList<Double> probabilities;
	private Map<Object, Double> fromValueToProbability;
	private PotentialFactory potentialFactory;
	
	public TableSamplingFactor(Variable variable, ArrayList<Object> values, ArrayList<Double> probabilities, PotentialFactory potentialFactory, Random random) {
		super(list(variable), random);
		myAssert( ! values.isEmpty(), () -> getClass() + " requires at least one value, but got none.");
		myAssert( values.size() == probabilities.size(), () -> getClass() + " requires one probability for each value, but got " + values.size() + " values and " + probabilities.size() + " probabilities.");
		this.variable = variable;
		this.values = values;
		this.probabilities = probabilities;
		this.fromValueToProbability = mapFromLists(values, probabilities);
		this.potentialFactory = potentialFactory;
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		if (isNotDefined(variable, sample)) {
			sample(sample);
		}
		else {
			weigh(sample);
		}
	}

	private void sample(Sample sample) throws Error {
		int indexOfValue = Util.sample(probabilities, getRandom());
		Object value = values.get(indexOfValue);
		sample.getAssignment().set(variable, value);
	}

	private void weigh(Sample sample) {
		double probabilityOfVariableValue = probabilityOfVariableValue(sample);
		sample.updatePotential(potentialFactory.make(probabilityOfVariableValue));
	}
	
	private double probabilityOfVariableValue(Sample sample) {
		Object value = getValue(variable, sample);
		double probability = getOrUseDefault(fromValueToProbability, value, 0.0);
		return probability;
	}
	
	private boolean isNotDefined(Variable variable, Sample sample) {
		boolean result = getValue(variable, sample) == null;
		return result;
	}

	private ArithmeticNumber getValue(Variable variable, Sample sample) {
		ArithmeticNumber result = (ArithmeticNumber) sample.getAssignment().get(variable);
		return result;
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		SamplingRule samplingRule = samplingRule(this, list(variable), list(), 1.0/values.size());
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(getVariables(), samplingRule);
		return result;
	}

}
