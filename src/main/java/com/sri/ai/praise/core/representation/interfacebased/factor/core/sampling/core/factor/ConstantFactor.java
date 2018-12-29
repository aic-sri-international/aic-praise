package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRule;
import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;

/**
 * A sampling factor binding a variable to a value.
 *  
 * @author braz
 *
 */
public class ConstantFactor extends AbstractSamplingFactor {

	private Variable variable;
	private Object value;
	private PotentialFactory potentialFactory;
	
	public ConstantFactor(Variable variable, Object value, PotentialFactory potentialFactory) {
		super(list(variable), null /* null */);
		this.variable = variable;
		this.value = value;
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
		sample.getAssignment().set(variable, value);
	}

	private void weigh(Sample sample) {
		double probabilityOfVariableValue = probabilityOfVariableValue(sample);
		sample.updatePotential(potentialFactory.make(probabilityOfVariableValue));
	}
	
	private double probabilityOfVariableValue(Sample sample) {
		Object value = getValue(variable, sample);
		double probability = value.equals(this.value)? 1.0 : 0.0;
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
		SamplingRule samplingRule = samplingRule(this, list(variable), list(), Double.MAX_VALUE);
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(getVariables(), samplingRule);
		return result;
	}

}
