package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRule;
import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;

/**
 * A sampling factor binding a variable to a value.
 *  
 * @author braz
 *
 */
public class ConstantSamplingFactor extends AbstractSamplingFactor {

	private Variable variable;
	private Object value;
	
	public ConstantSamplingFactor(Variable variable, Object value) {
		super(list(variable), null /* null */);
		this.variable = variable;
		this.value = value;
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
		sample.updatePotential(probabilityOfVariableValue(sample));
	}
	
	private Potential probabilityOfVariableValue(Sample sample) {
		Object value = getValue(variable, sample);
		Potential probability = value.equals(this.value)? sample.getPotential().one() : sample.getPotential().zero();
		return probability;
	}
	
	private boolean isNotDefined(Variable variable, Sample sample) {
		boolean result = getValue(variable, sample) == null;
		return result;
	}

	private Object getValue(Variable variable, Sample sample) {
		Object result = sample.getAssignment().get(variable);
		return result;
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		SamplingRule samplingRule = samplingRule(this, list(variable), list(), Double.MAX_VALUE);
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(getVariables(), samplingRule);
		return result;
	}

}
