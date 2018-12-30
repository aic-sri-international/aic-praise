package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRule;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;

public class EqualitySamplingFactor extends AbstractSamplingFactor {

	private Variable variable1;
	
	private Variable variable2;
	
	public EqualitySamplingFactor(Variable variable1, Variable variable2) {
		super(arrayList(variable1, variable2), null /* random not necessary */);
		this.variable1 = variable1;
		this.variable2 = variable2;
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		Object variable1Value = sample.getAssignment().get(variable1);
		Object variable2Value = sample.getAssignment().get(variable2);
		if (variable1Value != null) {
			variable1IsDefined(sample, variable1Value, variable2Value);
		}
		else if (variable2Value != null) {
			variable1IsUndefinedAndVariable2IsDefined(sample, variable2Value);
		}
		else {
			bothVarianlesAreUndefined();
		}
	}

	private void variable1IsDefined(Sample sample, Object variable1Value, Object variable2Value) {
		if (variable2Value != null) {
			bothVariablesAreDefined(sample, variable1Value, variable2Value);
		}
		else {
			variable2IsUndefinedAndVariable1IsDefined(sample, variable1Value);
		}
	}

	private void bothVariablesAreDefined(Sample sample, Object variable1Value, Object variable2Value) {
		if ( ! variable1Value.equals(variable2Value)) {
			sample.updatePotential(sample.getPotential().zero());
		}
	}

	private void variable2IsUndefinedAndVariable1IsDefined(Sample sample, Object variable1Value) {
		sample.getAssignment().set(variable2, variable1Value);
	}

	private void variable1IsUndefinedAndVariable2IsDefined(Sample sample, Object variable2Value) {
		sample.getAssignment().set(variable1, variable2Value);
	}

	private void bothVarianlesAreUndefined() {
		// do nothing
	}

	@Override
	public SamplingRuleSet makeSamplingRules() {
		SamplingRule fromVariable1ToVariable2 = samplingRule(this, list(variable2), list(variable1), Double.MAX_VALUE);
		SamplingRule fromVariable2ToVariable1 = samplingRule(this, list(variable1), list(variable2), Double.MAX_VALUE);
		return new DefaultSamplingRuleSet(getVariables(), fromVariable1ToVariable2, fromVariable2ToVariable1);
	}

	@Override
	public String toString() {
		return variable1 + " = " + variable2;
	}
}