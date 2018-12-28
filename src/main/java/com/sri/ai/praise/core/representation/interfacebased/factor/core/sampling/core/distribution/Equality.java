package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRule;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;

public class Equality extends AbstractSamplingFactor {

	private Variable variable1;
	
	private Variable variable2;
	
	private PotentialFactory potentialFactory;
	
	public Equality(Variable variable1, Variable variable2, PotentialFactory potentialFactory) {
		super(arrayList(variable1, variable2), null /* random not necessary */);
		this.variable1 = variable1;
		this.variable2 = variable2;
		this.potentialFactory = potentialFactory;
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		Object variable1Value = sample.getAssignment().get(variable1);
		Object variable2Value = sample.getAssignment().get(variable2);
		if (variable1Value != null) {
			if (variable2Value != null) {
				if ( ! variable1Value.equals(variable2Value)) {
					sample.updatePotential(potentialFactory.make(0.0));
				}
			}
			else {
				sample.getAssignment().set(variable2, variable1Value);
			}
		}
		else {
			sample.getAssignment().set(variable1, variable2Value);
		}
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