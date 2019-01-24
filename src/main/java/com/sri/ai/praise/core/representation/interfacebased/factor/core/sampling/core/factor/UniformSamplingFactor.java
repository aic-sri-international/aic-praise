package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRuleFromVariables;
import static com.sri.ai.util.Util.list;

import java.util.Random;
import java.util.function.Supplier;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;

/**
 * A conditional uniform distribution sampling factor.
 *  
 * @author braz
 *
 */
public class UniformSamplingFactor<T> extends AbstractSamplingFactor {

	private Variable variable;
	private Supplier sampler;
	private Potential potentialOfEachValue;
	private double estimatedSuccessWeight;
	
	public UniformSamplingFactor(
			Variable variable, 
			Supplier<T> sampler, 
			Potential potentialOfEachValue,
			double estimatedSuccessWeight,
			Random random) {
		
		super(list(variable), random);
		this.variable = variable;
		this.sampler = sampler;
		this.potentialOfEachValue = potentialOfEachValue;
		this.estimatedSuccessWeight = estimatedSuccessWeight;
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		if (sample.instantiates(variable)) {
			sample.updatePotential(potentialOfEachValue);
		}
		else {
			Object value = sampler.get();
			sample.getAssignment().set(variable, value);
		}
	}

	@Override
	public String toString() {
		return "Uniform(" + variable + ")";
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		return new DefaultSamplingRuleSet(samplingRuleFromVariables(this, list(variable), list(), estimatedSuccessWeight));
	}

}
