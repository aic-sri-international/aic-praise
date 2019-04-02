package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRule;
import static com.sri.ai.util.Util.list;

import java.util.Random;
import java.util.function.Supplier;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;

/**
 * A {@link SamplingFactor} based on a {@link Supplier} providing values for a given variable
 * with uniform probability.
 * The supplier is assumed to be uniformly distributed and no check on this is performed.
 * 
 * @author braz
 *
 */
public class SupplierBasedUniformSamplingFactor extends AbstractSamplingFactor {

	private Variable variable;
	private Supplier<Object> function;
	private double estimatedSuccessProbability;
	
	public SupplierBasedUniformSamplingFactor(Variable variable, Supplier<Object> function, double estimatedSuccessProbability, Random random) {
		super(list(variable), random);
		this.variable = variable;
		this.function = function;
		this.estimatedSuccessProbability = estimatedSuccessProbability;
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		if ( ! sample.instantiates(variable)) {
			sample.set(variable, function.get());
		}
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		return new DefaultSamplingRuleSet(samplingRule(this, list(variable), list(), estimatedSuccessProbability));
	}

	@Override
	public String toString() {
		return "Uniform sampler for " + variable;
	}
}
