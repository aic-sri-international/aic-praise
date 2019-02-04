package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.list;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Potential;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;

/**
 * A sampling factor that weighs in with a fixed potential
 * if a boolean variable is instantiated as a given boolean function,
 * and does nothing otherwise.
 * <p>
 * Two such sampling factors on a variable and its negation
 * with non-negative potentials summing up to 1 
 * are equivalent to a Bernoulli factor, hence the name.
 * 
 * @author braz
 *
 */
public class HalfBernoulliSamplingFactor extends AbstractSamplingFactor {

	private Variable variable;
	private Boolean truthValue;
	private Potential potential;
	
	public HalfBernoulliSamplingFactor(
			Variable variable, 
			boolean truthValue, 
			Potential potential, 
			Random random) {
		
		super(list(variable), random);
		this.variable = variable;
		this.truthValue = truthValue;
		this.potential = potential;
	}

	@Override
	public void sampleOrWeigh(Sample sample) {
		if (applies(sample)) {
			sample.updatePotential(potential);
		}
	}

	public boolean applies(Sample sample) {
		boolean result = 
				sample.instantiates(variable) 
				&& 
				sample.getAssignment().get(variable).equals(truthValue);
		return result;
	}

	@Override
	protected SamplingRuleSet makeSamplingRules() {
		return new DefaultSamplingRuleSet(arrayList());
	}

}
