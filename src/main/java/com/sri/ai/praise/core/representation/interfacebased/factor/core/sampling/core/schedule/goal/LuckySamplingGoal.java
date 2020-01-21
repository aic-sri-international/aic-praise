package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal;

import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.ContingentSamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;

/**
 * A sampling goal meant to check for conditions in variables already marginalized.
 * For example, suppose that a factor on X, Y has a sampling rule X <- Y, Y != 0.
 * Then if we marginalize Y, we still want the resulting factor to indicate
 * that it can instantiate X, but this is not an absolute statement.
 * We cannot provide a sampling rule X <- Y, Y != 0 for Y because Y is not a variable in the marginalized factor.
 * The solution is to provide X <- lucky(inner factor, Y != 0).
 * This goal samples all variables of the inner factor and tests the contingent goal Y != 0.
 * The sampling rule will therefore only proceed if Y is sampled to a non-zero value.
 * Otherwise, the planner will use other ways to instantiate X.
 * 
 * @author braz 
 *
 */
public class LuckySamplingGoal extends AbstractVariablesRelatedGoal implements ContingentSamplingGoal {

	private SamplingFactor factor;
	private SamplingGoal innerGoal;
	
	public LuckySamplingGoal(SamplingGoal innerGoal, SamplingFactor factor) {
		super(list());
		this.factor = factor;
		this.innerGoal = innerGoal;
	}

	@Override
	public boolean isSatisfied(Sample sample) {
		// TODO: this is violating the constraint of not modifying the sample; that is one of the reasons LuckySamplingGoal is going away.
		factor.sampleOrWeigh(sample);
		boolean result = innerGoal.isSatisfied(sample);
		return result;
	}

	@Override
	public String getGoalName() {
		return "lucky[" + innerGoal + ", " + factor + "]";
	}

}
