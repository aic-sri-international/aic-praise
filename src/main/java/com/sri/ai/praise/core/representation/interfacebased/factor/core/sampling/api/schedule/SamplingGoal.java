package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.util.planning.api.Goal;

/**
 * A type of {@link Goal} that depends on variables in a {@link Sample} to be satisfied.
 * @author braz
 * 
 *
 */
public interface SamplingGoal extends Goal {

	/**
	 * Determines whether this goal is satisfied by this sample (without being allowed to modify the sample).
	 * @param sample
	 * @return
	 */
	boolean isSatisfied(Sample sample);

	/**
	 * Provides the variables on which this goal depends.
	 * @return
	 */
	Collection<? extends Variable> getVariables();
	
}
