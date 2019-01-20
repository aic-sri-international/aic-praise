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

	boolean isSatisfied(Sample sample);
	
	/**
	 * Provides the variables on which this goal depends.
	 * @return
	 */
	Collection<? extends Variable> dependencies();
	
}
