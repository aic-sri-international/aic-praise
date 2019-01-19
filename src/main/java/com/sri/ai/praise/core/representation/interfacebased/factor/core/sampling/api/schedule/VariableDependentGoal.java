package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.planning.api.Goal;

/**
 * A type of {@link Goal} that depends on information on a {@link Variable} to be satisfied.
 * @author braz
 * 
 *
 */
public interface VariableDependentGoal extends Goal {

	/**
	 * Provides the variables on which this goal depends.
	 * @return
	 */
	Collection<? extends Variable> dependencies();
	
}
