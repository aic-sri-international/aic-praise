package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule;

import static com.sri.ai.util.Util.assertType;

import java.util.Collection;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingState;
import com.sri.ai.util.planning.api.ContingentGoal;
import com.sri.ai.util.planning.api.Goal;
import com.sri.ai.util.planning.api.State;

/**
 * A type of {@link Goal} that depends on variables in a {@link Sample} to be satisfied.
 * @author braz
 * 
 *
 */
public interface SamplingGoal extends ContingentGoal {

	@Override
	default boolean isSatisfied(State state) {
		SamplingState sampleState = assertType(state, SamplingState.class, getClass());
		boolean result = isSatisfied(sampleState.getSample());
		return result;
	}

	boolean isSatisfied(Sample sample);

	/**
	 * Provides the variables on which this goal depends.
	 * @return
	 */
	Collection<? extends Variable> dependencies();
	
}
