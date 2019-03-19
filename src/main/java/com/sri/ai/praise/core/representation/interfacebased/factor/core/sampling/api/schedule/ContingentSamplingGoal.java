package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule;

import static com.sri.ai.util.Util.assertType;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingState;
import com.sri.ai.util.planning.api.ContingentGoal;
import com.sri.ai.util.planning.api.State;

/**
 * A {@link SamplingGoal} that is also a {@link ContingentGoal},
 * defining a default method for {@link #isSatisfied(State)}
 * that reduces to {@link SamplingGoal#isSatisfied(Sample)}.
 *  
 * @author braz
 *
 */
public interface ContingentSamplingGoal extends ContingentGoal, SamplingGoal {

	/**
	 * Implements {@link #isSatisfied(State)} by assuming the state is a {@link SampleState}
	 * and invoking {@link SamplingGoal#isSatisfied(Sample)} on it.
	 */
	@Override
	default boolean isSatisfied(State state) {
		SamplingState sampleState = assertType(state, SamplingState.class, getClass());
		boolean result = isSatisfied(sampleState.getSample());
		return result;
	}
	
}
