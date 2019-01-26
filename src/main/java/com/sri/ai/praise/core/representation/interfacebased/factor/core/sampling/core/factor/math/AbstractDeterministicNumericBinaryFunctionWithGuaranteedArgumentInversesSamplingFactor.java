package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractDeterministicFunctionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SpecificationForFunctionResultSamplingRule;

/**
 * A specialization of {@link AbstractDeterministicFunctionSamplingFactor} for numeric operators with two arguments.
 * 
 * @author braz
 *
 */
public abstract class AbstractDeterministicNumericBinaryFunctionWithGuaranteedArgumentInversesSamplingFactor
		extends AbstractDeterministicNumericBinaryFunctionSamplingFactor {

	public AbstractDeterministicNumericBinaryFunctionWithGuaranteedArgumentInversesSamplingFactor(Variable result, List<? extends Variable> arguments,
			Random random) {
		super(result, arguments, random);
	}

	////////////////////
	
	@Override
	protected Collection<? extends SamplingGoal> conditionsForInverseOfArgument(int i) {
		return list();
	}
	
	@Override
	protected Iterator<SpecificationForFunctionResultSamplingRule> specificationsForShortCircuitingSamplingRules() {
		return iterator();
		// there are no short-circuiting rules for the result;
		// if there were, there would be conditions for the inverse of arguments,
		// namely that the operation on the others were not short circuiting
		// (because then the argument's value becomes irrelevant, so there would be no way to choose a value for the inverse).
	}

}