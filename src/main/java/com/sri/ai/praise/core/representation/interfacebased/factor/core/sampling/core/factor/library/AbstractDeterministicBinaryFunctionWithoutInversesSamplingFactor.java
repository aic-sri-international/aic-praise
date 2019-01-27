package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library;

import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SpecificationForFunctionResultSamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.base.AbstractDeterministicBinaryFunctionSamplingFactor;

public abstract class AbstractDeterministicBinaryFunctionWithoutInversesSamplingFactor<A, B, R> extends AbstractDeterministicBinaryFunctionSamplingFactor<A, B, R> {

	public AbstractDeterministicBinaryFunctionWithoutInversesSamplingFactor(
			Variable functionResult, 
			ArrayList<? extends Variable> arguments, 
			Random random) {
		
		super(functionResult, arguments, random);
	}
	
	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return iterator(); 
	}

	@Override
	protected Collection<? extends SamplingGoal> conditionsForInverseOfArgument(int i) {
		return list();
	}

	@Override
	protected Iterator<SpecificationForFunctionResultSamplingRule> specificationsForShortCircuitingSamplingRules() {
		return iterator();
	}

	@Override
	protected A computeFirstFromOthers(B secondValue, R functionResultValue) {
		throw new Error(getClass().getSimpleName() + " does not provide inverse argument sampling");
	}

	@Override
	protected B computeSecondFromOthers(A firstValue, R functionResultValue) {
		throw new Error(getClass().getSimpleName() + " does not provide inverse argument sampling");
	}

	@Override
	protected boolean isInvalidFirstArgument(A value) {
		return false;
	}

	@Override
	protected boolean isInvalidSecondArgument(B value) {
		return false;
	}
}