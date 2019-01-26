package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Random;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ConditionedSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SpecificationForFunctionResultSamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.AbstractDeterministicBinaryFunctionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableEqualsGoal;

public class EqualitySamplingFactor extends AbstractDeterministicBinaryFunctionSamplingFactor<Object, Object, Boolean> {

	public static SamplingFactor equalitySamplingFactor(Variable variable1, Variable variable2, Random random) {
		Variable truth = new DefaultExpressionVariable(Expressions.TRUE);
		SamplingFactor equalityAsFunction = new EqualitySamplingFactor(truth, arrayList(variable1, variable2), random);
		Sample sample = DefaultSample.makeFreshSample();
		sample.getAssignment().set(truth, true);
		SamplingFactor equalityFactor = ConditionedSamplingFactor.condition(equalityAsFunction, sample);
		return equalityFactor;
	}

	public EqualitySamplingFactor(Variable functionResult, ArrayList<? extends Variable> arguments, Random random) {
		super(functionResult, arguments, random);
	}
	
	@Override
	protected String operatorSymbol() {
		return "=";
	}

	@Override
	protected Boolean operation(Object firstValue, Object secondValue) {
		return firstValue.equals(secondValue);
	}

	@Override
	protected Collection<? extends SamplingGoal> conditionsForInverseOfArgument(int i) {
		return list(new VariableEqualsGoal(getFunctionResult(), true));
	}

	@Override
	protected Iterator<SpecificationForFunctionResultSamplingRule> specificationsForShortCircuitingSamplingRules() {
		return iterator();
	}

	@Override
	protected Object computeFirstFromOthers(Object secondValue, Boolean functionResultValue) {
		myAssert(functionResultValue, () -> getClass().getSimpleName() + " asked to set first argument " + getFirst() + " equal to " + getSecond() + " but " + getFunctionResult() + " is set to false");
		return secondValue;
	}

	@Override
	protected Object computeSecondFromOthers(Object firstValue, Boolean functionResultValue) {
		myAssert(functionResultValue, () -> getClass().getSimpleName() + " asked to set second argument " + getSecond() + " equal to " + getFirst() + " but " + getFunctionResult() + " is set to false");
		return firstValue;
	}

	@Override
	protected boolean isInvalidFunctionResult(Boolean value) {
		return false;
	}

	@Override
	protected boolean isInvalidFirstArgument(Object value) {
		return false;
	}

	@Override
	protected boolean isInvalidSecondArgument(Object value) {
		return false;
	}

	@Override
	protected String getFunctionName() {
		return "equality";
	}

	@Override
	public String toString() {
		return getFunctionResult() + " = (" + getFirst() + " = " + getSecond() + ")";
	}
}