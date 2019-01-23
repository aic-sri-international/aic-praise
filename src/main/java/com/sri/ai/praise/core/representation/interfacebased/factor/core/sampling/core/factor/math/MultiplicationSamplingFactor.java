package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math;

import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.FunctionOnSetOfVariablesSatisfiesCondition;
import com.sri.ai.util.collect.IntegerIterator;

/**
 * A {@link AbstractCommutativeAssociativeRingSamplingFactor} instantiated for multiplication.  
 * @author braz
 *
 */
public class MultiplicationSamplingFactor extends AbstractCommutativeAssociativeRingSamplingFactor<Double> {

	public MultiplicationSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	@Override
	protected Double computeMissingArgument(Double functionResultValue, Double definedArgumentsOperatorApplication, int missingArgumentIndex) {
		double result = functionResultValue / definedArgumentsOperatorApplication;
		return result;
	}

	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return new IntegerIterator(0, getArguments().size()); // none
	}

	private Map<Integer, Collection<? extends SamplingGoal>> conditionsForInverseOfArgumentCache = map();
	
	@Override
	protected Collection<? extends SamplingGoal> conditionsForInverseOfArgument(int i) {
		return getValuePossiblyCreatingIt(conditionsForInverseOfArgumentCache, i, this::makeConditionsForInverseOfArgument);
	}

	private Collection<? extends SamplingGoal> makeConditionsForInverseOfArgument(int i) {
		FunctionOnSetOfVariablesSatisfiesCondition<Double> 
		productOfOtherArgumentsIsNotZero = 
		new FunctionOnSetOfVariablesSatisfiesCondition<Double>(
				"productIsNotZero",
				getArgumentsOtherThan(i), 
				c -> evaluateFunctionFromAllArgumentsValues(c.iterator()), 
				v -> v != 0.0);
		return list(productOfOtherArgumentsIsNotZero);
	}
	
	@Override
	protected Double getIdentityElement() {
		return 1.0;
	}

	@Override
	protected Double getAbsorbingElement() {
		return 0.0;
	}

	@Override
	protected Double apply(Double v1, Double v2) {
		return v1 * v2;
	}

	@Override
	protected Class<Double> getValueClass() {
		return Double.class;
	}

	@Override
	protected String getFunctionName() {
		return "*";
	}

}
