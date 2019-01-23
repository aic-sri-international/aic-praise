package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.number;

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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.math.AbstractAssociativeCommutativeSemiRingSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.FunctionOnSetOfVariablesSatisfiesCondition;
import com.sri.ai.util.collect.IntegerIterator;

/**
 * A {@link AbstractAssociativeCommutativeSemiRingSamplingFactor} instantiated for
 * operation that has an inverse for arguments as long as operation on remaining arguments does not result
 * in the absorbing element.  
 * @author braz
 *
 */
public abstract class AbstractAssociativeCommutativeSemiRingWithInverseDependingOnOperationOnOtherArgumentBeingDifferentFromAbsorbingElementSamplingFactor<T> 
extends AbstractAssociativeCommutativeSemiRingSamplingFactor<T> {

	public AbstractAssociativeCommutativeSemiRingWithInverseDependingOnOperationOnOtherArgumentBeingDifferentFromAbsorbingElementSamplingFactor(Variable result, List<? extends Variable> arguments, Random random) {
		super(result, arguments, random);
	}

	@Override
	protected Iterator<? extends Integer> argumentsWithInverseFunctionIterator() {
		return new IntegerIterator(0, getArguments().size());
	}

	private Map<Integer, Collection<? extends SamplingGoal>> conditionsForInverseOfArgumentCache = map();
	
	@Override
	protected Collection<? extends SamplingGoal> conditionsForInverseOfArgument(int i) {
		return getValuePossiblyCreatingIt(conditionsForInverseOfArgumentCache, i, this::makeConditionsForInverseOfArgument);
	}

	private Collection<? extends SamplingGoal> makeConditionsForInverseOfArgument(int i) {
		FunctionOnSetOfVariablesSatisfiesCondition<T> 
		operationOnOtherArgumentsIsNotAbsorbingElement = 
		new FunctionOnSetOfVariablesSatisfiesCondition<T>(
				getFunctionName() + "IsNot" + getAbsorbingElement(),
				getArgumentsOtherThan(i), 
				c -> evaluateFunctionFromAllArgumentsValues(c.iterator()), 
				v -> !isAbsorbingElement(v));
		return list(operationOnOtherArgumentsIsNotAbsorbingElement);
	}

}
