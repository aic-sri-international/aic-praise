package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import static com.sri.ai.util.Util.myAssert;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.util.function.api.variables.Unit;
import com.sri.ai.util.function.core.values.SetOfRealValues;
import com.sri.ai.util.function.core.variables.RealVariable;

public class FromRealExpressionVariableToRealVariableWithRange {

	public static RealVariable makeRealVariableWithRange(String name, RealInterval type, int numberOfDiscreteValues, Context context) {
		
		myAssert(numberOfDiscreteValues > 0, () -> ExpressionSamplingFactor.class + " requires a positive number of discrete values but received " + numberOfDiscreteValues + " for variable " + name);
		myAssert(type.boundsAreConstants(),  () -> ExpressionSamplingFactor.class + " requires real-valued variables to have constant bounds, but got " + name + " in " + type);
		myAssert(!type.noLowerBound(),       () -> ExpressionSamplingFactor.class + " requires real-valued variables to be bounded, but got " + name + " in " + type);
		myAssert(!type.noUpperBound(),       () -> ExpressionSamplingFactor.class + " requires real-valued variables to be bounded, but got " + name + " in " + type);
		
		SetOfRealValues setOfRealValues = makeSetOfRealValues(type, numberOfDiscreteValues);
		
		RealVariable result = new RealVariable(name, Unit.NONE, setOfRealValues); // TODO: accept option specification of units for each of the factor's variables
		return result;
	}

	public static SetOfRealValues makeSetOfRealValues(RealInterval type, int numberOfDiscreteValues) {
		BigDecimal lowerBound = new BigDecimal(type.getLowerBound().toString());
		BigDecimal upperBound = new BigDecimal(type.getUpperBound().toString());
	
		BigDecimal step = computeStep(type, numberOfDiscreteValues, lowerBound, upperBound);
	
		SetOfRealValues setOfRealValues = assembleSetOfRealValues(type, lowerBound, step, upperBound);
		return setOfRealValues;
	}

	public static BigDecimal computeStep(RealInterval type, int numberOfDiscreteValues, BigDecimal lowerBound, BigDecimal upperBound) {
		
		// If the interval is closed on both ends,
		// we have the bounds to be the first and last discrete values,
		// and step to be the upper minus lower bound, divided by the number of gaps, which is the number of discrete values minus 1.
		// However, if a bound is open, we do not use that bound as a value,
		// and instead use lower bound plus step as the first discretized value, and upper bound minus step as the last discretized value.
		// However, doing so will create less discretized points than planned (we are throwing away the open bounds).
		// So to compensate for that think in terms of "delimiters", which are the discretized values plus the open bounds,
		// and use those to compute the step.
		int numberOfDelimiters = numberOfDiscreteValues + (type.lowerBoundIsOpen()? 1 : 0) + (type.upperBoundIsOpen()? 1 : 0);
		BigDecimal numberOfGaps = new BigDecimal(numberOfDelimiters).subtract(new BigDecimal(1));
		BigDecimal step = upperBound.subtract(lowerBound).divide(numberOfGaps, new MathContext(16, RoundingMode.FLOOR));
		return step;
	}

	public static SetOfRealValues assembleSetOfRealValues(RealInterval type, BigDecimal lowerBound, BigDecimal step, BigDecimal upperBound) {
		SetOfRealValues setOfRealValues;
		if (step.compareTo(BigDecimal.ZERO) <= 0) {
			setOfRealValues = assembleSetOfRealValuesWithNoMoreThanOnePoint(type, lowerBound, step);
		}
		else {
			setOfRealValues = assembleSetOfRealValuesWithMoreThanOnePoint(type, lowerBound, step, upperBound);
		}
		return setOfRealValues;
	}

	public static SetOfRealValues assembleSetOfRealValuesWithNoMoreThanOnePoint(RealInterval type, BigDecimal lowerBound, BigDecimal step) {
		SetOfRealValues setOfRealValues;
		if (!type.lowerBoundIsOpen() && !type.upperBoundIsOpen()) {
			// singleton set
			setOfRealValues = new SetOfRealValues(lowerBound, BigDecimal.ZERO, lowerBound);
		}
		else {
			// empty set of values, which means an upper bound smaller than the lower bound
			setOfRealValues = new SetOfRealValues(lowerBound, step, lowerBound.subtract(BigDecimal.ONE));
		}
		return setOfRealValues;
	}

	public static SetOfRealValues assembleSetOfRealValuesWithMoreThanOnePoint(RealInterval type, BigDecimal lowerBound, BigDecimal step, BigDecimal upperBound) {
		SetOfRealValues setOfRealValues;
		
		BigDecimal firstValue = lowerBound;
		if (type.lowerBoundIsOpen()) {
			firstValue = firstValue.add(step);
		}
	
		BigDecimal lastValue = upperBound;
		if (type.upperBoundIsOpen()) {
			lastValue = lastValue.subtract(step);
		}
	
		setOfRealValues = new SetOfRealValues(firstValue, step, lastValue);
		setOfRealValues.setLowerBoundForDiscretizedValue(lowerBound);
		setOfRealValues.setUpperBoundForDiscretizedValue(upperBound);
		return setOfRealValues;
	}

}
