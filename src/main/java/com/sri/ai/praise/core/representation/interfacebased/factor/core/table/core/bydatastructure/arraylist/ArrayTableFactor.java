package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist;

import static com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck.factorsAreEqual;
import static com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck.factorsHaveDifferentValues;
import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.castOrThrowError;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.listFrom;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.pair;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.ratioisInOnePlusOrMinusEpsilon;
import static com.sri.ai.util.Util.round;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.Util.setFrom;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.toIntArray;
import static com.sri.ai.util.Util.unorderedEquals;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.DoubleUnaryOperator;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ArrayUtils;

import com.google.common.primitives.Ints;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsAreOfIncomparableClasses;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsHaveDifferentVariables;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.AbstractTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.util.Timer;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.IntegerIterator;
import com.sri.ai.util.math.MixedRadixNumber;

/**
 * Discrete table implementation of {@link Factor}.
 * 
 * @author gabriel
 * @author bobak
 * @author braz
 *
 */

public class ArrayTableFactor extends AbstractTableFactor {
	
	////////////////////////////////////////////////////////////////////////////////////////////////////
	// DATA MEMBERS ////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////

	public static int maximumNumberOfEntriesToShow = 5;
	public static int decimalPlaces = -1;
	
	private final MixedRadixNumber parameterIndexRadix; // null if number of variables is 0.

	/*  NOTE:  Understanding the Parameter Order
	 * 
	 * Example: consider a factor with three binary variables v1, v2, and v3 in that same order
	 * 
	 * parameters will be arranged based on the following variable assignment order:
	 * 
	 * 		[ (v1=0,v2=0,v3=0), (v1=0,v2=0,v3=1), (v1=0,v2=1,v3=0), (v1=0,v2=1,v3=1), 
	 * 		  (v1=1,v2=0,v3=0), (v1=1,v2=0,v3=1), (v1=1,v2=1,v3=0), (v1=1,v2=1,v3=1) ]
	 * 
	 * Note that the order would change if the order the variables are stored is changed.
	 * 
	 * parameterIndexRadix is responsible for mapping the variable assignments to the correct parameter index.
	 */
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTORS /////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public ArrayTableFactor(String factorName, Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {
		super(factorName, variables);
		set(parameters);
		this.parameterIndexRadix = createMixedRadixNumberForIndexingFactorParameters();
	}

	public ArrayTableFactor(String factorName, Collection<? extends TableVariable> variables, double[] parameters) {
		super(factorName, variables);
		set(parameters);
		this.parameterIndexRadix = createMixedRadixNumberForIndexingFactorParameters();
	}
	
	public ArrayTableFactor(Collection<? extends TableVariable> variables) {
		this(variables, allocate(variables));
	}

	private static double[] allocate(Collection<? extends TableVariable> variables) {
		// println("Creating array table factor on " + variables);
		return new double[numberOfEntries(variables)];
	}

	public ArrayTableFactor(Collection<? extends TableVariable> variables, double[] parameters) {
		this("phi", variables, parameters);
	}
	
	public ArrayTableFactor(Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {
		this("phi", variables, parameters);
	}
	
	public ArrayTableFactor(Collection<? extends TableVariable> variables, Double defaultValue) {
		this(variables, arrayListFilledWith(defaultValue, numberOfEntries(variables)));
	}
	
	public static interface ParameterFunction {
		double apply(int[] values);
	}
	public static interface ParameterFunction1 { double apply(int v1); }
	public static interface ParameterFunction2 { double apply(int v1, int v2); }
	public static interface ParameterFunction3 { double apply(int v1, int v2, int v3); }
	public static interface ParameterFunction4 { double apply(int v1, int v2, int v3, int v4); }
	public static interface ParameterFunction5 { double apply(int v1, int v2, int v3, int v4, int v5); }
	public static interface ParameterFunction6 { double apply(int v1, int v2, int v3, int v4, int v5, int v6); }
	public static interface ParameterFunction7 { double apply(int v1, int v2, int v3, int v4, int v5, int v6, int v7); }
	public static interface ParameterFunction8 { double apply(int v1, int v2, int v3, int v4, int v5, int v6, int v7, int v8); }
	public static interface ParameterFunction9 { double apply(int v1, int v2, int v3, int v4, int v5, int v6, int v7, int v8, int v9); }
	
	public static ArrayTableFactor fromFunctionOnArray(Collection<? extends TableVariable> variables, ParameterFunction parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	public static ArrayTableFactor arrayTableFactor(Collection<? extends TableVariable> variables, ParameterFunction1 parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index[0]);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	public static ArrayTableFactor arrayTableFactor(Collection<? extends TableVariable> variables, ParameterFunction2 parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index[0], index.index[1]);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	public static ArrayTableFactor arrayTableFactor(Collection<? extends TableVariable> variables, ParameterFunction3 parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index[0], index.index[1], index.index[2]);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	public static ArrayTableFactor arrayTableFactor(Collection<? extends TableVariable> variables, ParameterFunction4 parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index[0], index.index[1], index.index[2], index.index[3]);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	public static ArrayTableFactor arrayTableFactor(Collection<? extends TableVariable> variables, ParameterFunction5 parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index[0], index.index[1], index.index[2], index.index[3], index.index[4]);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	public static ArrayTableFactor arrayTableFactor(Collection<? extends TableVariable> variables, ParameterFunction6 parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index[0], index.index[1], index.index[2], index.index[3], index.index[4], index.index[5]);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	public static ArrayTableFactor arrayTableFactor(Collection<? extends TableVariable> variables, ParameterFunction7 parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index[0], index.index[1], index.index[2], index.index[3], index.index[4], index.index[5], index.index[6]);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	public static ArrayTableFactor arrayTableFactor(Collection<? extends TableVariable> variables, ParameterFunction8 parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index[0], index.index[1], index.index[2], index.index[3], index.index[4], index.index[5], index.index[6], index.index[7]);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	public static ArrayTableFactor arrayTableFactor(Collection<? extends TableVariable> variables, ParameterFunction9 parameterFunction) {
		var factor = new ArrayTableFactor(variables, 0.0);
		var index = factor.makeArrayIndex(variables);
		do {
			factor.parameters[index.offset] = parameterFunction.apply(index.index[0], index.index[1], index.index[2], index.index[3], index.index[4], index.index[5], index.index[6], index.index[7], index.index[8]);
		} while (index.incrementIfPossible());
		return factor;
	}
	
	private MixedRadixNumber createMixedRadixNumberForIndexingFactorParameters() {
		ArrayList<Integer> cardinalities = getCardinalities();
		if (cardinalities.isEmpty()) {
			return null;
		}
		else {
			return new MixedRadixNumber(BigInteger.ZERO, cardinalities);
		}
	}

	public ArrayList<Integer> getCardinalities() {
		return mapIntoArrayList(variables, v->v.getCardinality());
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// PARAMETERS ACCESS - ARRAY ////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private double[] parameters;

	private void set(ArrayList<Double> parameters) {
		checkNumberOfParameters(parameters.size());
		this.parameters = ArrayUtils.toPrimitive(parameters.toArray(new Double[parameters.size()]));
	}

	private void set(double[] parameters) {
		checkNumberOfParameters(parameters.length);
		this.parameters = parameters;
	}

	private void checkNumberOfParameters(int numberOfParameters) {
		myAssert(numberOfParameters == this.numberOfEntries(), () -> "ArrayTableFactor: number of entries must be " + numberOfEntries() + " but got " + numberOfParameters + " parameters");
	}

	private double get(int i) {
		return parameters[i];
	}
	
	private double set(int i, double value) {
		return parameters[i] = value;
	}
	
	@Override
	protected boolean parametersAreAllEqual() {
		return Util.allEqual(parameters);
	}
	
	@Override
	protected String parametersString() {
		if (maximumNumberOfEntriesToShow != -1 && numberOfEntries() > maximumNumberOfEntriesToShow) {
			return "[greater than " + maximumNumberOfEntriesToShow + " entries]";
		}
		else {
			DoubleUnaryOperator rounder = decimalPlaces == -1? d -> d : d -> round(d, decimalPlaces);
			return "[" + Util.join(Arrays.stream(parameters).map(rounder).boxed().collect(Collectors.toList())) + "]";
		}
	}

	private double parametersAggregate(BinaryDoubleOperator operator, double initialValue) {
		double result = initialValue;
		for (int i = 0; i != parameters.length; i++) {
			result = operator.apply(result, parameters[i]);
		}
		return result;
	}
	
	private double parametersSum() {
		double result = 0;
		for (int i = 0; i != parameters.length; i++) {
			result += parameters[i];
		}
		return result;
	}
	
	private double[] parametersDividedBy(double divider) {
		double[] newEntries = new double[numberOfEntries()];
		for (int i = 0; i != parameters.length; i++) {
			newEntries[i] = parameters[i]/divider;
		}
		return newEntries;
	}

	private double[] parametersInversion() {
		double[] newEntries = new double[numberOfEntries()];
		for (int i = 0; i != parameters.length; i++) {
			newEntries[i] = 1.0/parameters[i];
		}
		return newEntries;
	}

	private double getParameter(int parameterIndex) {
		return parameters[parameterIndex];
	}

	@Override
	public ArrayList<Double> getEntries() {
		ArrayList<Double> result = new ArrayList<>(parameters.length);
		for (int i = 0; i != parameters.length; i++) {
			result.add(parameters[i]);
		}
		return result;
	}

	/**
	 * @deprecated
	 * @param defaultValue
	 */
	@Deprecated
	public void reinitializeEntries(Double defaultValue) {
		parameters = new double[numberOfEntries()];
		for (int i = 0; i != parameters.length; i++) {
			parameters[i] = defaultValue;
		}
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// BASIC METHODS ////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected boolean firstParameterIsZero() {
		return get(0) == 0;
	}

	@Override
	protected boolean thereAreZeroParameters() {
		return numberOfEntries() == 0;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SLICING //////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ArrayTableFactor slice(List<TableVariable> variables, List<Integer> values) {
		return (ArrayTableFactor) super.slice(variables, values);
	}

	@Override
	public ArrayTableFactor slice(Map<TableVariable, Integer> assignment) {
		return (ArrayTableFactor) super.slice(assignment);
	}

	@Override
	protected ArrayTableFactor slicePossiblyModifyingAssignment(
			Map<TableVariable, Integer> assignment,
			ArrayList<? extends TableVariable> remainingVariables) {

		// TODO: too slow! Use offset techniques
		
		ArrayTableFactor result = new ArrayTableFactor(remainingVariables, 1.0);
		Iterator<ArrayList<Integer>> assignmentsToRemainingVariables = makeCartesianProductIterator(remainingVariables);
		for (ArrayList<Integer> remainingVariablesValues: in(assignmentsToRemainingVariables)) {
			Util.putAll(assignment, remainingVariables, remainingVariablesValues);
			var value = getEntryFor(assignment);
			result.setEntryFor(assignment, value);
		}
		return result;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// ADDITION /////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected ArrayTableFactor addTableFactor(TableFactor another) {
		ArrayTableFactor result = new ArrayTableFactor(getVariables());
		ArrayTableFactor anotherArrayTableFactor = (ArrayTableFactor) another;
		for (int i = 0; i != numberOfEntries(); i++) {
			result.set(i, get(i) + anotherArrayTableFactor.get(i));
		}
		return result;
	}
	
	/**
	 * Not currently public, but useful in the implementation of another method.
	 */
	private ArrayTableFactor subtractTableFactor(TableFactor another) {
		ArrayTableFactor result = new ArrayTableFactor(getVariables());
		ArrayTableFactor anotherArrayTableFactor = (ArrayTableFactor) another;
		for (int i = 0; i != numberOfEntries(); i++) {
			result.set(i, get(i) - anotherArrayTableFactor.get(i));
		}
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// MULTIPLICATION ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	@Override
	public ArrayTableFactor multiply(Factor another) {
		return (ArrayTableFactor) super.multiply(another);
	}

	@Override
	protected ArrayTableFactor multiplyTableFactor(TableFactor another) {
		var anotherArrayTableFactor = castOrThrowError(getClass(), another, () -> "multiplyTableFactor supported for two " + getClass() + " objects only");
		
		Set<TableVariable> common = intersection(getVariables(), anotherArrayTableFactor.getVariables());
		List<TableVariable> exclusive1 = setDifference(getVariables(), common);
		List<TableVariable> exclusive2 = setDifference(anotherArrayTableFactor.getVariables(), common);
 		
		ArrayList<TableVariable> totalVariables = new ArrayList<>(exclusive1.size() + exclusive2.size() + common.size());
		totalVariables.addAll(exclusive1);
		totalVariables.addAll(exclusive2);
		totalVariables.addAll(common);
		ArrayTableFactor result = new ArrayTableFactor(totalVariables);
		
		ArrayIndex exclusive1Values = makeArrayIndex(exclusive1, this);
 		ArrayIndex exclusive2Values = makeArrayIndex(exclusive2, anotherArrayTableFactor);
		ArrayIndex commonValuesInThis = makeArrayIndex(common, this); // this and the one below could be combined into an ArrayIndex on multiple sets of strides
		ArrayIndex commonValuesInAnother = makeArrayIndex(common, anotherArrayTableFactor);

		int offsetInResult = 0;
		do {
			exclusive2Values.reset();
			do {
				commonValuesInThis.reset();
				commonValuesInAnother.reset();
				do {
					
					double value1 = get(exclusive1Values.offset() + commonValuesInThis.offset());
					double value2 = anotherArrayTableFactor.get(exclusive2Values.offset() + commonValuesInAnother.offset());
					
					result.set(offsetInResult, value1 * value2);
					
					offsetInResult++;
					
					commonValuesInThis.incrementIfPossible();
				} while (commonValuesInAnother.incrementIfPossible());
			} while (exclusive2Values.incrementIfPossible());
		} while (exclusive1Values.incrementIfPossible());
		
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// NORMALIZATION ////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected double computeNormalizationConstant() {
		return parametersSum();
	}

	@Override
	public ArrayTableFactor normalize() {
		return (ArrayTableFactor) super.normalize();
	}

	@Override
	protected ArrayTableFactor normalizeBy(Double normalizationConstant) {
		Double divider = normalizationConstant;
		var newEntries = parametersDividedBy(divider);
		return new ArrayTableFactor(getVariables(), newEntries);
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// AGGREGATION //////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private static interface BinaryDoubleOperator {
		double apply(double aggregatedValue, double newValue);
	}
	
	@Override
	public ArrayTableFactor sumOut(Collection<? extends Variable> variablesToSumOut) {
		return (ArrayTableFactor) super.sumOut(variablesToSumOut);
	}

	@Override
	public ArrayTableFactor max(Collection<? extends Variable> variablesToSumOut) {
		return (ArrayTableFactor) super.max(variablesToSumOut);
	}

	@Override
	public ArrayTableFactor min(Collection<? extends Variable> variablesToSumOut) {
		return (ArrayTableFactor) super.min(variablesToSumOut);
	}

	@Override
	public Factor potentialRange(Collection<? extends Variable> variablesToEliminate) {
		var min = min(variablesToEliminate);
		var max = max(variablesToEliminate);
		return max.subtractTableFactor(min);
	}

	@Override
	public double value() {
		myAssert(getVariables().size() == 0, () -> "value is valid only for factors with no variables.");
		return getParameter(0);
	}
	
	@Override
	protected ArrayTableFactor sumOut(List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining) {

//		if (summationCost() > 1000000)
//			println("ArrayTableFactor.sumOut summation cost: " + summationCost());
		
		var resultAndTime = Timer.getResultAndTime(() -> aggregate((a, v) -> a + v, 0, eliminated, remaining));

//		if (summationCost() > 1000000)
//			println("ArrayTableFactor.sumOut done, time: " + resultAndTime.second + " ms.") ;
		
		var result = resultAndTime.first;
		long time = resultAndTime.second;
		
		if (numberOfEntries() > Integer.MAX_VALUE) { // change to something like 200K to see how long different methods for iterating over the factor take
			println("\n" + getVariables());
			println("Time to eliminate variables from it: " + time + " ms.");
			println("Number of entries: " + numberOfEntries());
			
			MixedRadixNumber allValues;
			ArrayList<Integer> allIndices;
			double sum;
			long start, end;

			allValues = new MixedRadixNumber(0, getCardinalities());
			start = System.currentTimeMillis();
			while (allValues.canIncrement()) allValues.increment();
			end = System.currentTimeMillis();
			println("Time to iterate over all entries: "  + (end - start) + " ms.");
			
			
			var arrayIndex = new ArrayIndex(toIntArray(getCardinalities()), getStrides());
			start = System.currentTimeMillis();
			while (!arrayIndex.isOver()) arrayIndex.increment();
			end = System.currentTimeMillis();
			println("Time to iterate over all entries with ArrayIndex: "  + (end - start) + " ms.");
			
			
			var iterator = makeCartesianProductIterator(getVariables());
			start = System.currentTimeMillis();
			while (iterator.hasNext()) iterator.next();
			end = System.currentTimeMillis();
			println("Time to iterate over all entries with cartesian product iterator: "  + (end - start) + " ms.");
			
			
			allValues = new MixedRadixNumber(0, getCardinalities());
			allIndices = mapIntoArrayList(getVariables(), getVariables()::indexOf);
			start = System.currentTimeMillis();
			while (allValues.canIncrement()) {
				allValues.increment();
				sumOfValuesTimesStridesInOriginal(allValues, allIndices);
			}
			end = System.currentTimeMillis();
			println("Time to iterate over all entries and compute offsets: "  + (end - start) + " ms.");

			
			allValues = new MixedRadixNumber(0, getCardinalities());
			allIndices = mapIntoArrayList(getVariables(), getVariables()::indexOf);
			start = System.currentTimeMillis();
			sum = 0;
			while (allValues.canIncrement()) {
				allValues.increment();
				var offset = sumOfValuesTimesStridesInOriginal(allValues, allIndices);
				sum += get(offset);
			}
			end = System.currentTimeMillis();
			println("Time to iterate over all entries, compute offsets and sum all entries (" + sum + "): "  + (end - start) + " ms.");

			
			allValues = new MixedRadixNumber(0, getCardinalities());
			allIndices = mapIntoArrayList(getVariables(), getVariables()::indexOf);
			start = System.currentTimeMillis();
			sum = 0;
			while (allValues.canIncrement()) {
				allValues.increment();
				sum += get(100);
			}
			end = System.currentTimeMillis();
			println("Time to iterate over all entries, NOT compute offsets and sum fixed entries (" + sum + "): "  + (end - start) + " ms.");
		}
		return result;
	}

	@Override
	protected TableFactor max(List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining) {
		return aggregate((a, v) -> v > a ? v : a, Double.MIN_VALUE, eliminated, remaining);
	}

	@Override
	protected TableFactor min(List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining) {
		return aggregate((a, v) -> v < a ? v : a, Double.MAX_VALUE, eliminated, remaining);
	}

	private ArrayTableFactor aggregate(BinaryDoubleOperator operator, double initialValue, List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining) {
		
		if (eliminated.isEmpty()) {
			return this;
		}
		
		if (remaining.isEmpty()) {
			return new ArrayTableFactor(remaining, parametersAggregate(operator, initialValue));
		}
		
		ArrayTableFactor result = new ArrayTableFactor(remaining);
		
 		ArrayIndex remainingValues = makeArrayIndex(remaining, this);
		ArrayIndex eliminatedValues = makeArrayIndex(eliminated, this);

		int offsetInResult = 0; // we do not need to compute offset in resulting factor since it will be sequential
		do {
			double aggregatedValueForTheseRemainingValues = initialValue;
			eliminatedValues.reset();
			do {
				double value = get(remainingValues.offset() + eliminatedValues.offset());
				aggregatedValueForTheseRemainingValues = operator.apply(aggregatedValueForTheseRemainingValues, value);
			} while (eliminatedValues.incrementIfPossible());
			result.set(offsetInResult, aggregatedValueForTheseRemainingValues);
			offsetInResult++;
		} while (remainingValues.incrementIfPossible());
		
		return result;
	}

	private static ArrayIndex makeArrayIndex(Collection<? extends TableVariable> someVariables, ArrayTableFactor factor) {
		return factor.makeArrayIndex(someVariables);
	}

	private ArrayIndex makeArrayIndex(Collection<? extends TableVariable> someVariables) {
		var cardinalities = mapIntoList(someVariables, TableVariable::getCardinality);
		var strides = mapIntoList(someVariables, this::getStride);
		return new ArrayIndex(cardinalities, strides);
	}

	private int sumOfValuesTimesStridesInOriginal(MixedRadixNumber values, ArrayList<Integer> indicesOfVariablesInOriginal) {
		int result = 0;
		for (int i = 0; i != indicesOfVariablesInOriginal.size(); i++) {
			int stride = getStride(indicesOfVariablesInOriginal.get(i));
			int value = values.getCurrentNumeralValue(i);
			result += stride * value;
		}
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// NRMALIZATION /////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ArrayTableFactor normalize(Collection<? extends Variable> variablesToNormalize) {

		if (variablesToNormalize.isEmpty()) {
			// This means each element is normalized by itself, so we return a table of 1's.
			return new ArrayTableFactor(getVariables(), 1.0);
		}

		myAssert(getVariables().containsAll(variablesToNormalize), () -> "Not all variables to normalize occur in factor: " + variablesToNormalize + " not all in " + getVariables());

		// TODO: it's odd that sum takes a List and normalize takes a Collection, forcing us here to create a new list.
		
//		if (summationCost() > 1000000)
//			println("ArrayTableFactor.NORMALIZATION summation cost: " + summationCost());

		var resultAndTime = Timer.getResultAndTime(() -> divideByTableFactor(sumOut(listFrom(variablesToNormalize))));

//		if (summationCost() > 1000000)
//			println("ArrayTableFactor.NORMALIZATION done, time: " + resultAndTime.second + " ms.") ;

		return resultAndTime.first;
		
		// I wonder if reducing normalization to a division by a sum is as efficient
		// as writing dedicated code.
		// Having thought about it for a bit, it seems that it is, because doing this normalization in one swipe
		// would not work since we need to sum certain subsets of elements and then "go back" to compute their division
		// by their respective normalization constant.
		// Doing it like this does it in two swipes, which seems pretty much required anyway,
		// but stores all normalization constants in memory at once, thus incurring
		// in an allocation time overhead and memory overhead.
		// There is also a probably negligible overhead for copying variable lists more than once.
		// We may want to benchmark this at some point.
		// Right now I am leaving it since normalization is not used very often anyway.
	}


	private ArrayTableFactor divideByTableFactor(TableFactor another) {
		
		// ATTENTION: THIS IS BASICALLY A COPY OF multiplyTableFactor.
		// I've copied the code, only changing the basic operation line, to avoid any overhead since this is an inner loop.
		// Also, this is here for normalization and I didn't make division an interface method for Factor, but it is worth considering.
		// Note that implementing normalization by using multiplication and inversion would make it significantly more expensive,
		// performing a division of 1.0 by the normalization constant and then multiplying by the numerator. Might as well do it only once.
		
		var anotherArrayTableFactor = castOrThrowError(getClass(), another, () -> "divideTableFactor supported for two " + getClass() + " objects only");
		
		Set<TableVariable> common = intersection(getVariables(), anotherArrayTableFactor.getVariables());
		List<TableVariable> exclusive1 = setDifference(getVariables(), common);
		List<TableVariable> exclusive2 = setDifference(anotherArrayTableFactor.getVariables(), common);
 		
		ArrayList<TableVariable> totalVariables = new ArrayList<>(exclusive1.size() + exclusive2.size() + common.size());
		totalVariables.addAll(exclusive1);
		totalVariables.addAll(exclusive2);
		totalVariables.addAll(common);
		ArrayTableFactor result = new ArrayTableFactor(totalVariables);
		
		ArrayIndex exclusive1Values = makeArrayIndex(exclusive1, this);
 		ArrayIndex exclusive2Values = makeArrayIndex(exclusive2, anotherArrayTableFactor);
		ArrayIndex commonValuesInThis = makeArrayIndex(common, this); // these two could be combined into an ArrayIndex on multiple sets of strides
		ArrayIndex commonValuesInAnother = makeArrayIndex(common, anotherArrayTableFactor);

		int offsetInResult = 0;
		do {
			exclusive2Values.reset();
			do {
				commonValuesInThis.reset();
				commonValuesInAnother.reset();
				do {
					
					double value1 = get(exclusive1Values.offset() + commonValuesInThis.offset());
					double value2 = anotherArrayTableFactor.get(exclusive2Values.offset() + commonValuesInAnother.offset());
					
					result.set(offsetInResult, value1 / value2);
					
					offsetInResult++;
					
					commonValuesInThis.incrementIfPossible();
				} while (commonValuesInAnother.incrementIfPossible());
			} while (exclusive2Values.incrementIfPossible());
		} while (exclusive1Values.incrementIfPossible());
		
		return result;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INVERSION ////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ArrayTableFactor invert() {
		var newEntries = parametersInversion();
		return new ArrayTableFactor(getVariables(), newEntries);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEXED ACCESS - GETTERS /////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public Double getEntryFor(int[] values) {
		int parameterIndex = getParameterIndex(values);
		return getParameter(parameterIndex);
	}

	@Override
	public Double getEntryFor(ArrayList<Integer> values) {
		int parameterIndex = getParameterIndex(values);
		return getParameter(parameterIndex);
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEXED ACCESS - SETTERS /////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected void setEntryFor(int[] values, Double newParameterValue) {
		int parameterIndex = getParameterIndex(values);
		set(parameterIndex, newParameterValue);
	}
	
	@Override
	public void setEntryFor(ArrayList<Integer> values, Double newParameterValue) {
		int parameterIndex = getParameterIndex(values);
		set(parameterIndex, newParameterValue);
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEX UTILITIES //////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private int getParameterIndex(ArrayList<Integer> values) {
		int[] variableValuesArray = Ints.toArray(values);
		int parameterIndex = getParameterIndex(variableValuesArray);
		return parameterIndex;
	}
	
	private int getParameterIndex(int[] values) {
		if (parameterIndexRadix == null) {
			return 0;
		}
		else {
			return parameterIndexRadix.getValueFor(values).intValue();
		}
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// MATHEMATICALLY EQUALS ////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	public static final double MATHEMATICALLY_EQUALS_RATIO_EPSILON = 0.0001;
	
	@Override
	public boolean mathematicallyEquals(Factor another) {
		if (another instanceof ArrayTableFactor) {
			return mathematicallyEqualsAnotherArrayTableFactor((ArrayTableFactor) another);
		}
		else {
			return false;
		}
	}

	private boolean mathematicallyEqualsAnotherArrayTableFactor(ArrayTableFactor anotherArrayTableFactor) {
		if (haveSameVariables(anotherArrayTableFactor)) {
			return mathematicallyEqualsAnotherArrayTableFactorWithTheSameVariables(anotherArrayTableFactor);
		}
		else {
			return false;
		}
	}

	private boolean haveSameVariables(ArrayTableFactor anotherArrayTableFactor) {
		return unorderedEquals(getVariables(), anotherArrayTableFactor.getVariables());
	}

	private boolean mathematicallyEqualsAnotherArrayTableFactorWithTheSameVariables(ArrayTableFactor anotherArrayTableFactor) {
		return findEqualityViolatingPairOfArrayIndicesOrderedByVariablesOfThis(anotherArrayTableFactor) == null;
	}

	/**
	 * Returns null if other array table factor is the same up to a ratio, or
	 * a pair of array indices (with indices ordered according to <code>this.getVariables()</code> for which there is a difference.
	 */
	private Pair<ArrayIndex, ArrayIndex> findEqualityViolatingPairOfArrayIndicesOrderedByVariablesOfThis(ArrayTableFactor anotherArrayTableFactor) {
		ArrayIndex index1 = makeArrayIndex(getVariables(), this);
		ArrayIndex index2 = makeArrayIndex(getVariables(), anotherArrayTableFactor);
		// note that we use the getVariables() for both so that they go over the same assignment order.
		
		do {
			if (!nextPositionsAreEqual(this, index1, anotherArrayTableFactor, index2)) {
				return pair(index1, index2);
				// note that we must immediately either break or return when finding violating pair (as opposing to setting a flag to be checked in 'while' condition),
				// or otherwise index2 would move on from violating position
			}
			index2.incrementIfPossible();
		} while (index1.incrementIfPossible());
		
		return null;
	}
	
	private static boolean nextPositionsAreEqual(
			ArrayTableFactor arrayTableFactor1,
			ArrayIndex index1,
			ArrayTableFactor arrayTableFactor2,
			ArrayIndex index2) {
		
		double value1 = arrayTableFactor1.get(index1.offset());
		double value2 = arrayTableFactor2.get(index2.offset());
		return compare(value1, value2);
	}

	private static boolean compare(double value1, double value2) {
		return ratioisInOnePlusOrMinusEpsilon(value1, value2, MATHEMATICALLY_EQUALS_RATIO_EPSILON);
	}

	@Override
	public FactorsEqualityCheck checkEquality(Factor another) {
		if (another instanceof ArrayTableFactor) {
			return checkEqualityAgainstAnotherArrayTableFactor((ArrayTableFactor) another);
		}
		else {
			return new DefaultFactorsAreOfIncomparableClasses<>(this, another);
		}
	}

	private FactorsEqualityCheck checkEqualityAgainstAnotherArrayTableFactor(ArrayTableFactor anotherArrayTableFactor) {
		if (haveSameVariables(anotherArrayTableFactor)) {
			return checkEqualityAgainstAnotherArrayTableFactorWithTheSameVariables(anotherArrayTableFactor);
		}
		else {
			var variablesInFirstButNotInSecond = setFrom(subtract(getVariables(), anotherArrayTableFactor.getVariables()));
			var variablesInSecondButNotInFirst = setFrom(subtract(anotherArrayTableFactor.getVariables(), getVariables()));
			return new DefaultFactorsHaveDifferentVariables<>(
					this, anotherArrayTableFactor, variablesInFirstButNotInSecond, variablesInSecondButNotInFirst);
		}
	}

	private FactorsEqualityCheck checkEqualityAgainstAnotherArrayTableFactorWithTheSameVariables(
			ArrayTableFactor anotherArrayTableFactor) {
		
		var violatingPair = findEqualityViolatingPairOfArrayIndicesOrderedByVariablesOfThis(anotherArrayTableFactor);
		
		if (violatingPair != null) {
			var violatingAssignment = listFrom(violatingPair.first.index());
			var valueInFirst = get(violatingPair.first.offset());
			var valueInSecond = anotherArrayTableFactor.get(violatingPair.second.offset());
			return 
					factorsHaveDifferentValues(
							this, anotherArrayTableFactor, 
							violatingAssignment, 
							valueInFirst, valueInSecond);
		}
		else {
			return factorsAreEqual(this, anotherArrayTableFactor);
		}
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// equals and hashCode //////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	@Override
	public int hashCode() {
		return getVariables().hashCode()*31 + parameters.hashCode();
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// PROBABLY UNNECESSARILY PUBLIC ////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	public static Iterator<ArrayList<Integer>> makeCartesianProductIterator(Collection<? extends TableVariable> variables) {
		List<NullaryFunction<Iterator<? extends Integer>>> makersOfIteratorsOverValues = 
				mapIntoList(variables, v -> () -> new IntegerIterator(0, v.getCardinality()));
		
		return new CartesianProductIterator<Integer>(makersOfIteratorsOverValues);
	}
	
}
