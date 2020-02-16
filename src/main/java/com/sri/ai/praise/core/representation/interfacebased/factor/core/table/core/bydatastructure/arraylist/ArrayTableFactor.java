package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist;

import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.castOrThrowError;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.listFrom;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.Util.toIntArray;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ArrayUtils;

import com.google.common.primitives.Ints;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.AbstractTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
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
		this(variables, new double[numberOfEntries(variables)]);
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
		this.parameters = ArrayUtils.toPrimitive(parameters.toArray(new Double[parameters.size()]));
	}

	private void set(double[] parameters) {
		this.parameters = parameters;
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
		return "[" + Util.join(Arrays.stream(parameters).boxed().collect(Collectors.toList())) + "]";
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
		ArrayTableFactor result;
		Double divider = normalizationConstant;
		var newEntries = parametersDividedBy(divider);
		result = new ArrayTableFactor(getVariables(), newEntries);
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SUMMING OUT ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ArrayTableFactor sumOut(List<? extends Variable> variablesToSumOut) {
		return (ArrayTableFactor) super.sumOut(variablesToSumOut);
	}

	@Override
	protected ArrayTableFactor sumOutEverythingExcept(List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining) {

		if (eliminated.isEmpty()) {
			return this;
		}
		
		ArrayTableFactor result = new ArrayTableFactor(remaining);
		
		if (remaining.isEmpty()) {
			result.set(0, parametersSum());
			return result;
		}
		
		long start, end;
		
		start = System.currentTimeMillis();

 		ArrayIndex remainingValues = makeArrayIndex(remaining, this);
		ArrayIndex eliminatedValues = makeArrayIndex(eliminated, this);

		int offsetInResult = 0; // we do not need to compute offset in resulting factor since it will be sequential
		do {
			double sumForTheseRemainingValues = 0;
			eliminatedValues.reset();
			do {
				sumForTheseRemainingValues += get(remainingValues.offset() + eliminatedValues.offset());
			} while (eliminatedValues.incrementIfPossible());
			result.set(offsetInResult, sumForTheseRemainingValues);
			offsetInResult++;
		} while (remainingValues.incrementIfPossible());

		end = System.currentTimeMillis();
		
		if (numberOfEntries() > Integer.MAX_VALUE) { // change to something like 200K to see how long different methods for iterating over the factor take
			println("\n" + getVariables());
			println("Time to eliminate variables from it: " + (end - start) + " ms.");
			println("Number of entries: " + numberOfEntries());
			
			MixedRadixNumber allValues;
			ArrayList<Integer> allIndices;
			double sum;
			

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

	private ArrayIndex makeArrayIndex(Collection<? extends TableVariable> someVariables, ArrayTableFactor factor) {
		var cardinalities = mapIntoList(someVariables, TableVariable::getCardinality);
		var strides = mapIntoList(someVariables, factor::getStride);
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
		
		return divideByTableFactor(sumOut(listFrom(variablesToNormalize)));
		
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
	protected Double getEntryFor(int[] values) {
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
	// PROBABLY UNNECESSARILY PUBLIC ////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	public static Iterator<ArrayList<Integer>> makeCartesianProductIterator(Collection<? extends TableVariable> variables) {
		List<NullaryFunction<Iterator<? extends Integer>>> makersOfIteratorsOverValues = 
				mapIntoList(variables, v -> () -> new IntegerIterator(0, v.getCardinality()));
		
		return new CartesianProductIterator<Integer>(makersOfIteratorsOverValues);
	}
	
}
