package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist;

import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.fromIntegerListToIntArray;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
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
		ArrayTableFactor result = initializeNewFactorUnioningVariables(another);
		result = operateOnUnionedParameters(another, result, (a,b) -> a + b);
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
		ArrayTableFactor result = initializeNewFactorUnioningVariables(another);
		result = operateOnUnionedParameters(another, result, (a,b) -> a * b);
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SHARED SUPPORT FOR ADDING AND MULTIPLYING ////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private ArrayTableFactor initializeNewFactorUnioningVariables(TableFactor another) {
		LinkedHashSet<TableVariable> newSetOfVariables = new LinkedHashSet<>(this.variables);
		newSetOfVariables.addAll(another.getVariables());
		Integer numberOfParametersForNewListOfVariables = numberOfEntries(newSetOfVariables);
		ArrayList<Double> newParameters = arrayListFilledWith(-1.0, numberOfParametersForNewListOfVariables);	
		ArrayTableFactor newFactor = new ArrayTableFactor(new ArrayList<>(newSetOfVariables), newParameters);
		return newFactor;
	}

	private ArrayTableFactor operateOnUnionedParameters(TableFactor another, ArrayTableFactor result, BiFunction<Double, Double, Double> operator) {
		Iterator<ArrayList<Integer>> cartesianProduct = makeCartesianProductIterator(result.variables);
		LinkedHashMap<Variable, Integer> variableValueMap = new LinkedHashMap<>();
		for(ArrayList<Integer> values: in(cartesianProduct)) {
			variableValueMap = result.putAll(variableValueMap, values);
			Double product = operator.apply(this.getEntryFor(variableValueMap), another.getEntryFor(variableValueMap));
			result.setEntryFor(variableValueMap, product);
		}
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

//	@Override
	protected ArrayTableFactor sumOutEverythingExceptOld(List<? extends Variable> variablesToSumOut, ArrayList<? extends TableVariable> variablesNotToSumOut) {

		ArrayTableFactor result = new ArrayTableFactor(variablesNotToSumOut, 0.0);
		
		LinkedHashMap<Variable, Integer> assignment = new LinkedHashMap<>();
		for (ArrayList<Integer> values: in(makeCartesianProductIterator(variables))) {
			assignment = putAll(assignment, values);
			Double currentValue = result.getEntryFor(assignment);
			Double addedValue = getEntryFor(assignment);
			result.setEntryFor(assignment, currentValue + addedValue);
		}
		return result;
	}
	
	private LinkedHashMap<Variable,Integer> putAll(LinkedHashMap<Variable, Integer> assignment, ArrayList<Integer> values) {
		for (int i = 0; i < variables.size(); i++) {
			assignment.put(variables.get(i), values.get(i));
		}
		return assignment;
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

 		ArrayIndex remainingValues = makeArrayIndex(remaining);
		ArrayIndex eliminatedValues = makeArrayIndex(eliminated);

		int offsetInResult = 0; // we do not need to compute offset in resulting factor since it will be sequential
		do {
			double sumForTheseRemainingValues = 0;
			eliminatedValues.reset();
			do {
				sumForTheseRemainingValues += get(remainingValues.getOffset() + eliminatedValues.getOffset());
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
			
			
			var arrayIndex = new ArrayIndex(fromIntegerListToIntArray(getCardinalities()), getStrides());
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

	private ArrayIndex makeArrayIndex(List<? extends TableVariable> variablesSubSet) {
		ArrayList<Integer> indicesOfVariablesSubSetInOriginal = mapIntoArrayList(variablesSubSet, getVariables()::indexOf);
		ArrayList<Integer> eliminatedCardinalities = mapIntoArrayList(variablesSubSet, TableVariable::getCardinality);
		ArrayList<Integer> eliminatedStrides = mapIntoArrayList(indicesOfVariablesSubSetInOriginal, this::getStride);
		int[] eliminatedCardinalitiesArray = Util.fromIntegerListToIntArray(eliminatedCardinalities);
		int[] eliminatedStridesArray = Util.fromIntegerListToIntArray(eliminatedStrides);
		ArrayIndex arrayIndex = new ArrayIndex(eliminatedCardinalitiesArray, eliminatedStridesArray);
		return arrayIndex;
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
