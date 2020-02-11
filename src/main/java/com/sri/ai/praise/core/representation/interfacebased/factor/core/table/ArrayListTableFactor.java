package com.sri.ai.praise.core.representation.interfacebased.factor.core.table;

import static com.sri.ai.util.Util.allEqual;
import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.sum;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

import com.google.common.primitives.Ints;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
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

public class ArrayListTableFactor extends AbstractTableFactor {
	
	////////////////////////////////////////////////////////////////////////////////////////////////////
	// DATA MEMBERS ////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private ArrayList<Double> parameters;
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
	
	public ArrayListTableFactor(String factorName, Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {
		super(factorName, variables);
	
		this.parameters = parameters;
		this.parameterIndexRadix = createMixedRadixNumberForIndexingFactorParameters();
	}
	
	public ArrayListTableFactor(Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {
		this("phi", variables, parameters);
	}
	
	public ArrayListTableFactor(Collection<? extends TableVariable> variables, Double defaultValue) {
		this(variables, arrayListFilledWith(defaultValue, numberOfEntries(variables)));
	}
	
	public ArrayListTableFactor(Collection<? extends TableVariable> variables) {
		this(variables, -1.0);
	}

	private MixedRadixNumber createMixedRadixNumberForIndexingFactorParameters() {
		ArrayList<Integer> cardinalities = getNumberOfEntries();
		if (cardinalities.isEmpty()) {
			return null;
		}
		else {
			return new MixedRadixNumber(BigInteger.ZERO, cardinalities);
		}
	}

	public ArrayList<Integer> getNumberOfEntries() {
		return mapIntoArrayList(variables, v->v.getCardinality());
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// BASIC METHODS ////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected boolean firstParameterIsZero() {
		return parameters.get(0) == 0;
	}

	@Override
	protected boolean thereAreZeroParameters() {
		return numberOfEntries() == 0;
	}
	
	@Override
	protected boolean parametersAreAllEqual() {
		return allEqual(parameters);
	}

	@Override
	protected String parametersString() {
		return parameters.toString();
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SLICING //////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ArrayListTableFactor slice(List<TableVariable> variables, List<Integer> values) {
		return (ArrayListTableFactor) super.slice(variables, values);
	}

	@Override
	public ArrayListTableFactor slice(Map<TableVariable, Integer> assignment) {
		return (ArrayListTableFactor) super.slice(assignment);
	}

	@Override
	protected ArrayListTableFactor slicePossiblyModifyingAssignment(
			Map<TableVariable, Integer> assignment,
			ArrayList<? extends TableVariable> remainingVariables) {
		
		ArrayListTableFactor result = new ArrayListTableFactor(remainingVariables);
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
	protected ArrayListTableFactor addTableFactor(TableFactor another) {
		ArrayListTableFactor result = initializeNewFactorUnioningVariables(another);
		result = operateOnUnionedParameters(another, result, (a,b) -> a + b);
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// MULTIPLICATION ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	@Override
	public ArrayListTableFactor multiply(Factor another) {
		return (ArrayListTableFactor) super.multiply(another);
	}

	@Override
	protected ArrayListTableFactor multiplyTableFactor(TableFactor another) {
		ArrayListTableFactor result = initializeNewFactorUnioningVariables(another);
		result = operateOnUnionedParameters(another, result, (a,b) -> a * b);
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SHARED SUPPORT FOR ADDING AND MULTIPLYING ////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private ArrayListTableFactor initializeNewFactorUnioningVariables(TableFactor another) {
		LinkedHashSet<TableVariable> newSetOfVariables = new LinkedHashSet<>(this.variables);
		newSetOfVariables.addAll(another.getVariables());
		Integer numberOfParametersForNewListOfVariables = numberOfEntries(newSetOfVariables);
		ArrayList<Double> newParameters = arrayListFilledWith(-1.0, numberOfParametersForNewListOfVariables);	
		ArrayListTableFactor newFactor = new ArrayListTableFactor(new ArrayList<>(newSetOfVariables), newParameters);
		return newFactor;
	}

	private ArrayListTableFactor operateOnUnionedParameters(TableFactor another, ArrayListTableFactor result, BiFunction<Double, Double, Double> operator) {
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
		return sum(parameters).doubleValue();
	}
	
	@Override
	public ArrayListTableFactor normalize() {
		return (ArrayListTableFactor) super.normalize();
	}

	@Override
	protected ArrayListTableFactor normalizeBy(Double normalizationConstant) {
		ArrayListTableFactor result;
		ArrayList<Double> newEntries = new ArrayList<>(getEntries().size());
		for (Double entry : this.parameters) {
			newEntries.add(entry/normalizationConstant);
		}
		result = new ArrayListTableFactor(getVariables(), newEntries);
		return result;

//		int numParameters = parameters.size();
//		for (int i = 0; i < numParameters; ++i) {
//			parameters.set(i, getParameter(i) / normalizationConstant);
//		}
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SUMMING OUT ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ArrayListTableFactor sumOut(List<? extends Variable> variablesToSumOut) {
		return (ArrayListTableFactor) super.sumOut(variablesToSumOut);
	}

	@Override
	protected ArrayListTableFactor sumOutEverythingExcept(List<? extends Variable> variablesToSumOut, ArrayList<? extends TableVariable> variablesNotToSumOut) {

		ArrayListTableFactor result = new ArrayListTableFactor(variablesNotToSumOut, 0.0);
		
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

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INVERSION ////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ArrayListTableFactor invert() {
		ArrayListTableFactor result;
		ArrayList<Double> newEntries = new ArrayList<>(getEntries().size());
		for (Double entry : this.parameters) {
			if (Math.abs(entry) < 0.00000001) {
				throw new Error("Can't invert : 0 value in the table factor.");
			}
			newEntries.add(1/entry);
		}
		result = new ArrayListTableFactor(getVariables(), newEntries);
		return result;
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
	
	private Double getParameter(int parameterIndex) {
		return parameters.get(parameterIndex);
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEXED ACCESS - SETTERS /////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected void setEntryFor(int[] values, Double newParameterValue) {
		int parameterIndex = getParameterIndex(values);
		setParameter(parameterIndex, newParameterValue);
	}
	
	@Override
	public void setEntryFor(ArrayList<Integer> values, Double newParameterValue) {
		int parameterIndex = getParameterIndex(values);
		setParameter(parameterIndex, newParameterValue);
	}
	
	private Double setParameter(int parameterIndex, Double newParameterValue) {
		return parameters.set(parameterIndex, newParameterValue);
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
	// PROBABLY UNNECESSARY, OR AT LEAST UNNECESSARILY PUBLIC, HELPER METHODS ///////////////////////////
	// TODO: revisit and possibly get rid of them                             ///////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	public static Iterator<ArrayList<Integer>> makeCartesianProductIterator(Collection<? extends TableVariable> variables) {
		List<NullaryFunction<Iterator<? extends Integer>>> makersOfIteratorsOverValues = 
				mapIntoList(variables, v -> () -> new IntegerIterator(0, v.getCardinality()));
		
		return new CartesianProductIterator<Integer>(makersOfIteratorsOverValues);
	}
	
	public void reinitializeEntries(Double defaultValue) {
		this.parameters = arrayListFilledWith(defaultValue, parameters.size());
	}
	
	public ArrayList<Double> getEntries() {
		return this.parameters;
	}

}
