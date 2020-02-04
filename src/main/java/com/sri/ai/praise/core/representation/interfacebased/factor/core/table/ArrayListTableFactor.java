package com.sri.ai.praise.core.representation.interfacebased.factor.core.table;

import static com.sri.ai.util.Util.allEqual;
import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.mapFromListOfKeysAndListOfValues;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.product;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.Util.sum;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ConstantFactor;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.IntegerIterator;
import com.sri.ai.util.explanation.tree.DefaultExplanationTree;
import com.sri.ai.util.explanation.tree.ExplanationTree;
import com.sri.ai.util.math.MixedRadixNumber;

/**
 * Discrete table implementation of {@link Factor}.
 * 
 * @author gabriel
 * @author bobak
 * @author braz
 *
 */

public class ArrayListTableFactor implements TableFactor {
	
	////////////////////////////////////////////////////////////////////////////////////////////////////
	// DATA MEMBERS ////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private String name;
	private final ArrayList<TableVariable> variables;
	private ArrayList<Double> parameters;
	private final MixedRadixNumber parameterIndexRadix;

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
	
	@SuppressWarnings("unchecked")
	public ArrayListTableFactor(String factorName, Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {

		this.name = factorName;
		
		if (variables instanceof ArrayList) {
			this.variables = (ArrayList<TableVariable>) variables;
		}
		else {
			this.variables = new ArrayList<TableVariable>(variables);
		}

		// NDARRAY
		this.parameters = parameters;
		this.parameterIndexRadix = ABS_createMixedRadixNumberForIndexingFactorParameters();
	}
	
	public ArrayListTableFactor(Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {
		this("phi", variables, parameters);
	}
	
	public ArrayListTableFactor(Collection<? extends TableVariable> variables, Double defaultValue) {
		this(variables, arrayListFilledWith(defaultValue, STAY_numberOfTableEntries(variables)));
	}
	
	public ArrayListTableFactor(Collection<? extends TableVariable> variables) {
		this(variables, -1.0);
	}

	private MixedRadixNumber ABS_createMixedRadixNumberForIndexingFactorParameters() { // NDARRAY
		ArrayList<Integer> cardinalities = mapIntoArrayList(variables, v->v.getCardinality());
		return new MixedRadixNumber(BigInteger.ZERO, cardinalities);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// BASIC METHODS ////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public void setName(String newName) {
		this.name = newName;
	}
	
	@Override
	public boolean contains(Variable variable) {
		return variables.contains(variable);
	}
	
	@Override
	public ArrayList<TableVariable> getVariables() {
		return variables;
	}

	@Override
	public boolean isIdentity() {
		if (ABS_thereAreZeroParameters()) {
			return true;	
		}
		if (ABS_firstParameterIsZero()) {
			return false;	
		}
		return ABS_parametersAreAllEqual();
	}

	@Override
	public boolean isZero() {
		return false;
	}

	@Override
	public String toString() {
		return name + variables.toString() + ": " + ABS_parametersString();
	}

	private boolean ABS_firstParameterIsZero() { // NDARRAY
		return parameters.get(0) == 0;
	}

	private boolean ABS_thereAreZeroParameters() { // NDARRAY
		return parameters.size() == 0;
	}
	
	private boolean ABS_parametersAreAllEqual() { // NDARRAY
		return allEqual(parameters);
	}

	private String ABS_parametersString() { // NDARRAY
		return parameters.toString();
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SLICING //////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// TODO: perhaps make this a non-static method.
	/**
	 * Makes a new factor representing a slice of the original factor.
	 * 
	 * @param factor (factor to slice on)
	 * @param variables (variables to slice on)
	 * @param values (values of the above-mentioned variables to slice by)
	 * @return sub-factor produced from slicing the passed variables at their given values
	 */
	@Override
	public ArrayListTableFactor slice(List<TableVariable> variables, List<Integer> values) {
		var assignment = mapFromListOfKeysAndListOfValues(variables, values);
		ArrayListTableFactor result = slicePossiblyModifyingAssignment(assignment);
		return result;
	}
	
	@Override
	public ArrayListTableFactor slice(Map<TableVariable, Integer> assignment) {
		var assignmentCopy = new LinkedHashMap<>(assignment);
		ArrayListTableFactor result = slicePossiblyModifyingAssignment(assignmentCopy);
		return result;
	}
	
	private ArrayListTableFactor slicePossiblyModifyingAssignment(Map<TableVariable, Integer> assignment) {
		var remainingVariables = new ArrayList<>(getVariables());
		remainingVariables.removeAll(assignment.keySet());
		ArrayListTableFactor result = ABS_slicePossiblyModifyingAssignment(assignment, remainingVariables);
		return result;
	}

	private ArrayListTableFactor ABS_slicePossiblyModifyingAssignment(
			Map<TableVariable, Integer> assignment,
			ArrayList<TableVariable> remainingVariables) { // NDARRAY
		
		ArrayListTableFactor result = new ArrayListTableFactor(remainingVariables);
		Iterator<ArrayList<Integer>> assignmentsToRemainingVariables = STAY_makeCartesianProductIterator(remainingVariables);
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
	public Factor add(Factor another) {
		Factor result;
		if (another instanceof ConstantFactor) {
			result = another.add(this);
		}		
		else if (another.getClass() != this.getClass()) {
			throw new Error("Trying to multiply different types of factors: this is a " +
						this.getClass() + "and another is a " + another.getClass());
		}
		else {
			result = ABS_addAbstractTableFactor((ArrayListTableFactor) another);
		}
		return result;
	}

	protected ArrayListTableFactor ABS_addAbstractTableFactor(ArrayListTableFactor another) { // NDARRAY
		ArrayListTableFactor result = STAY_initializeNewFactorUnioningVariables(another);
		result = STAY_operateOnUnionedParameters(another, result, (a,b) -> a + b);
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// MULTIPLICATION ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Multiplies current TableFactor to another passed in TableFactor
	 * 
	 * @param another (the other TableFactor to be multiplied to)
	 * @return a Factor reference to the TableFactor product of the multiplication
	 */
	@Override
	public Factor multiply(Factor another) {

		Factor result;
		
		if (another instanceof ConstantFactor) {
			result = another.multiply(this);
		}
		else if (another.getClass() != this.getClass()) {
			throw new Error("Trying to multiply different types of factors: this is a " +
							this.getClass() + "and another is a " + another.getClass());
		}
		else {
			result = ABS_multiplyTableFactor((ArrayListTableFactor) another);
		}
		
		return result;
	}

	private ArrayListTableFactor ABS_multiplyTableFactor(ArrayListTableFactor another) { // NDARRAY
		ArrayListTableFactor result = STAY_initializeNewFactorUnioningVariables(another);
		result = STAY_operateOnUnionedParameters(another, result, (a,b) -> a * b);
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SHARED SUPPORT FOR ADDING AND MULTIPLYING ////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	 // NDARRAY - TableFactor only
	
	private ArrayListTableFactor STAY_initializeNewFactorUnioningVariables(ArrayListTableFactor another) {
		LinkedHashSet<TableVariable> newSetOfVariables = new LinkedHashSet<>(this.variables);
		newSetOfVariables.addAll(another.variables);
		Integer numberOfParametersForNewListOfVariables = STAY_numberOfTableEntries(newSetOfVariables);
		ArrayList<Double> newParameters = arrayListFilledWith(-1.0, numberOfParametersForNewListOfVariables);	
		ArrayListTableFactor newFactor = new ArrayListTableFactor(new ArrayList<>(newSetOfVariables), newParameters);
		return newFactor;
	}

	private ArrayListTableFactor STAY_operateOnUnionedParameters(TableFactor another, ArrayListTableFactor result, BiFunction<Double, Double, Double> operator) {
		Iterator<ArrayList<Integer>> cartesianProduct = STAY_makeCartesianProductIterator(result.variables);
		LinkedHashMap<Variable, Integer> variableValueMap = new LinkedHashMap<>();
		for(ArrayList<Integer> values: in(cartesianProduct)) {
			variableValueMap = result.STAY_putAll(variableValueMap, values);
			Double product = operator.apply(this.getEntryFor(variableValueMap), another.getEntryFor(variableValueMap));
			result.setEntryFor(variableValueMap, product);
		}
		return result;
	}
	
	private static int STAY_numberOfTableEntries(Collection<? extends TableVariable> variables) {
		return product(functionIterator(variables, TableVariable::getCardinality)).intValue();
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// NORMALIZATION ////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Normalizes factor so that the overall sum of all parameters together = 1.0
	 * 
	 * @return reference to normalized self
	 */
	@Override
	public ArrayListTableFactor normalize() {
		Double normalizationConstant = ABS_computeNormalizationConstant();
		if (normalizationConstant != 0.0 && normalizationConstant != 1.0) {
			ABS_normalizeBy(normalizationConstant);
		}
		return this ;
	}

	private double ABS_computeNormalizationConstant() { // NDARRAY
		return sum(parameters).doubleValue();
	}
	
	private void ABS_normalizeBy(Double normalizationConstant) { // NDARRAY
		int numParameters = parameters.size();
		for (int i = 0; i < numParameters; ++i) {
			parameters.set(i, STAY_getParameter(i) / normalizationConstant);
		}
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SUMMING OUT ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Sums out given variables from factor.
	 * 
	 * @param variablesToSumOut (variables to sum out)
	 * @return new factor with given variables summed out
	 */
	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOutList) {
		
		//TODO: Error check for if variablesToSumOut is of type List<? extends TableVariable>
		//TODO: Error check for if a variable listed to sum out exists in the factor
		
		@SuppressWarnings("unchecked")
		LinkedHashSet<TableVariable> variablesToSumOut = new LinkedHashSet<>((List<TableVariable>) variablesToSumOutList);
		
		LinkedHashSet<TableVariable> variablesNotToSumOut = new LinkedHashSet<>();
		variablesNotToSumOut = (LinkedHashSet<TableVariable>) setDifference(variables, variablesToSumOut, variablesNotToSumOut);
		
		Factor result;
		// if every variable is summed out, return the sum of all the parameters in a constant factor
		if (variablesNotToSumOut.isEmpty()) {
			result = new ConstantFactor(ABS_computeNormalizationConstant());
		}
		else {
			result = ABS_sumOutEverythingExcept(variablesNotToSumOut);
		}
		
		return result;
	}

	private ArrayListTableFactor ABS_sumOutEverythingExcept(LinkedHashSet<TableVariable> variablesNotToSumOut) { // NDARRAY

		ArrayListTableFactor result = new ArrayListTableFactor(variablesNotToSumOut, 0.0);
		
		LinkedHashMap<Variable, Integer> assignment = new LinkedHashMap<>();
		for (ArrayList<Integer> values: in(STAY_makeCartesianProductIterator(variables))) {
			assignment = STAY_putAll(assignment, values);
			Double currentValue = result.getEntryFor(assignment);
			Double addedValue = getEntryFor(assignment);
			result.setEntryFor(assignment, currentValue + addedValue);
		}
		return result;
	}
	
	 // NDARRAY - TableFactor only
	private LinkedHashMap<Variable,Integer> STAY_putAll(LinkedHashMap<Variable, Integer> assignment, ArrayList<Integer> values) {
		for (int i = 0; i < variables.size(); i++) {
			assignment.put(variables.get(i), values.get(i));
		}
		return assignment;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INVERSION ////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public Factor ABS_invert() { // NDARRAY
		ArrayListTableFactor result;
		ArrayList<Double> newEntries = new ArrayList<>(STAY_getEntries().size());
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
	public Double getEntryFor(Map<? extends Variable, ?> assignment) {
		int[] values = variableValuesInFactorVariableOrder(assignment);
		Double result = ABS_getEntryFor(values);
		return result;
	}
	
	private Double ABS_getEntryFor(int[] values) { // NDARRAY
		int parameterIndex = STAY_getParameterIndex(values);
		return STAY_getParameter(parameterIndex);
	}

	@Override
	public Double ABS_getEntryFor(ArrayList<Integer> values) { // NDARRAY
		int parameterIndex = STAY_getParameterIndex(values);
		return STAY_getParameter(parameterIndex);
	}
	
	private Double STAY_getParameter(int parameterIndex) { // NDARRAY - TableFactor only
		return parameters.get(parameterIndex);
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEXED ACCESS - SETTERS /////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public void setEntryFor(Map<? extends Variable, ? extends Integer> assignment, Double newParameterValue) {
		int[] values = variableValuesInFactorVariableOrder(assignment);
		ABS_setEntryFor(values, newParameterValue);
	}

	private void ABS_setEntryFor(int[] values, Double newParameterValue) { // NDARRAY
		int parameterIndex = STAY_getParameterIndex(values);
		STAY_setParameter(parameterIndex, newParameterValue);
	}
	
	@Override
	public void ABS_setEntryFor(ArrayList<Integer> values, Double newParameterValue) { // NDARRAY
		int parameterIndex = STAY_getParameterIndex(values);
		STAY_setParameter(parameterIndex, newParameterValue);
	}
	
	private Double STAY_setParameter(int parameterIndex, Double newParameterValue) { // NDARRAY - TableFactor only
		return parameters.set(parameterIndex, newParameterValue);
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// ASSIGNMENT UTILITIES /////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private int[] variableValuesInFactorVariableOrder(Map<? extends Variable, ?> assignment) {
		//TODO: error checking
		//TODO: there is no mechanism for handling partial variable assignments
		int numVariables = variables.size();
		int[] indexOfVariablesValues = new int[numVariables];
		for(int i = 0; i < numVariables; ++i) {
			TableVariable variable = variables.get(i);
			indexOfVariablesValues[i] = (Integer) assignment.get(variable);
		}
		return indexOfVariablesValues;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEX UTILITIES //////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// NDARRAY - TableFactor only
	
	private int STAY_getParameterIndex(ArrayList<Integer> values) {
		int[] variableValuesArray = Ints.toArray(values);
		int parameterIndex = STAY_getParameterIndex(variableValuesArray);
		return parameterIndex;
	}
	
	private int STAY_getParameterIndex(int[] values) {
		return parameterIndexRadix.getValueFor(values).intValue();
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// UNIMPLEMENTED AGGREGATIONS ///////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		// TODO Auto-generated method stub
		return null;
	}
	

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
		// TODO Auto-generated method stub
		return null;
	}
	

	@Override
	public Factor min(Collection<? extends Variable> variablesToMinimize) {
		// TODO Auto-generated method stub
		return null;
	}
	

	@Override
	public Factor argmin(Collection<? extends Variable> variablesToMinimize) {
		// TODO Auto-generated method stub
		return null;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// UNIMPLEMENTED EXPLANATIONS ///////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private ExplanationTree explanation = DefaultExplanationTree.PLACEHOLDER;	// use currently not supported
	
	@Override
	public ExplanationTree getExplanation() {
		return explanation;
	}
	
	@Override
	public void setExplanation(ExplanationTree explanation) {
		this.explanation = explanation;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// PROBABLY UNNECESSARY, OR AT LEAST UNNECESSARILY PUBLIC, HELPER METHODS ///////////////////////////
	// TODO: revisit and possibly get rid of them                             ///////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	// NDARRAY - TableFactor only
	
	public static Iterator<ArrayList<Integer>> STAY_makeCartesianProductIterator(Collection<? extends TableVariable> variables) {
		List<NullaryFunction<Iterator<? extends Integer>>> makersOfIteratorsOverValues = 
				mapIntoList(variables, v -> () -> new IntegerIterator(0, v.getCardinality()));
		
		return new CartesianProductIterator<Integer>(makersOfIteratorsOverValues);
	}
	
	public void STAY_reinitializeEntries(Double defaultValue) {
		this.parameters = arrayListFilledWith(defaultValue, parameters.size());
	}
	
	public ArrayList<Double> STAY_getEntries() {
		return this.parameters;
	}

}
