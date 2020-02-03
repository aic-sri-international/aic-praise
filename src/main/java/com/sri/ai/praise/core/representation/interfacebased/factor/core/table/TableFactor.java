package com.sri.ai.praise.core.representation.interfacebased.factor.core.table;

import static com.sri.ai.util.Util.allEqual;
import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.mapFromListOfKeysAndListOfValues;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.product;
import static com.sri.ai.util.Util.putAll;
import static com.sri.ai.util.Util.setDifference;
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
 *
 */

public class TableFactor implements Factor {
	
	////////////////////////////////////////////////////////////////////////////////////////////////////
	// DATA MEMBERS ////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////
	
	private String name;
	private final ArrayList<TableVariable> variables;
	private ArrayList<Double> parameters;
	private final MixedRadixNumber parameterIndexRadix;
	private ExplanationTree explanation = DefaultExplanationTree.PLACEHOLDER;	// use currently not supported
	
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
	
	
	
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTORS /////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@SuppressWarnings("unchecked")
	public TableFactor(String factorName, Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {

		this.name = factorName;
		
		if (variables instanceof ArrayList<?>) {
			this.variables = (ArrayList<TableVariable>) variables;
		}
		else {
			this.variables = new ArrayList<TableVariable>(variables);
		}

		this.parameters = parameters;
		this.parameterIndexRadix = createMixedRadixNumberForIndexingFactorParameters();
	}
	
	public TableFactor(Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {
		this("phi", variables, parameters);
	}
	
	public TableFactor(Collection<? extends TableVariable> variables, Double defaultValue) {
		this(variables, arrayListFilledWith(defaultValue, numberOfTableEntries(variables)));
	}
	
	public TableFactor(Collection<? extends TableVariable> variables) {
		this(variables, -1.0);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////

	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// PUBLIC METHODS ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// STATIC METHODS ///////////////////////////////////////////////////////////////////////////////////
	
	public static int numberOfTableEntries(Collection<? extends TableVariable> variables) {
		return product(functionIterator(variables, TableVariable::getCardinality)).intValue();
	}
	
	public static Iterator<ArrayList<Integer>> makeCartesianProductIterator(Collection<? extends TableVariable> variables) {
		List<NullaryFunction<Iterator<? extends Integer>>> makersOfIteratorsOverValues = 
				mapIntoList(variables, v -> () -> new IntegerIterator(0, v.getCardinality()));
		
		return new CartesianProductIterator<Integer>(makersOfIteratorsOverValues);
	}
	
	// TODO: perhaps make this a non-static method.
	/**
	 * Makes a new factor representing a slice of the original factor.
	 * 
	 * @param factor (factor to slice on)
	 * @param variables (variables to slice on)
	 * @param values (values of the above-mentioned variables to slice by)
	 * @return sub-factor produced from slicing the passed variables at their given values
	 */
	public static TableFactor slice(TableFactor factor, List<TableVariable> variables, List<Integer> values) {
		Map<TableVariable, Integer> assignment = mapFromListOfKeysAndListOfValues(variables, values);
		TableFactor result = slicePossiblyModifyingAssignment(factor, assignment);
		return result;
	}
	
	public static TableFactor slice(TableFactor factor, Map<TableVariable, Integer> assignment) {
		Map<TableVariable, Integer> assignmentCopy = new LinkedHashMap<>(assignment);
		TableFactor result = slicePossiblyModifyingAssignment(factor, assignmentCopy);
		return result;
	}
	
	private static TableFactor slicePossiblyModifyingAssignment(TableFactor factor, Map<TableVariable, Integer> assignment) {

		ArrayList<TableVariable> remainingVariables = new ArrayList<>(factor.getVariables());
		
		remainingVariables.removeAll(assignment.keySet());
		
		Iterator<ArrayList<Integer>> assignmentsToRemainingVariables = makeCartesianProductIterator(remainingVariables);
		TableFactor result = new TableFactor(remainingVariables);
		for (ArrayList<Integer> remainingVariablesValues: in(assignmentsToRemainingVariables)) {
			putAll(assignment, remainingVariables, remainingVariablesValues);
			var value = factor.getEntryFor(assignment);
			result.setEntryFor(assignment, value);
		}
		
		return result;
	}


	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	// METHODS BASED ON IMPLEMENTING FACTOR /////////////////////////////////////////////////////////////
	
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
		if (parameters.size() == 0) {
			return true;	
		}
		if (parameters.get(0) == 0) {
			return false;	
		}
		return allEqual(parameters);
	}
	
	/**
	 * Normalizes factor so that the overall sum of all parameters together = 1.0
	 * 
	 * @return reference to normalized self
	 */
	@Override
	public TableFactor normalize() {
		Double normalizationConstant = sumOfParameters();
		if (normalizationConstant != 0.0 && normalizationConstant != 1.0) {
			normalizeBy(normalizationConstant);
		}
		return this ;
	}
	
	private void normalizeBy(Double normalizationConstant) {
		int numParameters = parameters.size();
		for (int i = 0; i < numParameters; ++i) {
			parameters.set(i, parameters.get(i) / normalizationConstant);
		}
	}

	public TableFactor normalizedCopy() {
		Double normalizationConstant = sumOfParameters();
		TableFactor normalizedTableFactor = this;
		if (normalizationConstant != 0.0 && normalizationConstant != 1.0) {
			ArrayList<Double> newParameters = new ArrayList<>(parameters.size());
			for (Double unnormalizedParameter : parameters) {
				newParameters.add(unnormalizedParameter/normalizationConstant);
			}
			normalizedTableFactor = new TableFactor(variables, newParameters);
		}
		return normalizedTableFactor;
	}

	/**
	 * Returns parameter corresponding to a given assignments
	 * 
	 * @param assignment (variables and their values)
	 * @return parameter corresponding the given variable assignments
	 */
	@Override
	public Double getEntryFor(Map<? extends Variable, ?> assignment) {
		int[] variableValues = variableValuesInFactorVariableOrder(assignment);
		Double result = getEntryFor(variableValues);
		return result;
	}
	
	public <T extends Variable, U> Double getEntryFor(List<T> variables, List<U> values) {
		Map<T, U> assignment = mapFromListOfKeysAndListOfValues(variables, values);
		return getEntryFor(assignment);
	}
	
	public Double getEntryFor(ArrayList<Integer> variableValuesInTheRightOrder) {
		int parameterIndex = getParameterIndex(variableValuesInTheRightOrder);
		return parameters.get(parameterIndex);
	}
	
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
			result = new ConstantFactor(sumOfParameters());
		}
		else {
			result = sumOutEverythingExcept(variablesNotToSumOut);
		}
		
		return result;
	}
	
	
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
		else
		{
			result = multiply((TableFactor)another);
		}
		
		return result;
	}
	
	
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
		else
		{
			result = add((TableFactor) another);
		}
		
		return result;
		
	}

	@Override
	public boolean isZero() {
		return false;
	}

	@Override
	public Factor invert() {
		TableFactor result;
		ArrayList<Double> newEntries = new ArrayList<>(getEntries().size());
		for (Double entry : getEntries()) {
			if (Math.abs(entry) < 0.00000001) {
				throw new Error("Can't invert : 0 value in the table factor.");
			}
			newEntries.add(1/entry);
		}
		result = new TableFactor(getVariables(), newEntries);
		return result;
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		// TODO Auto-generated method stub
		return null;
	}
	

	@Override
	public ExplanationTree getExplanation() {
		return explanation;
	}
	
	
	@Override
	public void setExplanation(ExplanationTree explanation) {
		this.explanation = explanation;
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
	
	
	
	// TABLEFACTOR-SPECIFIC METHODS /////////////////////////////////////////////////////////////////////
	
	@Override
	public String toString() {
		
		String factorAsString = name + variables.toString() + ": " + parameters.toString();
		
		return factorAsString;
	}
	
	
	public void reinitializeEntries(Double defaultValue) {
		this.parameters = arrayListFilledWith(defaultValue, parameters.size());
	}
	
	
	public TableFactor multiply(TableFactor another) {
		
		TableFactor result = initializeNewFactorUnioningVariables(another);
		result = operateOnUnionedParameters(another, result, (a,b) -> a * b);
		
		return result;
	}
	
	
	public TableFactor add(TableFactor another) {
		
		TableFactor result = initializeNewFactorUnioningVariables(another);
		result = operateOnUnionedParameters(another, result, (a,b) -> a + b);
		
		return result;
	}
	
	
	public void setEntryFor(Map<? extends Variable, ? extends Integer> assignment, Double newParameterValue) {
		int parameterIndex = getParameterIndex(assignment);
		parameters.set(parameterIndex, newParameterValue);
	}
	
	public <T extends Variable> void setEntryFor(List<T> variableList, List<Integer> variableValues, Double newParameterValue) {
		Map<T, Integer> variablesAndTheirValues = mapFromListOfKeysAndListOfValues(variableList, variableValues);
		int parameterIndex = getParameterIndex(variablesAndTheirValues);
		parameters.set(parameterIndex, newParameterValue);
	}
	
	public void setEntryFor(ArrayList<Integer> variableValuesInTheRightOrder, Double newParameterValue) {
		int parameterIndex = getParameterIndex(variableValuesInTheRightOrder);
		parameters.set(parameterIndex, newParameterValue);
	}
	
	
	public void setName(String newName) {
		this.name = newName;
	}
	
	
	public ArrayList<Double> getEntries() {
		return this.parameters;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// PRIVATE METHODS //////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// HELPER METHODS ///////////////////////////////////////////////////////////////////////////////////
	
	private MixedRadixNumber createMixedRadixNumberForIndexingFactorParameters()
	{
		ArrayList<Integer> listOfVariableCardinalities = mapIntoArrayList(variables, v->v.getCardinality());
		
		return new MixedRadixNumber(BigInteger.ZERO, listOfVariableCardinalities);
	}
	
	
	private Double sumOfParameters()
	{
		Double sumOfParameters = 0.;
		for(Double p : this.parameters) {
			sumOfParameters = sumOfParameters + p;
		}
		return sumOfParameters;
	}
	
	
	private TableFactor sumOutEverythingExcept(LinkedHashSet<TableVariable> variablesNotToSumOut) {

		TableFactor result = new TableFactor(variablesNotToSumOut, 0.0);
		
		LinkedHashMap<Variable, Integer> assignment = new LinkedHashMap<>();
		for(ArrayList<Integer> values: in(makeCartesianProductIterator(variables))) {
			assignment = addtoVariableValueMap(assignment, values);
			Double currentValue = result.getEntryFor(assignment);
			Double addedValue = getEntryFor(assignment);
			result.setEntryFor(assignment, currentValue + addedValue);
		}
		return result;
	}
	
	private LinkedHashMap<Variable,Integer> addtoVariableValueMap(LinkedHashMap<Variable, Integer> assignment, ArrayList<Integer> values) {
		for (int i = 0; i < variables.size(); ++i) {
			assignment.put(variables.get(i), values.get(i));
		}
		return assignment;
	}

	private TableFactor initializeNewFactorUnioningVariables(TableFactor another) {
		LinkedHashSet<TableVariable> newSetOfVariables = new LinkedHashSet<>(this.variables);
		newSetOfVariables.addAll(another.variables);
		Integer numberOfParametersForNewListOfVariables = numberOfTableEntries(newSetOfVariables);
		ArrayList<Double> newParameters = arrayListFilledWith(-1.0, numberOfParametersForNewListOfVariables);	
		TableFactor newFactor = new TableFactor(new ArrayList<>(newSetOfVariables), newParameters);
		return newFactor;
	}
	

	/**
	 * Returns the indices in this.parameters holding the parameter corresponding to the input variables assignments
	 * <p>
	 * First, the input Map<Variable,VariableAssignment> is converted into an int[] such that each element's index 
	 * corresponds to the variables in VariableList and the element values corresponding to the variable assignments.  
	 * This array is then passed to another overload of this function to obtain the return value.
	 * 
	 * @param variableValueMap (a map from a variable to its assigned value)
	 * @return index position in this.parameters of the parameter corresponding to the variable assignments provided
	 */
	private int getParameterIndex(Map<? extends Variable, ? extends Integer> variableValueMap) {
		int[] varValues = variableValuesInFactorVariableOrder(variableValueMap);
		
		int parameterIndex = getParameterIndex(varValues);
		return parameterIndex;
	}
	
	
	/**
	 * Returns the stored parameter corresponding to the input variable assignments
	 * 
	 * @param varValues (array with indices corresponding to variables, and entries
	 * 					 corresponding to the variable assignments)
	 * @return parameter corresponding to the input variable assignments
	 */
	private Double getEntryFor(int[] varValues) {
		int parameterIndex = getParameterIndex(varValues);
		return parameters.get(parameterIndex);
	}
	
	
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
	
	
	/**
	 * Returns the index in this.parameters holding the parameter corresponding to the variable assignments input
	 * 
	 * @param variableValuesInTheRightOrder (List with element positions corresponding to variables, and entries
	 * 										 corresponding to the variable assignments)
	 * @return index position in this.parameters of the parameter corresponding to the variable assignments provided
	 */
	private int getParameterIndex(ArrayList<Integer> variableValuesInTheRightOrder) {
		int[] variableValuesArray = Ints.toArray(variableValuesInTheRightOrder);
		int parameterIndex = getParameterIndex(variableValuesArray);
		return parameterIndex;
	}
	
	
	/**
	 * Returns the index in this.entries holding the parameter corresponding to the variable assignments input
	 * 
	 * @param variableValues (array with indices corresponding to variables, and entries
	 * 						  corresponding to the variable assignments)
	 * @return index position in this.entries of the parameter corresponding to the variable assignments provided
	 */
	private int getParameterIndex(int[] variableValues) {
		int parameterIndex = parameterIndexRadix.getValueFor(variableValues).intValue();
		return parameterIndex;
	}
	

	private TableFactor operateOnUnionedParameters(TableFactor another, TableFactor result, BiFunction<Double, Double, Double> operator) {
		Iterator<ArrayList<Integer>> cartesianProduct = makeCartesianProductIterator(result.variables);
		LinkedHashMap<Variable, Integer> variableValueMap = new LinkedHashMap<>();
		for(ArrayList<Integer> values: in(cartesianProduct)) {
			variableValueMap = result.addtoVariableValueMap(variableValueMap, values);
			Double product = operator.apply(this.getEntryFor(variableValueMap), another.getEntryFor(variableValueMap));
			result.setEntryFor(variableValueMap, product);
		}
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////

}
