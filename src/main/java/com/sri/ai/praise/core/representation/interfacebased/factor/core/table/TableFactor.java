package com.sri.ai.praise.core.representation.interfacebased.factor.core.table;

import static com.sri.ai.util.Util.arrayListFilledWith;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapFromListOfKeysAndListOfValues;
import static com.sri.ai.util.Util.setDifference;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.ConstantFactor;
import com.sri.ai.util.DefaultExplanationTree;
import com.sri.ai.util.ExplanationTree;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.math.MixedRadixNumber;

/**
 * Data type representing a graph factor.
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
	private final ArrayList<TableVariable> variableList;
	private final LinkedHashSet<TableVariable> variableSet;
	private ArrayList<Double> parameters;
	private final MixedRadixNumber parameterIndexRadix;
	private ExplanationTree explanation = DefaultExplanationTree.PLACEHOLDER;	// use currently not supported
	
	/*  NOTE:  Understanding the Parameter Order
	 * 
	 * ex:  consider a factor with three binary variables v1, v2, and v3 in that same order
	 * 
	 * parameters will be arranged based on the following variable assignment order:
	 * 
	 * 		[ (v1=0,v2=0,v3=0), (v1=0,v2=0,v3=1), (v1=0,v2=1,v3=0), (v1=0,v2=1,v3=1), 
	 * 		  (v1=1,v2=0,v3=0), (v1=1,v2=0,v3=1), (v1=1,v2=1,v3=0), (v1=1,v2=1,v3=1) ]
	 * 
	 * Note that the order would change if the order the variables are stored as is changed.
	 * 
	 * parameterIndexRadix is responsible for mapping the variable assignments to the correct parameter index.
	 */
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTORS /////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public TableFactor(String factorName, Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {

		this.name = factorName;
		
		if(variables instanceof LinkedHashSet<?>)
		{
			this.variableSet = (LinkedHashSet<TableVariable>) variables;
		}
		else
		{
			this.variableSet = new LinkedHashSet<TableVariable>(variables);
		}
		
		if(variables instanceof ArrayList<?>)
		{
			this.variableList = (ArrayList<TableVariable>) variables;
		}
		else
		{
			this.variableList = new ArrayList<TableVariable>(variables);
		}

		this.parameters = parameters;
		this.parameterIndexRadix = createMixedRadixNumberForIndexingFactorParameters();
	}
	
	public TableFactor(Collection<? extends TableVariable> variables, ArrayList<Double> parameters) {

		this("phi",variables,parameters);
	}
	
	public TableFactor(Collection<? extends TableVariable> variables, Double defaultValue) {
		this(variables, arrayListFilledWith(defaultValue, numEntries(variables)));
	}
	
	public TableFactor(Collection<? extends TableVariable> variables) {
		this(variables, -1.);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// PUBLIC METHODS ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	// STATIC METHODS ///////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Calculates the number of parameters are needed for a factor with scope based on the passed variables
	 * 
	 * @param variableList
	 * @return number of parameters a factor of given scope needs
	 */
	public static int numEntries(Collection<? extends TableVariable> variableList) {
		int result = 1;
		for(TableVariable v : variableList) {
			result *= v.getCardinality();
		}
		return result;
	}
	
	
	//TODO:  finish description
	/**
	 *
	 * 
	 * @param listOfVariables
	 * @return
	 */
	public static Iterator<ArrayList<Integer>> getCartesianProduct(Collection<TableVariable> listOfVariables) {
		
		ArrayList<ArrayList<Integer>> listOfValuesForTheVariables = mapIntoArrayList(listOfVariables, 
																	v -> makeArrayWithValuesFromZeroToCardinalityMinusOne(v.getCardinality()));
		ArrayList<NullaryFunction<Iterator<Integer>>> iteratorForListOfVariableValues = 
				mapIntoArrayList(listOfValuesForTheVariables, element -> () -> element.iterator());
		
		Iterator<ArrayList<Integer>> cartesianProduct = new CartesianProductIterator<Integer>(iteratorForListOfVariableValues);
		return cartesianProduct;
	}
	
	
	//TODO:  I suggest refactoring copyToSubTableFactor() to getSubFactor() or sliceFactorAt() across project
	/**
	 * Slices the factor along variable values provided, returning the sub-factor produced by the slicing
	 * 
	 * @param factor (factor to slice on)
	 * @param variablesPredetermined (variables to slice on)
	 * @param valuesPredetermined (values of the above-mentioned variables to slice by)
	 * @return sub-factor produced from slicing the passed variables at their given values
	 */
	public static TableFactor copyToSubTableFactor(TableFactor factor,
			List<TableVariable> variablesPredetermined, List<Integer> valuesPredetermined) {
		
		Map<TableVariable, Integer> mapOfvaluesPredetermined = mapFromListOfKeysAndListOfValues(variablesPredetermined, valuesPredetermined);
		TableFactor result = copyToSubTableFactorWithoutRecreatingANewMap(factor, mapOfvaluesPredetermined);
		return result;
	}
	
	public static TableFactor copyToSubTableFactor(TableFactor factor,
			Map<TableVariable, Integer> mapOfvaluesPredetermined) {

		Map<TableVariable, Integer> map2= new LinkedHashMap<>(mapOfvaluesPredetermined);
		TableFactor result = copyToSubTableFactorWithoutRecreatingANewMap(factor, map2);
		
		return result;
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	// METHODS BASED ON IMPLEMENTING FACTOR /////////////////////////////////////////////////////////////
	
	@Override
	public boolean contains(Variable variable) {
		boolean res = variableSet.contains(variable);
		return res;
	}
	
	
	@Override
	public ArrayList<TableVariable> getVariables() {
		return variableList;
	}
	

	//TODO:  Check correctness of isIdentity() function
	@Override
	public boolean isIdentity() {
		if(parameters.size() == 0 || parameters.get(0) == 0) {
			return false;	
		}
		double valueAtZero = parameters.get(0);
		for(Double v : parameters) {
			if (v != valueAtZero) {
				return false;
			}
		}
		return true;
	}
	

	/**
	 * Normalizes factor so that the overall sum of all parameters together = 1.0
	 * 
	 * @return reference to normalized self
	 */
	@Override
	public TableFactor normalize() {
		
		Double normalizationConstant = sumOfParameters();
		if(normalizationConstant != 0.0 && normalizationConstant != 1.0) {
			this.normalizeBy(normalizationConstant);
		}
		return this ;
	}
	
	
	/**
	 * Returns parameter corresponding to given variable-assignments
	 * 
	 * @param variablesAndTheirValues (variables and their value assignments)
	 * @return parameter corresponding the given variable assignments
	 */
	@Override
	public Double getEntryFor(Map<? extends Variable, ? extends Object> variablesAndTheirValues) {
		int[] variableValues = variableValuesInFactorVariableOrder(variablesAndTheirValues);
		Double result = getEntryFor(variableValues);
		return result;
	}
	
	public <T extends Variable, U extends Object> Double getEntryFor(List<T> variableList, List<U> variableValues) {
		Map<T, U> variablesAndTheirValues = mapFromListOfKeysAndListOfValues(variableList, variableValues);
		return getEntryFor(variablesAndTheirValues);
	}
	
	public Double getEntryFor(List<? extends Object> variableValuesInTheRightOrder) {
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
		
		//TODO: Error check for if variablesToSumOut is of type List<? extends TableVarable>
		//TODO: Error check for if a variable listed to SumOut exists in the factor
		
		LinkedHashSet<TableVariable> variablesToSumOut = new LinkedHashSet<>((List<TableVariable>) variablesToSumOutList);
		
		LinkedHashSet<TableVariable> variablesNotToSumOut = new LinkedHashSet<>();
		variablesNotToSumOut = (LinkedHashSet<TableVariable>) setDifference(this.variableSet, variablesToSumOut, variablesNotToSumOut);
		
		Factor result;
		// if every variable is summed out, return the sum of all the parameters in a constant factor
		if(variablesNotToSumOut.isEmpty()) {
			result = new ConstantFactor(sumOfParameters());
		}
		else
		{
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
		
		if(another instanceof ConstantFactor) {
			result = another.multiply(this);
		}
		else if(another.getClass() != this.getClass()) {
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
		
		if(another instanceof ConstantFactor) {
			result = another.add(this);
		}		
		else if(another.getClass() != this.getClass()) {
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
			if(Math.abs(entry) < 0.00000001) {
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
		
		String factorAsString = name + variableSet.toString() + ": " + parameters.toString();
		
		return factorAsString;
	}
	
	
	public void reinitializeEntries(Double defaultValue) {
		this.parameters = arrayListFilledWith(defaultValue, parameters.size());
	}
	
	
	public TableFactor normalizedCopy() {
		
		Double normalizationConstant = sumOfParameters();
		TableFactor normalizedTableFactor = this;
		if(normalizationConstant != 0.0 && normalizationConstant != 1.0) {
			ArrayList<Double> newParameters = new ArrayList<>(parameters.size());
			for (Double unnormalizedParameter : parameters) {
				newParameters.add(unnormalizedParameter/normalizationConstant);
			}
			normalizedTableFactor = new TableFactor(variableSet, newParameters);
		}
		return normalizedTableFactor;
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
	
	
	public void setEntryFor(Map<? extends Variable, ? extends Object> variablesAndTheirValues, Double newParameterValue) {
		int parameterIndex = getParameterIndex(variablesAndTheirValues);
		parameters.set(parameterIndex, newParameterValue);
	}
	
	public <T extends Variable, U extends Object> void setEntryFor(List<T> variableList, List<U> variableValues, Double newParameterValue) {
		Map<T, U> variablesAndTheirValues = mapFromListOfKeysAndListOfValues(variableList, variableValues);
		int parameterIndex = getParameterIndex(variablesAndTheirValues);
		parameters.set(parameterIndex, newParameterValue);
	}
	
	public void setEntryFor(List<? extends Integer> variableValuesInTheRightOrder, Double newParameterValue) {
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
		ArrayList<Integer> listOfVariableCardinalities = mapIntoArrayList(variableList, v->v.getCardinality());
		
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
	
	
	private void normalizeBy(Double normalizationConstant)
	{
		int numParameters = parameters.size();
		for(int i = 0; i < numParameters; ++i)
		{
			parameters.set(i, parameters.get(i)/normalizationConstant);
		}
	}
	
	
	private TableFactor sumOutEverythingExcept(LinkedHashSet<TableVariable> variablesNotToSumOut) {

		TableFactor result = new TableFactor(variablesNotToSumOut, 0.0);
		
		Iterator<ArrayList<Integer>> cartesianProduct = getCartesianProduct(this.variableList);
		LinkedHashMap<Variable, Integer> variableValueMap = new LinkedHashMap<>();
		for(ArrayList<Integer> values: in(cartesianProduct)) {
			variableValueMap = addtoVariableValueMap(variableValueMap, values);
			Double currentValue = result.getEntryFor(variableValueMap);
			Double addedValue = this.getEntryFor(variableValueMap);
			result.setEntryFor(variableValueMap, currentValue + addedValue);
		}
		return result;
	}
	
	
	private TableFactor initializeNewFactorUnioningVariables(TableFactor another)
	{
		LinkedHashSet<TableVariable> newListOfVariables = new LinkedHashSet<>(this.variableSet);
		newListOfVariables.addAll(another.variableSet);
		Integer numberOfParametersForNewListOfVariables = numEntries(newListOfVariables);
		ArrayList<Double> newParameters = arrayListFilledWith(-1.0,numberOfParametersForNewListOfVariables);	
		TableFactor newFactor = new TableFactor(newListOfVariables, newParameters);
		
		return newFactor;
	}
	
	
	private LinkedHashMap<Variable,Integer> addtoVariableValueMap(LinkedHashMap<Variable,Integer> variableValueMap, ArrayList<Integer> values)
	{
		int numVars = variableList.size();
		for(int i = 0; i < numVars; ++i)
		{
			variableValueMap.put(variableList.get(i), values.get(i));
		}

		return variableValueMap;
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
	private int getParameterIndex(Map<? extends Variable, ? extends Object> variableValueMap) {
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
	
	
	private int[] variableValuesInFactorVariableOrder(Map<? extends Variable, ? extends Object> mapFromVariableToVariableValue) {
		//TODO: error checking
		//TODO: there is no mechanism for handling partial variable assignments
		int numVariables = variableList.size();
		int[] indexOfVariablesValues = new int[numVariables];
		for(int i = 0; i < numVariables; ++i)
		{
			TableVariable v = variableList.get(i);
			indexOfVariablesValues[i] = (Integer) mapFromVariableToVariableValue.get(v);
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
	private int getParameterIndex(List<? extends Object> variableValuesInTheRightOrder) {
		int[] variableValuesArray = new int[variableValuesInTheRightOrder.size()];
		int i = 0;
		for(Object variableValue : variableValuesInTheRightOrder) {
			
			//TODO:  CURRENTLY NOT SUPPORTING VARIABLE VALUES THAT DO NOT RANGE FROM 0 - variableCardinality...  ONCE
			//		 THIS IS SUPPORTED, NEED TO ADJUST THIS SECTION OF CODE
			
			variableValuesArray[i] = (Integer) variableValue;
			i++;
		}
		
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
		int parameterIndex = this.parameterIndexRadix.getValueFor(variableValues).intValue();
		return parameterIndex;
	}
	

	private static ArrayList<Integer> makeArrayWithValuesFromZeroToCardinalityMinusOne(int cardinality) {
		ArrayList<Integer> result = new ArrayList<>(cardinality);
		for (int i = 0; i < cardinality; i++) {
			result.add(i);
		}
		return result;
	}
	
	
	private static TableFactor copyToSubTableFactorWithoutRecreatingANewMap(TableFactor factor,
			Map<TableVariable, Integer> mapOfvaluesPredetermined) {
		ArrayList<TableVariable> newVariables = new ArrayList<>(factor.getVariables());
		
		newVariables.removeAll(mapOfvaluesPredetermined.keySet());
		if(newVariables.size() == 0) {
			return null;
		}
		Iterator<ArrayList<Integer>> cartesianProduct = getCartesianProduct(newVariables);
		
		TableFactor result = new TableFactor(newVariables);
		for(ArrayList<Integer> instantiations: in(cartesianProduct)) {
			for (int i = 0; i < newVariables.size(); i++) {
				mapOfvaluesPredetermined.put(newVariables.get(i), instantiations.get(i));
			}
			Double newEntryValue =  factor.getEntryFor(mapOfvaluesPredetermined);
			result.setEntryFor(mapOfvaluesPredetermined, newEntryValue);
		}
		return result;
	}
	
	
	private TableFactor operateOnUnionedParameters(TableFactor another, TableFactor result, BiFunction<Double, Double, Double> operator)
	{
		Iterator<ArrayList<Integer>> cartesianProduct = getCartesianProduct(result.variableSet);
		
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
