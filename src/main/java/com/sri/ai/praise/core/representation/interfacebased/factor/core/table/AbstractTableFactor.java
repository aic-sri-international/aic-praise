package com.sri.ai.praise.core.representation.interfacebased.factor.core.table;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.mapFromListOfKeysAndListOfValues;
import static com.sri.ai.util.Util.product;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ConstantFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.util.Enclosing;
import com.sri.ai.util.explanation.tree.DefaultExplanationTree;
import com.sri.ai.util.explanation.tree.ExplanationTree;

public abstract class AbstractTableFactor implements TableFactor {

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// ABSTRACT METHODS     /////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public abstract void setEntryFor(ArrayList<Integer> values, Double newParameterValue);

	protected abstract void setEntryFor(int[] values, Double newParameterValue);

	@Override
	public abstract Double getEntryFor(ArrayList<Integer> values);

	protected abstract Double getEntryFor(int[] values);

	@Override
	public abstract TableFactor invert();

	protected abstract TableFactor sumOutEverythingExcept(ArrayList<? extends TableVariable> variablesNotToSumOut);

	protected abstract void normalizeBy(Double normalizationConstant);

	protected abstract double computeNormalizationConstant();

	protected abstract TableFactor addTableFactor(TableFactor another);

	protected abstract TableFactor multiplyTableFactor(TableFactor another);

	protected abstract TableFactor slicePossiblyModifyingAssignment(Map<TableVariable, Integer> assignment, ArrayList<? extends TableVariable> remainingVariables);

	protected abstract String parametersString();

	protected abstract boolean parametersAreAllEqual();

	protected abstract boolean thereAreZeroParameters();

	protected abstract boolean firstParameterIsZero();
	
	////////////////////////////////////////////////////////////////////////////////////////////////////
	// DATA MEMBERS ////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////
	
	protected String name;
	protected final ArrayList<? extends TableVariable> variables;
	private ExplanationTree explanation = DefaultExplanationTree.PLACEHOLDER;

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTOR //////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@SuppressWarnings("unchecked")
	public AbstractTableFactor(String factorName, Collection<? extends TableVariable> variables) {

		this.name = factorName;
		
		if (variables instanceof ArrayList) {
			this.variables = (ArrayList<TableVariable>) variables;
		}
		else {
			this.variables = new ArrayList<TableVariable>(variables);
		}
	}
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// BASIC METHODS ////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ArrayList<? extends TableVariable> getVariables() {
		return variables;
	}
	
	@Override
	public void setName(String newName) {
		this.name = newName;
	}

	@Override
	public boolean contains(Variable variable) {
		return variables.contains(variable);
	}

	@Override
	public boolean isIdentity() {
		if (thereAreZeroParameters()) {
			return true;	
		}
		if (firstParameterIsZero()) {
			return false;	
		}
		return parametersAreAllEqual();
	}

	@Override
	public boolean isZero() {
		return firstParameterIsZero() && parametersAreAllEqual();
	}

	@Override
	public String toString() {
		return name + variables.toString() + ": " + parametersString();
	}

	private int numberOfEntries = -1;
	@Override
	public int numberOfEntries() {
		if (numberOfEntries == -1) {
			numberOfEntries = numberOfEntries(variables);
		}
		return numberOfEntries;
	}
	
	protected static int numberOfEntries(Collection<? extends TableVariable> variables) {
		return product(functionIterator(variables, TableVariable::getCardinality)).intValue();
	}
	
	@Override
	public int summationCost() {
		return numberOfEntries();
	}
	
	
	@Override
	public EmptyTableFactor emptyVersion() {
		return new EmptyTableFactor(this);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SLICING //////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Makes a new factor representing a slice of the original factor.
	 * 
	 * @param factor (factor to slice on)
	 * @param variables (variables to slice on)
	 * @param values (values of the above-mentioned variables to slice by)
	 * @return sub-factor produced from slicing the passed variables at their given values
	 */
	@Override
	public TableFactor slice(List<TableVariable> variables, List<Integer> values) {
		var assignment = mapFromListOfKeysAndListOfValues(variables, values);
		TableFactor result = slicePossiblyModifyingAssignment(assignment);
		return result;
	}

	@Override
	public TableFactor slice(Map<TableVariable, Integer> assignment) {
		var assignmentCopy = new LinkedHashMap<>(assignment);
		TableFactor result = slicePossiblyModifyingAssignment(assignmentCopy);
		return result;
	}

	private TableFactor slicePossiblyModifyingAssignment(Map<TableVariable, Integer> assignment) {
		var remainingVariables = new ArrayList<>(getVariables());
		remainingVariables.removeAll(assignment.keySet());
		TableFactor result = slicePossiblyModifyingAssignment(assignment, remainingVariables);
		return result;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// ADDTION //////////////////////////////////////////////////////////////////////////////////////////
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
			result = addTableFactor((TableFactor) another);
		}
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
			result = multiplyTableFactor((TableFactor) another);
		}
		
		return result;
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
	public AbstractTableFactor normalize() {
		
		// TODO: big problem here, operation is not supposed to be in-place!
		
		Double normalizationConstant = computeNormalizationConstant();
		if (normalizationConstant != 0.0 && normalizationConstant != 1.0) {
			normalizeBy(normalizationConstant);
		}
		return this;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SUMMING OUT //////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Sums out given variables from factor.
	 * 
	 * @param variablesToSumOut (variables to sum out)
	 * @return new factor with given variables summed out
	 */
	@Override
	public TableFactor sumOut(List<? extends Variable> variablesToSumOutList) {
		
		//TODO: Error check for if variablesToSumOut is of type List<? extends TableVariable>
		//TODO: Error check for if a variable listed to sum out exists in the factor
		
		// TODO: to avoid this ugly cast we would have to make TableFactor generic with the
		// type of variable V it applies to, so the parameter of this method would be
		// List<? extends V>.
		@SuppressWarnings("unchecked")
		LinkedHashSet<TableVariable> variablesToSumOut = new LinkedHashSet<>((List<TableVariable>) variablesToSumOutList);
		
		var variablesNotToSumOut = setDifference(variables, variablesToSumOut, arrayList());
		
		TableFactor result = sumOutEverythingExcept(variablesNotToSumOut);
		
		return result;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// INDEXED ACCESS ///////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public Double getEntryFor(Map<? extends Variable, ?> assignment) {
		int[] values = variableValuesInFactorVariableOrder(assignment);
		Double result = getEntryFor(values);
		return result;
	}

	@Override
	public void setEntryFor(Map<? extends Variable, ? extends Integer> assignment, Double newParameterValue) {
		int[] values = variableValuesInFactorVariableOrder(assignment);
		setEntryFor(values, newParameterValue);
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
	// UNIMPLEMENTED AGGREGATIONS ///////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		throw new Error((new Enclosing(){}).methodName() + " not yet implemented for " + getClass().getSimpleName());
	}

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
		throw new Error((new Enclosing(){}).methodName() + " not yet implemented for " + getClass().getSimpleName());
	}

	@Override
	public Factor min(Collection<? extends Variable> variablesToMinimize) {
		throw new Error((new Enclosing(){}).methodName() + " not yet implemented for " + getClass().getSimpleName());
	}

	@Override
	public Factor argmin(Collection<? extends Variable> variablesToMinimize) {
		throw new Error((new Enclosing(){}).methodName() + " not yet implemented for " + getClass().getSimpleName());
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// UNIMPLEMENTED EXPLANATIONS ///////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public ExplanationTree getExplanation() {
		return explanation;
	}

	@Override
	public void setExplanation(ExplanationTree explanation) {
		this.explanation = explanation;
	}

}