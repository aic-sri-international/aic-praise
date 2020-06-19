package com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor.ZERO_FACTOR;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.castOrThrowError;
import static com.sri.ai.util.Util.mapFromListOfKeysAndListOfValues;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.pair;
import static com.sri.ai.util.Util.product;
import static com.sri.ai.util.Util.removeNonDestructively;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ConstantFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.KroneckerDeltaFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.api.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.empty.EmptyTableFactor;
import com.sri.ai.util.Enclosing;
import com.sri.ai.util.base.Pair;
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

	/**
	 * Extending classes must implement this method to sum out variables; <code>variablesNotToSumOut</code> are guaranteed to be in the same order as
	 * in the original factor.
	 * @param variablesToSumOut
	 * @param variablesNotToSumOut
	 * @return
	 */
	protected abstract TableFactor sumOut(List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining);

	protected abstract TableFactor max(List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining);

	protected abstract TableFactor min(List<? extends TableVariable> eliminated, ArrayList<? extends TableVariable> remaining);

	protected abstract TableFactor normalizeBy(Double normalizationConstant);

	protected abstract double computeNormalizationConstant();

	protected abstract TableFactor addTableFactor(TableFactor another);

	protected abstract TableFactor multiplyTableFactor(TableFactor another);

	protected abstract TableFactor multiplyKroneckerDeltaFactor(KroneckerDeltaFactor another);

	protected abstract TableFactor slicePossiblyModifyingAssignment(Map<TableVariable, Integer> assignment, ArrayList<? extends TableVariable> remaining);

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

	@Override
	public int memory() {
		return numberOfEntries();
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
		var remaining = new ArrayList<>(getVariables());
		remaining.removeAll(assignment.keySet());
		TableFactor result = slicePossiblyModifyingAssignment(assignment, remaining);
		return result;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// ADDTION //////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public Factor add(Factor another) {
		
		myAssert(getVariables().equals(another.getVariables()), () -> "Added factors must agree on variables, but got " + getVariables() + " and " + another.getVariables());
		
		Factor result;
		
		if (another instanceof ConstantFactor) {
			result = another.add(this);
		}		
		else if (another.getClass() != this.getClass()) {
			throw new Error("Trying to add different types of factors: this is a " +
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
			result = this;
		}
		else if (another instanceof ZeroFactor) {
			result = ZERO_FACTOR;
		}
		else if (another instanceof TableFactor) {
			result = multiplyTableFactor((TableFactor) another);
		}
		else if (another instanceof KroneckerDeltaFactor) {
			result = multiplyKroneckerDeltaFactor((KroneckerDeltaFactor) another);
		}
		else {
			throw new Error("Unimplemented multiplication between factors of classes " + this.getClass() + " and " + another.getClass());
		}
		
		return result;
	}

	protected TableVariable getTableVariable(KroneckerDeltaFactor kronecker, int variableIndex) {
		return castOrThrowError(TableVariable.class, kronecker.getVariables().get(variableIndex), KroneckerDeltaFactor.class + " instance being operated along with TableFactor by " + getClass() + " instance but its variable %s is not an instance of %s; it is an instance of %s");
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
	public TableFactor normalize() {
		Double normalizationConstant = computeNormalizationConstant();
		if (normalizationConstant != 0.0) {
			if (normalizationConstant != 1.0) {
				return normalizeBy(normalizationConstant);
			}
			else {
				return this;
			}
		}
		else {
			throw new Error("Normalization requested but normalization constant is 0.");
		}
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// AGGREGATION //////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	public TableFactor sumOut(Collection<? extends Variable> eliminated) {
		boolean resultMayBeUpToAConstant = true;
		var eliminatedAndRemaining = organizeVariablesForElimination(eliminated, resultMayBeUpToAConstant);
		return sumOut(eliminatedAndRemaining.first, eliminatedAndRemaining.second);
	}

	@Override
	public TableFactor max(Collection<? extends Variable> eliminated) {
		boolean resultMayBeUpToAConstant = false;
		var eliminatedAndRemaining = organizeVariablesForElimination(eliminated, resultMayBeUpToAConstant);
		return max(eliminatedAndRemaining.first, eliminatedAndRemaining.second);
	}

	@Override
	public TableFactor min(Collection<? extends Variable> eliminated) {
		boolean resultMayBeUpToAConstant = false;
		var eliminatedAndRemaining = organizeVariablesForElimination(eliminated, resultMayBeUpToAConstant);
		return min(eliminatedAndRemaining.first, eliminatedAndRemaining.second);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// SUPPORT FOR AGGREGATION OPERATORS ////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Returns a pair of collections;
	 * the first collection contains the variables to be eliminated that are actually in the factor;
	 * the second collection contains the remaining variables.
	 * The option resultMayBeUpToAConstant indicates that it is ok for originally eliminated variables to not be in the factor,
	 * but in any case they will not be present in the first collection of returned pair.
	 * @param eliminated
	 * @param resultMayBeUpToAConstant
	 * @return
	 */
	protected 
	Pair<List<TableVariable>, ArrayList<TableVariable>> 
	organizeVariablesForElimination(
			Collection<? extends Variable> eliminated,
			boolean resultMayBeUpToAConstant) {
		
		var allEliminatedVariablesAreInFactor = getVariables().containsAll(eliminated);

		Collection<? extends Variable> eliminatedAndPresentInFactor;
		if (allEliminatedVariablesAreInFactor) {
			eliminatedAndPresentInFactor = eliminated;
		}
		else if (resultMayBeUpToAConstant) {
			eliminatedAndPresentInFactor = removeNonDestructively(eliminated, e -> !getVariables().contains(e));
		}
		else {
			throw new Error("Not all variables to be eliminated occur in factor, and operation cannot be up to a constant: " + eliminated + " not all in " + getVariables() + " of factor " + this);
		}
		
		// TODO: to avoid this ugly cast we would have to make TableFactor generic with the
		// type of variable V it applies to, so the parameter of this method would be
		// List<? extends V>.
		@SuppressWarnings("unchecked")
		List<TableVariable> eliminatedTableVariables = new LinkedList<>((Collection<TableVariable>) eliminatedAndPresentInFactor);
		
		var remaining = setDifference(getVariables(), eliminatedTableVariables, arrayList());
		
		return pair(eliminatedTableVariables, remaining);
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	// STRIDE ///////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	protected int getStride(int variableIndex) {
		return getStrides()[variableIndex];
	}

	protected int getStride(TableVariable variable) {
		return getStrides()[getVariables().indexOf(variable)];
	}

	private int[] strides;
	
	protected int[] getStrides() {
		if (strides == null) {
			strides = new int[getVariables().size()];
			int acc = 1;
			for (int i = getVariables().size() - 1; i != -1; i--) {
				strides[i] = acc;
				acc *= getVariables().get(i).getCardinality(); 
			}
		}
		return strides;
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
	public Factor normalize(Collection<? extends Variable> variablesToNormalize) {
		throw new Error((new Enclosing(){}).methodName() + " not yet implemented for " + getClass().getSimpleName());
	}

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
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