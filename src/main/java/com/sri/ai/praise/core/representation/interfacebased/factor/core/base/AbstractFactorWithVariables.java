package com.sri.ai.praise.core.representation.interfacebased.factor.core.base;

import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.unorderedEquals;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsEqualityCheck;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsAreEqual;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsAreOfIncomparableClasses;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsHaveDifferentVariables;
import com.sri.ai.util.Enclosing;
import com.sri.ai.util.explanation.tree.ExplanationTree;

/** An abstract implementation of a factor storing its own variables. */
public abstract class AbstractFactorWithVariables implements Factor {

	private ArrayList<? extends Variable> variables;
	
	public AbstractFactorWithVariables(ArrayList<? extends Variable> variables) {
		this.variables = variables;
	}
	
	ArrayList<? extends Variable> getVariablesArrayList() {
		return variables;
	}
	
	@Override
	public boolean contains(Variable variable) {
		return variables.contains(variable);
	}

	@Override
	public List<? extends Variable> getVariables() {
		return Collections.unmodifiableList(variables);
	}

	@Override
	public Factor normalize() {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public Factor add(Factor another) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public Factor invert() {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public Factor min(Collection<? extends Variable> variablesToMinimize) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public Factor argmin(Collection<? extends Variable> variablesToMinimize) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public ExplanationTree getExplanation() {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public Factor potentialRange(Collection<? extends Variable> variablesToEliminate) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public double value() {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public void setExplanation(ExplanationTree explanation) {
		throw new Error((new Enclosing(){}).methodName() + " not implemented for " + getClass());
	}

	@Override
	public boolean equals(Object another) {
		if (another instanceof Factor) {
			return mathematicallyEquals((Factor) another);
		}
		else {
			return false;
		}
	}
	
	@Override
	public int hashCode() {
		return variables.hashCode();
	}
	
	@Override
	public boolean mathematicallyEquals(Factor another) {
		if (another instanceof AbstractFactorWithVariables) {
			var anotherWithVariables = (AbstractFactorWithVariables) another;
			return unorderedEquals(variables, anotherWithVariables.getVariables());
		}
		else {
			return false;
		}
	}

	@Override
	public FactorsEqualityCheck checkEquality(Factor another) {
		if (another instanceof AbstractFactorWithVariables) {
			var anotherWithVariables = (AbstractFactorWithVariables) another;
			boolean equals = unorderedEquals(variables, anotherWithVariables.getVariables());
			if (equals) {
				return new DefaultFactorsAreEqual<>(this, another);
			}
			else {
				return new DefaultFactorsHaveDifferentVariables<>(this, another);
			}
		}
		else {
			return new DefaultFactorsAreOfIncomparableClasses<>(this, another);
		}
	}

	@Override
	public String toString() {
		return getClass().getSimpleName() + "(" + join(variables) + ")";
	}
}
