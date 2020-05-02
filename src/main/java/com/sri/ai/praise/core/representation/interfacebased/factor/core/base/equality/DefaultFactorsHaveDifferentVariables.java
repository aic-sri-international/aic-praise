package com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality;

import static com.sri.ai.util.Util.set;
import static com.sri.ai.util.Util.setDifference;

import java.util.Collection;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.equality.FactorsHaveDifferentVariables;

public class DefaultFactorsHaveDifferentVariables<F extends Factor> 
extends AbstractFactorsEqualityCheck<F>
implements FactorsHaveDifferentVariables<F> {
	
	private Set<? extends Variable> variablesInFirstButNotInSecond;
	private Set<? extends Variable> variablesInSecondButNotInFirst;

	public DefaultFactorsHaveDifferentVariables(F first, F second, Set<? extends Variable> variablesInFirstButNotInSecond, Set<? extends Variable> variablesInSecondButNotInFirst) {
		super(first, second);
		this.variablesInFirstButNotInSecond = variablesInFirstButNotInSecond;
		this.variablesInSecondButNotInFirst = variablesInSecondButNotInFirst;
	}

	public DefaultFactorsHaveDifferentVariables(F first, F second) {
		this(
				first, second, 
				setDifference(first.getVariables(), second.getVariables(), set()),
				setDifference(second.getVariables(), first.getVariables(), set())
			);
	}

	@Override
	public Collection<? extends Variable> getVariablesInFirstButNotInSecond() {
		return variablesInFirstButNotInSecond;
	}

	@Override
	public Collection<? extends Variable> getVariablesInSecondButNotInFirst() {
		return variablesInSecondButNotInFirst;
	}
	
	@Override
	public String toString() {
		return 
				"Factors have different variables. Variables " + 
				getVariablesInFirstButNotInSecond() + 
				" appear in first but not second, and variables " + 
				getVariablesInSecondButNotInFirst() + 
				" appear in second but not first.";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result =
				prime * result
						+ ((variablesInFirstButNotInSecond == null) ? 0 : variablesInFirstButNotInSecond.hashCode());
		result =
				prime * result
						+ ((variablesInSecondButNotInFirst == null) ? 0 : variablesInSecondButNotInFirst.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		@SuppressWarnings("unchecked")
		DefaultFactorsHaveDifferentVariables<F> other = (DefaultFactorsHaveDifferentVariables<F>) obj;
		if (variablesInFirstButNotInSecond == null) {
			if (other.variablesInFirstButNotInSecond != null) {
				return false;
			}
		} else if (!variablesInFirstButNotInSecond.equals(other.variablesInFirstButNotInSecond)) {
			return false;
		}
		if (variablesInSecondButNotInFirst == null) {
			if (other.variablesInSecondButNotInFirst != null) {
				return false;
			}
		} else if (!variablesInSecondButNotInFirst.equals(other.variablesInSecondButNotInFirst)) {
			return false;
		}
		return true;
	}

}
