package com.sri.ai.praise.core.representation.interfacebased.factor.api.equality;

import java.util.List;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsAreEqual;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsAreOfIncomparableClasses;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsHaveDifferentValues;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.equality.DefaultFactorsHaveDifferentVariables;

public interface FactorsEqualityCheck<F> {
	
	F getFirst();
	
	F getSecond();
	
	boolean areEqual();

	////////////// STATIC BUILDERS
	
	static <F> DefaultFactorsAreEqual<F> factorsAreEqual(F factor1, F factor2) {
		return new DefaultFactorsAreEqual<>(factor1, factor2);
	}
	
	static <F> DefaultFactorsAreOfIncomparableClasses<F> factorsAreOfIncomparableClasses(F first, F second) {
		return new DefaultFactorsAreOfIncomparableClasses<F>(first, second);
	}

	static <F> DefaultFactorsHaveDifferentVariables factorsHaveDifferentVariables(F first, F second, Set<? extends Variable> variablesInFirstButNotInSecond, Set<? extends Variable> variablesInSecondButNotInFirst) {
		return new DefaultFactorsHaveDifferentVariables<>(first, second, variablesInFirstButNotInSecond, variablesInSecondButNotInFirst);
	}

	static <F extends Factor, V> DefaultFactorsHaveDifferentValues<F, V> factorsHaveDifferentValues(
			F first, 
			F second, 
			List<? extends V> violatingAssignment,
			double valueOfFirst,
			double valueOfSecond) {
		
		return new DefaultFactorsHaveDifferentValues<F,V>(first, second, violatingAssignment, valueOfFirst, valueOfSecond);
	}

}
