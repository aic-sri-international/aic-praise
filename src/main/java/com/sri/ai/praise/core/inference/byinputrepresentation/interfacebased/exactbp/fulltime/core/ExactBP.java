package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableElimination;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;

// TODO: this should be called ExactBP, and ExactBP should be called ExactBPInstance or some such.

public class ExactBP implements BinaryFunction<Variable, FactorNetwork, Factor> {

	@Override
	public Factor apply(Variable query, FactorNetwork factorNetwork) {
		return new VariableElimination(query, factorNetwork).apply();
	}

}
