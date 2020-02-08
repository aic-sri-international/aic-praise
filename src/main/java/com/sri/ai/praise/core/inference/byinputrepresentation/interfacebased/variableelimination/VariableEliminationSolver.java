package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering.EliminationOrdering;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;

// TODO: this should be called VariableElimination, and VariableElimination should be called VariableEliminationInstance or some such.

public class VariableEliminationSolver implements BinaryFunction<Variable, FactorNetwork, Factor> {

	private EliminationOrdering eliminationOrdering;
	
	public VariableEliminationSolver(EliminationOrdering eliminationOrdering) {
		this.eliminationOrdering = eliminationOrdering;
	}
	
	@Override
	public Factor apply(Variable query, FactorNetwork factorNetwork) {
		return new VariableElimination(query, factorNetwork, eliminationOrdering).apply();
	}

}
