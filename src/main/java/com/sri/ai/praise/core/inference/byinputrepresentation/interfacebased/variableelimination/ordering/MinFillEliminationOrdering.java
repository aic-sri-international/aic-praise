package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class MinFillEliminationOrdering implements EliminationOrdering {

	@Override
	public MinFillEliminationOrder make(Variable query, FactorNetwork factorNetwork) {
		return new MinFillEliminationOrder(query, factorNetwork);
	}

}
