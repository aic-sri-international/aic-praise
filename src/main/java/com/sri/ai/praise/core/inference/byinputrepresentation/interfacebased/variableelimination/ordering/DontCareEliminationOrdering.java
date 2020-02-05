package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class DontCareEliminationOrdering implements EliminationOrdering {

	@Override
	public DontCareEliminationOrder make(Variable query, FactorNetwork factorNetwork) {
		return new DontCareEliminationOrder(query, factorNetwork);
	}

}
