package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination;

import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.collect.EZIterator;

public class DontCareEliminationOrdering extends EZIterator<NextVariableInformation> implements EliminationOrdering {

	private Variable query;
	private FactorNetwork factorNetwork;
	
	public DontCareEliminationOrdering(Variable query, FactorNetwork factorNetwork) {
		this.query = query;
		this.factorNetwork = factorNetwork;
	}


	@Override
	protected NextVariableInformation calculateNext() {
		Variable nextVariable = getFirstSatisfyingPredicateOrNull(factorNetwork.getVariables(), v -> !v.equals(query));
		if (nextVariable == null) {
			return null;
		}
		else {
			List<Factor> factorsOnNextVariable = collectToList(factorNetwork.getFactors(), f -> f.contains(nextVariable));
			return new NextVariableInformation(nextVariable, factorsOnNextVariable);
		}
	}

}
