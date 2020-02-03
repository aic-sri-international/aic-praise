package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination;

import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.NullaryFunction;

public class VariableElimination implements NullaryFunction<Factor> {
	
	private Variable query;
	private EditableFactorNetwork factorNetwork;
	
	public VariableElimination(Variable query, EditableFactorNetwork factorNetwork) {
		this.query = query;
		this.factorNetwork = factorNetwork;
	}

	@Override
	public Factor apply() {
		Variable nextVariableToBeEliminated;
		while ((nextVariableToBeEliminated = pickNextVariableToBeEliminated()) != null) {
			eliminate(nextVariableToBeEliminated);
		}
		return getFirst(factorNetwork.getFactors());
	}

	private Variable pickNextVariableToBeEliminated() {
		return getFirstSatisfyingPredicateOrNull(factorNetwork.getVariables(), v -> !v.equals(query));
	}

	private void eliminate(Variable nextVariableToBeEliminated) {
		explain("Eliminating ", nextVariableToBeEliminated);
		List<Factor> factorsOnNextVariable = removeFactorsOnVariable(nextVariableToBeEliminated);
		Factor product = Factor.multiply(factorsOnNextVariable);
		explain("Number of variables in product factor: ", product.getVariables().size());
		Factor newFactor = product.sumOut(list(nextVariableToBeEliminated));
		factorNetwork.add(newFactor);
	}

	private List<Factor> removeFactorsOnVariable(Variable variable) {
		List<Factor> factorsOnNextVariable = collectToList(factorNetwork.getFactors(), f -> f.contains(variable));
		factorNetwork.removeAll(factorsOnNextVariable);
		return factorsOnNextVariable;
	}
	
}
