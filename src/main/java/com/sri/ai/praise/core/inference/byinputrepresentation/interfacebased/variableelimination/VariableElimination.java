package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.NullaryFunction;

public class VariableElimination implements NullaryFunction<Factor> {
	
	private Variable query;
	private EditableFactorNetwork factorNetwork;
	private EliminationOrdering eliminationOrdering;
	
	public VariableElimination(Variable query, EditableFactorNetwork factorNetwork, EliminationOrdering eliminationOrdering) {
		this.query = query;
		this.factorNetwork = factorNetwork;
		this.eliminationOrdering = eliminationOrdering;
	}
	
	public VariableElimination(Variable query, EditableFactorNetwork factorNetwork) {
		this(query, factorNetwork, new DontCareEliminationOrdering(query, factorNetwork));
	}
	
	@Override
	public Factor apply() {
		in(eliminationOrdering).forEach(this::eliminate);
		Factor queryResult = Factor.multiply(factorNetwork.getFactors());
		checkQueryResult(queryResult);
		return queryResult;
	}

	private void eliminate(NextVariableInformation nextVariableInformation) {
		Variable v = nextVariableInformation.getNextVariable();
		var factorsOnV = nextVariableInformation.getFactorsOnNextVariable();
		eliminate(v, factorsOnV);
	}

	public void eliminate(Variable v, List<? extends Factor> factorsOnV) {
		explain("Eliminating ", v);
		factorNetwork.removeAll(factorsOnV);
		Factor product = getProduct(factorsOnV);
		Factor newFactor = product.sumOut(v);
		factorNetwork.add(newFactor);
	}

	public Factor getProduct(List<? extends Factor> factorsOnV) {
		Factor product = Factor.multiply(factorsOnV);
		explain("Number of variables in product factor: ", product.getVariables().size());
		return product;
	}
	
	public void checkQueryResult(Factor queryResult) {
		myAssert( ! queryResult.getVariables().isEmpty(), () -> "Query result contains no variables");
		myAssert(getFirst(queryResult.getVariables()).equals(query), () -> "Query result should be defined on query variable " + query + " alone, but is defined on " + queryResult.getVariables());
	}

}
