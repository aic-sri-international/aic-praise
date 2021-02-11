package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination;

import static com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor.multiply;
import static com.sri.ai.util.Util.*;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;

import java.util.List;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering.EliminationOrder;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering.EliminationOrdering;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering.MinFillEliminationOrdering;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering.NextVariableInformation;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultEditableFactorNetwork;
import com.sri.ai.util.base.NullaryFunction;

public class VariableElimination implements NullaryFunction<Factor> {
	
	private static final EliminationOrdering DEFAULT_ELIMINATION_ORDERING = new MinFillEliminationOrdering();
	
	private Variable query;
	private EditableFactorNetwork factorNetwork;
	private EliminationOrder eliminationOrder;
	
	//////////// CONSTRUCTORS
	
	private void build(Variable query, EditableFactorNetwork factorNetwork, EliminationOrdering eliminationOrdering) {
		this.query = query;
		this.factorNetwork = factorNetwork;
		this.eliminationOrder = eliminationOrdering.make(query, factorNetwork);
	}
	
	private VariableElimination(Variable query, EditableFactorNetwork factorNetwork, EliminationOrdering eliminationOrdering) {
		EditableFactorNetwork internalFactorNetwork = factorNetwork.clone();
		build(query, internalFactorNetwork, eliminationOrdering);
	}

	public VariableElimination(Variable query, FactorNetwork factorNetwork, EliminationOrdering eliminationOrdering) {
		DefaultEditableFactorNetwork internalFactorNetwork = new DefaultEditableFactorNetwork(factorNetwork.getFactors());
		build(query, internalFactorNetwork, eliminationOrdering);
	}
	
	public VariableElimination(Variable query, FactorNetwork factorNetwork) {
		this(query, factorNetwork, DEFAULT_ELIMINATION_ORDERING);
	}
	
	public VariableElimination(Variable query, EditableFactorNetwork factorNetwork) {
		this(query, factorNetwork, DEFAULT_ELIMINATION_ORDERING);
	}
	
	//////////// MAIN ALGORITHM
	
	@Override
	public Factor apply() {
		in(eliminationOrder).forEach(this::eliminate);
		Factor queryResult = Factor.multiply(factorNetwork.getFactors());
		return queryResult;
	}

	private void eliminate(NextVariableInformation nextVariableInformation) {
		Variable v = nextVariableInformation.getNextVariable();
		var factorsOnV = nextVariableInformation.getFactorsOnNextVariable();
		eliminate(v, factorsOnV);
	}

	public void eliminate(Variable v, List<? extends Factor> factorsOnV) {
		explain("Eliminating ", v, ", free memory: ", actualFreeMemory()/1000000, " MB");
		factorNetwork.removeAll(factorsOnV);
		Factor product = getProduct(factorsOnV);
		Factor newFactor = product.sumOut(v);
		factorNetwork.add(newFactor);
	}

	public Factor getProduct(List<? extends Factor> factorsOnV) {
		Factor product = multiply(factorsOnV);
		explain("Summation cost in product factor: ", product.summationCost());
		return product;
	}

}
