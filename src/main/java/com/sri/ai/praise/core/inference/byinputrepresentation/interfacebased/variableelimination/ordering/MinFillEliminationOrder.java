package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.ordering;

import static com.sri.ai.util.Util.argmin;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.base.NotEquals.notEquals;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.collect.EZIterator;

public class MinFillEliminationOrder extends EZIterator<NextVariableInformation> implements EliminationOrder {

	private Variable query;
	private FactorNetwork factorNetwork;
	
	public MinFillEliminationOrder(Variable query, FactorNetwork factorNetwork) {
		this.query = query;
		this.factorNetwork = factorNetwork;
	}

	private class MinFillNextVariableInformation extends NextVariableInformation {

		private int summationCost;
		
		public MinFillNextVariableInformation(Variable nextVariable, List<? extends Factor> factorsOnNextVariable, int summationCost) {
			super(nextVariable, factorsOnNextVariable);
			this.summationCost = summationCost;
		}
		
		public int getSummationCost() {
			return summationCost;
		}
		
	}

	private MinFillNextVariableInformation computeVariableInformation(Variable variable) {
		List<Factor> factorsOnVariable = collectToList(factorNetwork.getFactors(), f -> f.contains(variable));
		List<Factor> emptyFactorsOnVariable = mapIntoList(factorsOnVariable, Factor::emptyVersion);
		Factor emptyProduct = Factor.multiply(emptyFactorsOnVariable);
		return new MinFillNextVariableInformation(variable, factorsOnVariable, emptyProduct.summationCost());
	}
	
	@Override
	protected NextVariableInformation calculateNext() {
		var remainingVariables = predicateIterator(factorNetwork.getVariables(), notEquals(query));
		var remainingVariablesInformation = mapIntoList(remainingVariables, this::computeVariableInformation);
		return argmin(remainingVariablesInformation, MinFillNextVariableInformation::getSummationCost);
	}

}
