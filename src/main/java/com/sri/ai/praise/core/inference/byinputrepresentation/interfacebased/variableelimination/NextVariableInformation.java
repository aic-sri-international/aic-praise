package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class NextVariableInformation {

		private Variable nextVariable;
		private List<? extends Factor> factorsOnNextVariable;
		
		public NextVariableInformation(Variable nextVariable, List<? extends Factor> factorsOnNextVariable) {
			this.nextVariable = nextVariable;
			this.factorsOnNextVariable = factorsOnNextVariable;
		}

		public Variable getNextVariable() {
			return nextVariable;
		}

		public List<? extends Factor> getFactorsOnNextVariable() {
			return factorsOnNextVariable;
		}
}
