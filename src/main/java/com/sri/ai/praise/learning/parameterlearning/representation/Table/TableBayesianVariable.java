package com.sri.ai.praise.learning.parameterlearning.representation.Table;

import com.sri.ai.praise.inference.representation.Table.TableVariable;
import com.sri.ai.praise.learning.parameterlearning.Variable;

public class TableBayesianVariable extends TableVariable implements Variable {

	public TableBayesianVariable(String variableName, int cardinality) {
		super(variableName, cardinality);
	}

}
