package com.sri.ai.praise.core.representation.translation.fromexpressionstosamplingfactors;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;

public class UniqueExpressionVariable extends DefaultExpressionVariable {

	private static final long serialVersionUID = 1L;
	
	private static int uniqueVariableOccurrenceIndex = 0;

	public UniqueExpressionVariable(Expression expression) {
		super(Expressions.apply("unique", expression, uniqueVariableOccurrenceIndex++));
	}
	
	@Override
	public String toString() {
		return "var(" + get(0) + ")";
	}

}
