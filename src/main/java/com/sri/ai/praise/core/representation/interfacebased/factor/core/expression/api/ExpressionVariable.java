package com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public interface ExpressionVariable extends Variable, Expression {

	List<? extends Object> getValues();

}