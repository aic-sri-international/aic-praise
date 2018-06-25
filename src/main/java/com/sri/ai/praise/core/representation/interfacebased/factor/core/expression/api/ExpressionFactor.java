package com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;

public interface ExpressionFactor extends Factor, Expression {
	
	Context getContext();

}
