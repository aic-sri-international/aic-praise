package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.util.base.TernaryFunction;

import java.util.ArrayList;

public interface FromExpressionAndContextToDiscreteExpressionEvaluator
extends TernaryFunction<Expression, ArrayList<? extends Expression>, Context, DiscreteExpressionEvaluator> {
    
}
