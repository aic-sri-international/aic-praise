package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.evaluatormaker;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.DiscreteExpressionEvaluator;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.HardCodedIncrementalDiscreteExpressionEvaluator;

import java.util.ArrayList;

public class HardCodedIncrementalDiscreteExpressionEvaluatorMaker
    implements DiscreteExpressionEvaluatorMaker {
    @Override
    public DiscreteExpressionEvaluator apply(
            Expression expression,
            ArrayList<? extends Expression> variables,
            Context context) {

        return
                new HardCodedIncrementalDiscreteExpressionEvaluator(
                expression, variables, context)::evaluate;
    }
}
