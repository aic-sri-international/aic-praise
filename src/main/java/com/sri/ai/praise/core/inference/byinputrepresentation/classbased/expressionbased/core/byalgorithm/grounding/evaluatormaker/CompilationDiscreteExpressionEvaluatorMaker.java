package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.evaluatormaker;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.interpreter.CompilationEvaluator;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.DiscreteExpressionEvaluator;

import java.util.ArrayList;

public class CompilationDiscreteExpressionEvaluatorMaker
    implements DiscreteExpressionEvaluatorMaker {

    @Override
    public DiscreteExpressionEvaluator apply(
            Expression expression,
            ArrayList<? extends Expression> variables,
            Context context) {

        var evaluator = CompilationEvaluator.makeEvaluator(expression, variables);
        return assignment -> ((Number) evaluator.apply(assignment)).doubleValue();
    }
}
