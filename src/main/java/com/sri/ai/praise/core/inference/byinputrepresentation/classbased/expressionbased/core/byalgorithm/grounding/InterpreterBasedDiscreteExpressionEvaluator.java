package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.interpreter.ContextAssignmentLookup;
import com.sri.ai.util.base.BinaryFunction;

import java.util.ArrayList;

import static com.sri.ai.expresso.api.Symbol.makeSymbol;
import static com.sri.ai.util.Util.arrayListFrom;

public class InterpreterBasedDiscreteExpressionEvaluator implements DiscreteExpressionEvaluator {

    private Expression expression;
    private BinaryFunction<Expression, Context, Expression> interpreter;
    private Context context;
    private ArrayList<? extends Expression> variables;

    public InterpreterBasedDiscreteExpressionEvaluator(
            Expression expression,
            ArrayList<? extends Expression> variables,
            BinaryFunction<Expression, Context, Expression> interpreter,
            Context context) {

        this.expression = expression;
        this.variables = variables;
        this.interpreter = interpreter;
        this.context = context;
    }

    @Override
    public double evaluate(int[] assignment) {
        setContextAssignment(assignment);
        return interpreter.apply(expression, context).doubleValue();
    }

    private void setContextAssignment(int[] indices) {
        for (int i = 0; i != variables.size(); i++) {
            var valueExpression = makeSymbol(indices[i]);
            context = ContextAssignmentLookup.setAssignment(context, variables.get(i), valueExpression);
        }
    }
}
