package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.util.Util;

import java.util.ArrayList;
import java.util.Map;
import java.util.function.Function;

import static com.sri.ai.util.Util.*;

/**
 * A {@link DiscreteExpressionEvaluator} with incremental functionality.
 */
public class HardCodedIncrementalDiscreteExpressionEvaluator
        extends HardCodedDiscreteExpressionEvaluator
        implements DiscreteExpressionEvaluator {

    /**
     * This currently piggybacks on {@link HardCodedDiscreteExpressionEvaluator} for the evaluation.
     * It achieves this by intercepting {@link HardCodedDiscreteExpressionEvaluator#evaluateVariable(Expression, Context)}
     * to read a variable's value off the current <code>int[]</code> assignment
     * (which is stored when {@link DiscreteExpressionEvaluator#evaluate(int[])} is invoked).
     * It therefore does NOT actually use values stored in the context as a normal {@link HardCodedDiscreteExpressionEvaluator} would.
     */

    private ArrayList<Expression> variables;
    private Map<Expression, Integer> variableIndices;
    private Map<Expression, Object> cache = map();
    private Map<Expression, Integer> maxVariableIndex = map();

    public HardCodedIncrementalDiscreteExpressionEvaluator(Expression expression, Context context) {
        super(expression, context);
        this.variables = arrayListFrom(Expressions.getVariablesBeingReferenced(expression, context));
        this.variableIndices = map();
        for (int i = 0; i != variables.size(); i++) {
            variableIndices.put(variables.get(i), i);
        }
        computeMaxVariableIndex(expression);
    }

    private int computeMaxVariableIndex(Expression expression) {
        Integer value;
        if (context.isVariable(expression)) {
            value = variableIndices.get(expression);
        }
        else {
            value = (Integer) Util.max(mapIntoList(expression.getArguments(), this:: computeMaxVariableIndex));
            if (value == null) {
                value = -1;
            }
        }
        maxVariableIndex.put(expression, value);
        return value;
    }

    @Override
    protected Object evaluateVariable(Expression variable, Context context) {
        return currentAssignment[variableIndices.get(variable)];
    }

    private boolean firstAssignment = true;
    @Override
    protected
    Object
    beforeEvaluate(
            Function<ArrayList<Object>, Object> functorOperation,
            Expression expression,
            Context context) {

        Object result;
        if (firstAssignment) {
            result = null;
            firstAssignment = false;
        }
        else if (maxVariableIndex.get(expression) >= findIndexOfMostSignificantChangedVariable(context)) {
            result = null;
        }
        else {
            result = cache.get(expression);
        }
        return result;
    }

    private int findIndexOfMostSignificantChangedVariable(Context context) {
        int indexOfMostSignificantChangedVariable;
        int value = 0;
        for (indexOfMostSignificantChangedVariable = variables.size();
             indexOfMostSignificantChangedVariable != 0
                     &&
                     (value = valueOf(indexOfMostSignificantChangedVariable - 1)) == 0;
             indexOfMostSignificantChangedVariable--)
            ;

        if (value != 0) {
            indexOfMostSignificantChangedVariable--;
        }

        return indexOfMostSignificantChangedVariable;
    }

    private int valueOf(int variableIndex) {
        return currentAssignment[variableIndex];
    }

    @Override
    protected void afterEvaluate(
            Function<ArrayList<Object>, Object> functorOperation,
            Expression expression,
            Context context,
            Object resultingValue) {

        cache.put(expression, resultingValue);
    }
}
