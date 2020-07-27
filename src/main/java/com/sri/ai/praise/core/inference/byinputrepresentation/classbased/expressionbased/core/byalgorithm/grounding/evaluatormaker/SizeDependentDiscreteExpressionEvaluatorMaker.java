package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.evaluatormaker;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.interpreter.CompilationIncrementalEvaluator;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.DiscreteExpressionEvaluator;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.HardCodedIncrementalDiscreteExpressionEvaluator;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.TableVariableMaker;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;

import java.util.ArrayList;

import static com.sri.ai.util.Util.product;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

/**
 * A {@link DiscreteExpressionEvaluatorMaker} that provides a
 * {@link CompilationIncrementalEvaluator} if the number of entries to be evaluated
 * is greater than {@link SizeDependentDiscreteExpressionEvaluatorMaker#MIN_NUMBER_OF_ENTRIES_FOR_COMPILATION}
 * (10,000) or a {@link HardCodedIncrementalDiscreteExpressionEvaluator} otherwise.
 * <p>
 * This is motivated by the compilation taking a long but fixed time (in terms of number of entries),
 * so it is useful only when a large number of entries will need to be evaluated.
 */
public class SizeDependentDiscreteExpressionEvaluatorMaker
    implements DiscreteExpressionEvaluatorMaker {

    public final int MIN_NUMBER_OF_ENTRIES_FOR_COMPILATION = 10000;

    @Override
    public DiscreteExpressionEvaluator apply(
            Expression expression,
            ArrayList<? extends Expression> variables,
            Context context) {

        if (numberOfEntries(variables, context) > MIN_NUMBER_OF_ENTRIES_FOR_COMPILATION) {
            var evaluator = CompilationIncrementalEvaluator.makeEvaluator(expression, variables);
            return assignment -> ((Number) evaluator.apply(assignment)).doubleValue();
        }
        else {
            return
                    new HardCodedIncrementalDiscreteExpressionEvaluator(expression, variables, context)
                            ::evaluate;
        }
    }

    private static int numberOfEntries(ArrayList<? extends Expression> variables, Context context) {
        var tableVariables = TableVariableMaker.getTableVariables(variables, context);
        return product(functionIterator(tableVariables, TableVariable::getCardinality))
                .intValue();
    }
}
