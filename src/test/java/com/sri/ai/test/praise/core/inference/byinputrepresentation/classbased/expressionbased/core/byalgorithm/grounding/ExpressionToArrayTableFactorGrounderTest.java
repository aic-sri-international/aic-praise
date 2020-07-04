package com.sri.ai.test.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.interpreter.CompilationEvaluator;
import com.sri.ai.grinder.interpreter.CompilationIncrementalEvaluator;
import com.sri.ai.grinder.interpreter.FastInterpreter;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.ExpressionToArrayTableFactorGrounder;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.InterpreterBasedDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.Timer;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.arrayTableFactor;
import static com.sri.ai.util.Timer.timeStringInSeconds;
import static com.sri.ai.util.Util.*;
import static com.sri.ai.util.base.Pair.pair;
import static org.junit.jupiter.api.Assertions.fail;

class ExpressionToArrayTableFactorGrounderTest {

    Map<String, FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder>

            evaluatorMakers =
            Util.map(
                    // repeating interpreter for JVM burn-in (second time around is faster)
                    "BruteForceInterpreter",
                    (FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder)
                            (e, c) -> new InterpreterBasedDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder(e,
                                    new BruteForceCommonInterpreter(), c),

                    "FastInterpreter",
                    (FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder)
                            (e, c) -> new InterpreterBasedDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder(e,
                                    new FastInterpreter(), c),

                    "CompilationEvaluator",
                    (FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder)
                            (e, c) -> {
                                var evaluator = CompilationEvaluator.makeEvaluatorForVariablesInOccurrenceOrderIn(e);
                                return array -> ((Number)evaluator.apply(array)).doubleValue();
                            },

                    "CompilationIncrementalEvaluator",
                    (FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder)
                            (e, c) -> {
                                var evaluator = CompilationIncrementalEvaluator.makeEvaluatorForVariablesInOccurrenceOrderIn(e);
                                return array -> ((Number)evaluator.apply(array)).doubleValue();
                            },

                    "BruteForceInterpreter2",
                    (FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder)
                            (e, c) -> new InterpreterBasedDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder(e,
                                    new BruteForceCommonInterpreter(), c),

                    "FastInterpreter2",
                    (FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder)
                            (e, c) -> new InterpreterBasedDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder(e,
                                    new FastInterpreter(), c)
                    );

    @Test
    void test() {

        ArrayTableFactor.maximumNumberOfEntriesToShow = 50;

        Map<String, Integer> variableDefinitions = map("i", 5, "j", 5);
        var def = variableDefinitions;

        String expressionString;
        ArrayTableFactor expectedFactor;

        expressionString = "if i < 3 then if j < 4 then 0.3 else 0.7 else 0.1";
        expectedFactor = arrayTableFactor(vars("i, j", def), (vi, vj) -> vi < 3 ? vj < 4? 0.3 : 0.7 : 0.1);
        runTest(expressionString, variableDefinitions, expectedFactor);

        expressionString = "if i < 3 and j = 4 then 0.3 else 0.7";
        expectedFactor = arrayTableFactor(vars("i, j", def), (vi, vj) -> vi < 3 && vj == 4 ? 0.3 : 0.7);
        runTest(expressionString, variableDefinitions, expectedFactor);

        expressionString = "1.0";
        expectedFactor = arrayTableFactor(list(), () -> 1.0);
        runTest(expressionString, variableDefinitions, expectedFactor);
    }

    @Test
    void performanceTest() {

        ArrayTableFactor.maximumNumberOfEntriesToShow = 50;

        Map<String, Integer> variableDefinitions = map("i", 100, "j", 100);
        var def = variableDefinitions;

        String expressionString;
        ArrayTableFactor expectedFactor;

        expressionString = "if i < 3 and j = 4 then 0.3 else 0.7";
        expectedFactor = arrayTableFactor(vars("i, j", def), (vi, vj) -> vi < 3 && vj == 4 ? 0.3 : 0.7);
        runTest(expressionString, variableDefinitions, expectedFactor);

        expressionString = "if i < 3 and j != 4 then if i > 0 or j = 3 then 0.3 else 0.5 else 0.7";
        expectedFactor = arrayTableFactor(vars("i, j", def), (vi, vj) -> vi < 3 && vj != 4 ? (vi > 0 || vj == 3? 0.3 : 0.5) : 0.7);
        runTest(expressionString, variableDefinitions, expectedFactor);

        expressionString = "if i < 3 and j != 4 then if i > 0 or j = 3 then 0.3 else 0.5 else if i = 2 and j = 1 then 0.1 else 0.2";
        expectedFactor = arrayTableFactor(vars("i, j", def), (vi, vj) -> vi < 3 && vj != 4 ? (vi > 0 || vj == 3? 0.3 : 0.5) : vi == 2 && vj == 1? 0.1 : 0.2);
        runTest(expressionString, variableDefinitions, expectedFactor);
    }

    private List<TableVariable> vars(String variables, Map<String, Integer> variableDefinitions) {
        var variableNamesArray = variables.split("\\s*,\\s*");
        return mapIntoList(variableNamesArray, name -> new TableVariable(name, variableDefinitions.get(name)));
    }

    private void runTest(
            String expressionString,
            Map<String, Integer> variableDefinitions,
            ArrayTableFactor expectedFactor) {

        println();
        println("Variables and cardinalities : ", variableDefinitions);
        println("Expression: ", expressionString);
        println("Expected factor: ", expectedFactor);

        for (var entry: evaluatorMakers.entrySet()) {
            var name = entry.getKey();
            var fromExpressionAndContextToEvaluator = entry.getValue();
            runTest(expressionString, variableDefinitions, expectedFactor, name, fromExpressionAndContextToEvaluator);
        }
    }

    @NotNull
    private void runTest(
            String expressionString,
            Map<String, Integer> variableDefinitions,
            ArrayTableFactor expectedFactor,
            String name,
            FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder fromExpressionAndContextToEvaluator) {

        var actualFactorAndTime =
                arrayTableFactorFromExpressionAndVariableDefinitions(
                        expressionString, variableDefinitions, fromExpressionAndContextToEvaluator);
        var actualFactor = actualFactorAndTime.first;

        println("Actual factor by " + name + ": ", actualFactor);
        println("Time by " + name + ": ", timeStringInSeconds(actualFactorAndTime, 2));
        var equalityCheck = expectedFactor.checkEquality(actualFactor);
        if (!equalityCheck.areEqual()) {
            fail(equalityCheck.toString());
        }
    }

    @NotNull
    private Pair<ArrayTableFactor, Long>
    arrayTableFactorFromExpressionAndVariableDefinitions(
            String expressionString,
            Map<String, Integer> variableDefinitions,
            FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder fromExpressionAndContextToEvaluator) {

        var expressionAndHOGModel = getExpressionAndHOGModel(expressionString, variableDefinitions);
        Expression expression = expressionAndHOGModel.first;
        Context context = expressionAndHOGModel.second.getContext();
        return Timer.getResultAndTime(
                () -> arrayTableFactorFromModelAndContext(expression, context, fromExpressionAndContextToEvaluator)
        );
    }

    private static ArrayTableFactor arrayTableFactorFromModelAndContext(
            Expression expression,
            Context context,
            FromExpressionAndContextToDiscreteExpressionEvaluatorOnVariablesInOccurrenceOrder evaluatorMaker) {

        var grounder = new ExpressionToArrayTableFactorGrounder(e -> evaluatorMaker.apply(e, context), context);
        var actualFactor = grounder.ground(expression);
        return actualFactor;
    }

    @NotNull
    private static Pair<Expression, HOGMExpressionBasedModel> getExpressionAndHOGModel(
            String expressionString,
            Map<String, Integer> variableDefinitions) {

        var expression = parse(expressionString);
        var hogModel = makeModel(variableDefinitions);
        return pair(expression, hogModel);
    }

    private static HOGMExpressionBasedModel makeModel(Map<String, Integer> variableDefinitions) {
        var definitionsString = mapIntoList(variableDefinitions.entrySet(), e -> fromMapEntryToString(e));
        var hogModelString = join("; ", definitionsString);
        var hogModel = new HOGMExpressionBasedModel(hogModelString);
        return hogModel;
    }

    private static String fromMapEntryToString(Map.Entry<String, Integer> entry) {
        return "random " + entry.getKey() + ": 0.." + (entry.getValue() - 1);
    }

}
