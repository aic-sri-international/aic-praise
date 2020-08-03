package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.AbstractExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.evaluatormaker.DiscreteExpressionEvaluatorMaker;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.evaluatormaker.SizeConditionalDiscreteExpressionEvaluatorMaker;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelReducer;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelReduction;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromcategoricaltointeger.CategoricalIntegerReducer;
import com.sri.ai.util.Timer;
import com.sri.ai.util.base.BinaryFunction;

import java.util.ArrayList;
import java.util.function.Function;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Timer.timed;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;

public class GroundingExpressionBasedSolver extends AbstractExpressionBasedSolver {

    private static final DiscreteExpressionEvaluatorMaker evaluatorMaker =
            new SizeConditionalDiscreteExpressionEvaluatorMaker();

    private static final BinaryFunction<Variable, FactorNetwork, Factor> solver =
            new VariableEliminationSolver();

    private static final ExpressionBasedModelReducer normalizer = new CategoricalIntegerReducer();

    @Override
    public void interrupt() {
        // TODO Move to abstract level
    }

    @Override
    protected Expression solveForQuerySymbolDefinedByExpressionBasedProblem(ExpressionBasedProblem problem) {
        var reduction = normalizer.invoke(problem.getOriginalExpressionBasedModel());
        var normalizedProblem = makeNormalizedProblem(reduction, problem);
        var normalizedSolution = solveForQuerySymbolDefinedByNormalizedExpressionBasedProblem(normalizedProblem);
        var solution = reduction.translateBack(normalizedSolution);
        return solution;
    }

    private ExpressionBasedProblem makeNormalizedProblem(
            ExpressionBasedModelReduction reduction,
            ExpressionBasedProblem problem) {

        var translatedExpressionBasedModel = reduction.getTranslation();
        var normalizedProblem =
                new DefaultExpressionBasedProblem(
                        problem.getQueryExpression(),
                        translatedExpressionBasedModel);
        return normalizedProblem;
    }

    private Expression solveForQuerySymbolDefinedByNormalizedExpressionBasedProblem(ExpressionBasedProblem problem) {
        var factorGrounder = new ExpressionToArrayTableFactorGrounder(evaluatorMaker, problem.getContext());
        var groundedFactorNetwork = timed(() -> makeGroundedFactorNetwork(e -> factorGrounder.ground(e), problem));
        var queryVariable = TableVariableMaker.makeTableVariable(problem.getQuerySymbol(), problem.getContext());
        var solutionFactor = timed(() -> solver.apply(queryVariable, groundedFactorNetwork.first));
        var normalizedSolutionExpression = timed(() -> makeSolutionExpression(solutionFactor.first, problem));
        println("Time for grounding      : ", Timer.timeStringInSeconds(groundedFactorNetwork, 3));
        println("Time for solving        : ", Timer.timeStringInSeconds(solutionFactor, 3));
        println("Time for converting back: ", Timer.timeStringInSeconds(normalizedSolutionExpression, 3));
        return normalizedSolutionExpression.first;
    }

    private FactorNetwork makeGroundedFactorNetwork(
            Function<Expression, ArrayTableFactor> grounder,
            ExpressionBasedProblem problem) {

        var factorExpressions = problem.getFactorExpressionsIncludingQueryDefinitionIfAny();
        var tables = mapIntoList(factorExpressions, grounder::apply);
        var groundedFactorNetwork = new DefaultFactorNetwork(tables);
        return groundedFactorNetwork;
    }

    private Expression makeSolutionExpression(Factor factor, ExpressionBasedProblem problem) {
        if (factor instanceof ArrayTableFactor) {
            var table = (ArrayTableFactor) factor;
            return makeSolutionExpressionForArrayTableFactor(table, problem);
        } else {
            return makeUniformSolutionExpression(problem);
        }
    }

    private Expression makeSolutionExpressionForArrayTableFactor(ArrayTableFactor table, ExpressionBasedProblem problem) {
        var probabilitiesTable = table.normalize();
        var query = problem.getQuerySymbol();
        var probabilities = probabilitiesTable.getEntries();
        var index = probabilitiesTable.numberOfEntries() - 1;
        var distributionFromIndex = probabilityExpression(probabilities, index);
        for (index = index - 1; index != -1; index--) {
            distributionFromIndex = update(distributionFromIndex, index, query, probabilities);
        }
        return distributionFromIndex;
    }

    private Expression update(Expression distributionFromIndex, int index, Expression query, ArrayList<Double> entries) {
        var distributionFromIndexPlusOne = distributionFromIndex;
        var queryEqualsIndex = equality(query, index);
        var probabilityForIndex = probabilityExpression(entries, index);
        distributionFromIndex = IfThenElse.make(queryEqualsIndex, probabilityForIndex, distributionFromIndexPlusOne);
        return distributionFromIndex;
    }

    private Expression makeUniformSolutionExpression(ExpressionBasedProblem problem) {
        var context = problem.getContext();
        var querySymbol = problem.getQuerySymbol();
        var queryCardinality = context.getCardinalityOfIntegerIntervalTypedRegisteredSymbol(querySymbol);
        return makeSymbol(1 / queryCardinality);
    }

    private Expression equality(Expression query, int index) {
        return Equality.make(query, makeSymbol(index));
    }

    private Expression probabilityExpression(ArrayList<Double> probabilities, int index) {
        return makeSymbol(probabilities.get(index));
    }

}
