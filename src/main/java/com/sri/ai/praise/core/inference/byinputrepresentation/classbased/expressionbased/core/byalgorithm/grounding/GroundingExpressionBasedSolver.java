package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Timer.getResultAndTime;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.interpreter.FastInterpreter;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.AbstractExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.variableelimination.VariableEliminationSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.Timer;
import com.sri.ai.util.base.BinaryFunction;

public class GroundingExpressionBasedSolver extends AbstractExpressionBasedSolver {

	private static final BinaryFunction<Expression, Context, Expression> interpreter = new FastInterpreter();
	
	private static final BinaryFunction<Variable, FactorNetwork, Factor> solver = new VariableEliminationSolver();
	
	@Override
	public void interrupt() {
		// TODO Move to abstract level
	}

	@Override
	protected Expression solveForQuerySymbolDefinedByExpressionBasedProblem(ExpressionBasedProblem problem) {
		var factorGrounder = new ExpressionToArrayTableFactorGrounder(interpreter, problem.getContext());
		var groundedFactorNetwork = getResultAndTime(() -> makeGroundedFactorNetwork(factorGrounder, problem));
		var queryVariable = factorGrounder.makeTableVariable(problem.getQuerySymbol());
		var solutionFactor = getResultAndTime(() -> solver.apply(queryVariable, groundedFactorNetwork.first));
		var solutionExpression = getResultAndTime(() -> makeSolutionExpression(queryVariable, solutionFactor.first, problem));
		println("Time for grounding      : ", Timer.timeStringInSeconds(groundedFactorNetwork, 3));
		println("Time for solving        : ", Timer.timeStringInSeconds(solutionFactor, 3));
		println("Time for converting back: ", Timer.timeStringInSeconds(solutionExpression, 3));
		return solutionExpression.first;
	}

	private FactorNetwork makeGroundedFactorNetwork(
			ExpressionToArrayTableFactorGrounder factorGrounder,
			ExpressionBasedProblem problem) {
		
		var factorExpressions = problem.getFactorExpressionsIncludingQueryDefinitionIfAny();
		var tables = mapIntoList(factorExpressions, factorGrounder::ground);
		var groundedFactorNetwork = new DefaultFactorNetwork(tables);
		return groundedFactorNetwork;
	}

	private Expression makeSolutionExpression(TableVariable queryVariable, Factor factor, ExpressionBasedProblem problem) {
		if (factor instanceof ArrayTableFactor) {
			var table = (ArrayTableFactor) factor;
			return makeSolutionExpressionForArrayTableFactor(queryVariable, table, problem);
		}
		else {
			return makeUniformSolutionExpression(problem);
		}
	}

	private Expression makeSolutionExpressionForArrayTableFactor(TableVariable queryVariable, ArrayTableFactor table, ExpressionBasedProblem problem) {
		var probabilitiesTable = table.normalize();
		var query = problem.getQuerySymbol();
		var probabilities = probabilitiesTable.getEntries();
		var index = probabilitiesTable.numberOfEntries() - 1;
		var distributionFromIndex = probabilityExpression(probabilities, index);
		for (index = index - 1 ; index != -1; index--) {
			distributionFromIndex = update(distributionFromIndex, index, query, probabilities);
		}
		var distributionForAllValues = distributionFromIndex;
		return distributionForAllValues;
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
		return makeSymbol(1/queryCardinality);
	}

	private Expression equality(Expression query, int index) {
		return Equality.make(query, makeSymbol(index));
	}

	private Expression probabilityExpression(ArrayList<Double> probabilities, int index) {
		return makeSymbol(probabilities.get(index));
	}

}
