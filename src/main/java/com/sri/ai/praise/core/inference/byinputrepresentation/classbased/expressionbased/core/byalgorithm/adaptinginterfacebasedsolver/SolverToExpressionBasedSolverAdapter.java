package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.adaptinginterfacebasedsolver;

import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.AbstractExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.Solver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;

public abstract class SolverToExpressionBasedSolverAdapter extends AbstractExpressionBasedSolver {

	private Function<ExpressionBasedProblem, Problem> fromExpressionBasedProblemToProblem;
	private Function<ExpressionBasedProblem, Solver> fromExpressionBasedProblemToSolver;
	private Solver solver;
	
	public SolverToExpressionBasedSolverAdapter(
			Function<ExpressionBasedProblem, Problem> fromExpressionBasedProblemToProblem, 
			Function<ExpressionBasedProblem, Solver> fromExpressionBasedProblemToSolver) {
		super();
		this.fromExpressionBasedProblemToSolver = fromExpressionBasedProblemToSolver;
		this.fromExpressionBasedProblemToProblem = fromExpressionBasedProblemToProblem;
	}

	@Override
	protected Expression solveForQuerySymbolDefinedByExpressionBasedProblem(ExpressionBasedProblem expressionBasedProblem) {
		Problem problem = fromExpressionBasedProblemToProblem.apply(expressionBasedProblem);
		solver = fromExpressionBasedProblemToSolver.apply(expressionBasedProblem);
		Expression result = solver.solve(problem);
		return result;
	}

	@Override
	public void interrupt() {
		solver.interrupt();
	}

}