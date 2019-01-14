package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.adaptinginterfacebasedsolver;

import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.AbstractExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.Solver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;

public abstract class SolverToExpressionBasedSolverAdapter extends AbstractExpressionBasedSolver {

	private Function<ExpressionBasedProblem, Problem> fromExpressionBasedProblemToProblem;
	private Solver solver;

	public SolverToExpressionBasedSolverAdapter(Function<ExpressionBasedProblem, Problem> fromExpressionBasedProblemToProblem, Solver solver) {
		super();
		this.solver = solver;
		this.fromExpressionBasedProblemToProblem = fromExpressionBasedProblemToProblem;
	}

	@Override
	protected Expression solveForQuerySymbolDefinedByExpressionBasedProblem(ExpressionBasedProblem expressionBasedProblem) {
		Problem problem = fromExpressionBasedProblemToProblem.apply(expressionBasedProblem);
		Expression result = getSolver().solve(problem);
		return result;
	}

	private Solver getSolver() {
		return solver;
	}

	@Override
	public void interrupt() {
		solver.interrupt();
	}

}