package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.adaptinginterfacebasedsolver;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.AbstractExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.Solver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.ExpressionBasedProblemToExpressionInterfaceBasedProblemConversion;

public abstract class SolverToExpressionBasedSolverAdapter extends AbstractExpressionBasedSolver {

	private Solver solver;

	public SolverToExpressionBasedSolverAdapter(Solver solver) {
		super();
		this.solver = solver;
	}

	@Override
	protected Expression solveForQuerySymbolDefinedByExpressionBasedProblem(ExpressionBasedProblem expressionBasedProblem) {
		Problem problem = ExpressionBasedProblemToExpressionInterfaceBasedProblemConversion.translate(expressionBasedProblem);
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