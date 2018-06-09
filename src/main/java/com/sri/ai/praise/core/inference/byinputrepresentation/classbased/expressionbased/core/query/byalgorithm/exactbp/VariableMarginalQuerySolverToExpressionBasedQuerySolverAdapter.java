package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.query.byalgorithm.exactbp;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.query.adapter.AbstractExpressionBasedQuerySolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.VariableMarginalQuerySolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedQuery;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.VariableMarginalQuery;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.ExpressionBasedQueryToVariableMarginalQueryTranslator;

public abstract class VariableMarginalQuerySolverToExpressionBasedQuerySolverAdapter extends AbstractExpressionBasedQuerySolver {

	private VariableMarginalQuerySolver variableMarginalQuerySolver;

	public VariableMarginalQuerySolverToExpressionBasedQuerySolverAdapter(VariableMarginalQuerySolver variableMarginalQuerySolver) {
		super();
		this.variableMarginalQuerySolver = variableMarginalQuerySolver;
	}

	@Override
	protected Expression computeNormalizedMarginal(ExpressionBasedQuery query) {
		VariableMarginalQuery variableMarginalQuerySolver = ExpressionBasedQueryToVariableMarginalQueryTranslator.convert(query);
		VariableMarginalQuerySolver solver = getVariableMarginalQuerySolver();
		Expression result = solver.solve(variableMarginalQuerySolver);
		return result;
	}

	private VariableMarginalQuerySolver getVariableMarginalQuerySolver() {
		return variableMarginalQuerySolver;
	}

	@Override
	public void interrupt() {
		variableMarginalQuerySolver.interrupt();
	}

}