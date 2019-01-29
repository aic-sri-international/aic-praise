package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem.ground;
import static com.sri.ai.util.Util.assertType;
import static com.sri.ai.util.Util.mapValues;

import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.interpreter.Assignment;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedProblem;

public abstract class AbstractGroundingExpressionBasedSolver implements ExpressionBasedSolver {

	abstract protected Expression combine(Map<Assignment, Expression> fromAssignmentToQueryFreeVariablesToGroundSolution);
	
	//////////////
	
	private ExpressionBasedSolver expressionBasedSolver;
	
	//////////////
	
	public AbstractGroundingExpressionBasedSolver(ExpressionBasedSolver expressionBasedSolver) {
		this.expressionBasedSolver = expressionBasedSolver;
	}

	//////////////
	
	@Override
	public void interrupt() {
		expressionBasedSolver.interrupt();
	}

	//////////////
	
	@Override
	public Expression solve(ExpressionBasedProblem problem) {
		HOGMExpressionBasedProblem hogmProblem = assertType(problem, HOGMExpressionBasedProblem.class, getClass());
		Expression result = combine(solve(ground(hogmProblem)));
		return result;
	}

	private Map<Assignment, Expression>  solve(Map<Assignment, HOGMExpressionBasedProblem> groundProblems) {
		return mapValues(groundProblems, expressionBasedSolver::solve);
	}

}
