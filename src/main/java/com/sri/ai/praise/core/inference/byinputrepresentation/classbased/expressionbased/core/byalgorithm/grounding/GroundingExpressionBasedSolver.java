package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionWithProbabilityFunction;

/**
 * An {@link ExpressionBasedSolver} specific for possibly relational {@link HOGMExpressionBasedProblem}s.
 * <p>
 * This solver is constructed based on a given {@link ExpressionBasedSolver} for grounded 
 * {@link HOGMExpressionBasedProblem}s
 * and returns a lazy {@link ExpressionWithProbabilityFunction}
 * that corresponds to a if-then-else expression on the relational query's free variables.
 * This lazy expression, of class {@link RelationalQuerySolutionExpression},
 * provides methods for providing the solution to the problem given an assignment to the query's free variables,
 * as well as for providing a {@link Function} representing its probability function,
 * without having to compute all such problems.
 * 
 * @author braz
 *
 */
public class GroundingExpressionBasedSolver implements ExpressionBasedSolver {

	//////////////
	
	private ExpressionBasedSolver expressionBasedSolver;
	
	//////////////
	
	public GroundingExpressionBasedSolver(ExpressionBasedSolver expressionBasedSolver) {
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
		RelationalQuerySolutionExpression result = new RelationalQuerySolutionExpression(problem, expressionBasedSolver);
		return result;
	}

}
