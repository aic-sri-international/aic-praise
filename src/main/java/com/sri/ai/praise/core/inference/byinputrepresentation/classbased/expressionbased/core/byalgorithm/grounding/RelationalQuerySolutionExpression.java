package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.getIndices;
import static com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.VariableMaskingExpressionWithProbabilityFunction.mask;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionDiscretization.makeSetOfVariablesWithRanges;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem.getIndexExpressionsSet;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem.ground;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem.groundNonQuantifiedExpressionWithAssignment;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem.makeRelationalExpressionFromGroundedVariable;
import static com.sri.ai.util.Util.assertType;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.grinder.helper.LazyIfThenElse;
import com.sri.ai.grinder.interpreter.Assignment;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionDiscretization;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionWithProbabilityFunction;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem;
import com.sri.ai.util.distribution.DiscretizedConditionalProbabilityDistributionFunction;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.core.functions.AggregatorFunction;

/**
 * A {@link ExpressionWithProbabilityFunction}
 * that serves as the solution from a {@link GroundingExpressionBasedSolver}.
 * It takes the following construction arguments:
 * <ul>
 * <li> a possibly relational {@link ExpressionBasedProblem} whose model is a {@link HOGMExpressionBasedModel},
 *      which includes a possibly relational (that is, with free variables <code>Q</code>) query
 * <li> a base {@link ExpressionBasedSolver} for solving grounded problems.
 * </ul>
 * <p>
 * When asked to provide a {@link DiscretizedConditionalProbabilityDistributionFunction},
 * this returns a function on variables <code>Q union Var(Result(Q))</code>,
 * where <code>Var(Result(Q))</code> is the set of variables
 * in the query result <code>Result(Q)</code> obtained for 
 * the grounded model and query grounded with the assignment <code>Q</code>.
 * This function (lazily) maps an assignment to variables <code>Q union Var(Result(Q))</code> to <code>Result(Q)</code>.
 * <p>
 * When asked to act like an {@link Expression}, this is an if then else expression from
 * all assignments on <code>Q</code> to the solutions <code>Result(Q)</code>. 
 * <p>
 * @author braz
 *
 */
public class RelationalQuerySolutionExpression extends LazyIfThenElse implements ExpressionWithProbabilityFunction {

	private static final long serialVersionUID = 1L;
	
	public RelationalQuerySolutionExpression(ExpressionBasedProblem problem, ExpressionBasedSolver baseSolver) {
		super(makeKeys(problem), makeSubExpressionMaker(problem, baseSolver), problem.getContext());
	}

	///////////////////// STATIC METHODS PROVIDING ARGUMENTS FOR SUPER CONSTRUCTOR
	
	private static Collection<? extends Expression> makeKeys(ExpressionBasedProblem problem) {
		
		// The "keys" in this case are the query free variables
		
		IndexExpressionsSet indexExpressionsSet = 
				getIndexExpressionsSet(problem.getQueryExpression(), problem.getContext());
		
		List<Expression> result = 
				getIndices(indexExpressionsSet);
		
		return result;
	}

	private static Function<Assignment, Expression> makeSubExpressionMaker(ExpressionBasedProblem problem, ExpressionBasedSolver baseSolver) {
		// the sub-expression makers takes an assignment to the keys and makes the corresponding then branch
		// Here, this means solving the grounded problems to the query grounded with the given assignment
		ExpressionBasedModel originalExpressionBasedModel = problem.getOriginalExpressionBasedModel();
		HOGMExpressionBasedModel hogModel = assertType(originalExpressionBasedModel, HOGMExpressionBasedModel.class, RelationalQuerySolutionExpression.class); 
		HOGMExpressionBasedModel groundedModel = ground(hogModel);
		Expression queryBody = FromRelationalToGroundHOGMExpressionBasedProblem.getQueryBody(problem);
		return assignment -> solveSubProblem(problem, baseSolver, groundedModel, queryBody, assignment);
	}

	private static Expression solveSubProblem(
			ExpressionBasedProblem problem, 
			ExpressionBasedSolver baseSolver,
			HOGMExpressionBasedModel groundedModel, 
			Expression queryBody, 
			Assignment assignment) {
		
		Expression groundQuery = groundNonQuantifiedExpressionWithAssignment(queryBody, assignment, problem.getContext());
		ExpressionBasedProblem groundedProblem = new HOGMExpressionBasedProblem(groundQuery, groundedModel);
		Expression baseSolution = baseSolver.solve(groundedProblem);
		VariableMaskingExpressionWithProbabilityFunction masked = maskWithRelationalQueryVariable(groundQuery, baseSolution);
		return masked;
	}

	private static VariableMaskingExpressionWithProbabilityFunction maskWithRelationalQueryVariable(Expression groundQuery, Expression baseSolution) {
		Expression liftedQueryVariable = makeRelationalExpressionFromGroundedVariable(groundQuery);
		ExpressionWithProbabilityFunction baseSolutionWithProbabilityFunction = (ExpressionWithProbabilityFunction) baseSolution;
		VariableMaskingExpressionWithProbabilityFunction masked = mask(baseSolutionWithProbabilityFunction, groundQuery, liftedQueryVariable);
		return masked;
		// TODO: this is a hack.
		// The main problem here is that it is assuming the base solver is returning a ExpressionWithProbabilityFunction,
		// which it might be not.
		// Ideally, we would use Expression's replace method and it would take of everything,
		// but this would require copying constructors all the way down the hierarchy, which I don't have time for right now.
		// Besides, it would be good to have more systematic data structures so that all these copy constructors could act
		// on a common data structure instead of having to write them for all classes.
	}
	
	/////////////////////
	
	@Override
	public com.sri.ai.util.function.api.functions.Function getDiscretizedConditionalProbabilityDistributionFunction() {
		
		Collection<? extends Expression> queryFreeVariables = getKeys();
		
		SetOfVariables setOfVariablesForQueryFreeVariables = 
				makeSetOfVariablesWithRanges(queryFreeVariables, v -> 25, getContext());
		
		AggregatorFunction aggregatorFunction = 
				new AggregatorFunction(
						setOfVariablesForQueryFreeVariables,
						assignment -> computeDiscretizedFunctionForSubSolution(assignment));
		
		return aggregatorFunction;
	}

	private com.sri.ai.util.function.api.functions.Function computeDiscretizedFunctionForSubSolution(
			com.sri.ai.util.function.api.variables.Assignment assignment) {
		
		Assignment expressoAssignment = ExpressionDiscretization.fromFunctionAssignmentToExpressoAssigment(assignment);
		ExpressionWithProbabilityFunction solutionWithFunction = getSolutionForSubProblemWithFunction(expressoAssignment);
		com.sri.ai.util.function.api.functions.Function function = solutionWithFunction.getDiscretizedConditionalProbabilityDistributionFunction();
		return function;
	}

	private 
	ExpressionWithProbabilityFunction getSolutionForSubProblemWithFunction(Assignment expressoAssignment) {
		Expression solution = getSubExpression(expressoAssignment); // using super's getSubExpression ensures caching
		return (ExpressionWithProbabilityFunction) solution;
	}
	
}
