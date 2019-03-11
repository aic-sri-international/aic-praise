package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.getIndices;
import static com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding.VariableMaskingExpressionWithProbabilityFunction.mask;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.VariableExpressionDiscretization.makeSetOfVariablesWithRanges;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.RelationalHOGMExpressionBasedModelGrounder.getIndexExpressionsSet;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.RelationalHOGMExpressionBasedModelGrounder.getQueryBody;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.RelationalHOGMExpressionBasedModelGrounder.getQueryIndexExpressionsSet;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.RelationalHOGMExpressionBasedModelGrounder.makeRelationalExpressionFromGroundedVariable;
import static com.sri.ai.util.Util.assertType;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.LazyIfThenElse;
import com.sri.ai.grinder.interpreter.Assignment;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedProblem;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.VariableExpressionDiscretization;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionWithProbabilityFunction;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.NonBooleanFactorError;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.RelationalHOGMExpressionBasedModelGrounder;
import com.sri.ai.util.Util;
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

	private ExpressionBasedProblem problem;
	private ExpressionBasedSolver solver;
	
	public RelationalQuerySolutionExpression(ExpressionBasedProblem problem, ExpressionBasedSolver baseSolver) {
		super(makeKeys(problem), makeSubExpressionMaker(problem, baseSolver), makeContext(problem));
		this.problem = problem;
		this.solver = baseSolver;
	}

	///////////////////// 
	
	public ExpressionBasedProblem getProblem() {
		return problem;
	}

	public ExpressionBasedSolver getSolver() {
		return solver;
	}

	///////////////////// STATIC METHODS PROVIDING ARGUMENTS FOR SUPER CONSTRUCTOR
	
	private static Collection<? extends Expression> makeKeys(ExpressionBasedProblem problem) {
		
		// The "keys" in this case are the query free variables
		
		IndexExpressionsSet indexExpressionsSet = 
				getIndexExpressionsSet(problem.getQueryExpression(), makeContext(problem));
		
		List<Expression> result = 
				getIndices(indexExpressionsSet);
		
		return result;
	}

	private static Function<Assignment, Expression> makeSubExpressionMaker(ExpressionBasedProblem problem, ExpressionBasedSolver baseSolver) {
		// the sub-expression makers takes an assignment to the keys and makes the corresponding then branch
		// Here, this means solving the grounded problems to the query grounded with the given assignment
		ExpressionBasedModel originalExpressionBasedModel = problem.getOriginalExpressionBasedModel();
		HOGMExpressionBasedModel hogModel = assertType(originalExpressionBasedModel, HOGMExpressionBasedModel.class, RelationalQuerySolutionExpression.class);
		RelationalHOGMExpressionBasedModelGrounder converter = new RelationalHOGMExpressionBasedModelGrounder(hogModel);
		return assignment -> solveSubProblem(problem, baseSolver, converter, assignment);
	}

	private static Expression solveSubProblem(
			ExpressionBasedProblem problem, 
			ExpressionBasedSolver baseSolver,
			RelationalHOGMExpressionBasedModelGrounder converter, 
			Assignment assignment) {
		
		Expression queryBody = getQueryBody(problem);

		Expression queryBodyWithoutStatistic = getQueryBodyWithoutStatistic(queryBody);
		
		ExpressionWithProbabilityFunction masked = 
				solveQueryBodyWithoutStatistic(
						queryBodyWithoutStatistic, 
						assignment,
						converter, 
						baseSolver, 
						makeContext(problem));
		
		Expression result = wrapResultInStatisticExpressionWithFunction(problem, queryBody, masked);
		
		return result;
	}

	public static Expression getQueryBodyWithoutStatistic(Expression queryBody) {
		Expression queryBodyWithoutStatistic;
		if (queryBody.hasFunctor("mean")) {
			queryBodyWithoutStatistic = queryBody.get(0);
		}
		else {
			queryBodyWithoutStatistic = queryBody;
		}
		return queryBodyWithoutStatistic;
	}

	public static Expression wrapResultInStatisticExpressionWithFunction(
			ExpressionBasedProblem problem,
			Expression queryBody, 
			ExpressionWithProbabilityFunction masked) {
		
		Expression result;
		if (queryBody.hasFunctor("mean")) {
			result = new MeanExpressionWithFunction(masked);
		}
		else {
			result = masked;
		}
		return result;
	}

	public static ExpressionWithProbabilityFunction solveQueryBodyWithoutStatistic(Expression queryBodyWithoutStatistic,
			Assignment assignment, RelationalHOGMExpressionBasedModelGrounder converter,
			ExpressionBasedSolver baseSolver, Context context) throws NonBooleanFactorError {
		Expression groundQuery = converter.groundNonQuantifiedExpressionWithAssignment(queryBodyWithoutStatistic, assignment, context);
		ExpressionBasedProblem groundedProblem = new HOGMExpressionBasedProblem(groundQuery, converter.getGroundedModel());
		Expression baseSolution = solveWithErrorLifting(baseSolver, groundedProblem);
		ExpressionWithProbabilityFunction masked = maskWithRelationalQueryVariable(queryBodyWithoutStatistic, groundQuery, baseSolution);
		return masked;
	}

	public static Expression solveWithErrorLifting(ExpressionBasedSolver baseSolver, ExpressionBasedProblem groundedProblem) throws NonBooleanFactorError {
		Expression baseSolution;
		try {
			baseSolution = baseSolver.solve(groundedProblem);
		}
		catch (NonBooleanFactorError nonBooleanFactorError) {
			Expression lifted = makeRelationalExpressionFromGroundedVariable(nonBooleanFactorError.getFactor());
			throw new NonBooleanFactorError(lifted, nonBooleanFactorError.getReason());
			// TODO this could be improved by having the converter keep track of the relational factor from which each
			// factor came and showing the original lifted factor instead.
		}
		return baseSolution;
	}

	private static ExpressionWithProbabilityFunction maskWithRelationalQueryVariable(Expression queryBody, Expression groundQuery, Expression baseSolution) {
		ExpressionWithProbabilityFunction baseSolutionWithProbabilityFunction = (ExpressionWithProbabilityFunction) baseSolution;
		ExpressionWithProbabilityFunction masked = mask(baseSolutionWithProbabilityFunction, groundQuery, queryBody);

		return masked;
		// TODO: this is a hack.
		// The main problem here is that it is assuming the base solver is returning a ExpressionWithProbabilityFunction,
		// which it might be not.
		// Ideally, we would use Expression's replace method and it would take of everything,
		// but this would require copying constructors all the way down the hierarchy, which I don't have time for right now.
		// Besides, it would be good to have more systematic data structures so that all these copy constructors could
		// be implemented at an abstract level acting on on a common data structure
		// instead of having to write them for all classes.
	}
	
	private static Context makeContext(ExpressionBasedProblem problem) {
		Context contextWithQueryIndices = problem.getContext().extendWith(getQueryIndexExpressionsSet(problem)); 
		return contextWithQueryIndices;
	}

	/////////////////////

	@Override
	public int getDiscretizedConditionalProbabilityDistributionFunctionQueryIndex() {
		Expression queryBody = getQueryBody(problem);
		AggregatorFunction aggregator = getDiscretizedConditionalProbabilityDistributionFunction();
		int result;
		if (queryBody.hasFunctor("mean")) {
			result = aggregator.getSetOfInputVariables().size() != 0? 0 : -1;
			// TODO: here we get the first index of a lifted query (if any) as the new query.
			// However, any of the indices of lifted query would work, so we want to offer that possibility
			// somehow at some point, either at this point, or just passing the information along
			// as to what indices are eligible.
		}
		else {
			result = Util.getIndexOfFirstSatisfyingPredicateOrMinusOne(aggregator.getSetOfInputVariables().getVariables(), v -> v.getName().equals(queryBody.toString()));
		}
		return result;
	}
	
	private AggregatorFunction aggregatorFunction;
	
	@Override
	public AggregatorFunction getDiscretizedConditionalProbabilityDistributionFunction() {
		if (aggregatorFunction == null) {
			Collection<? extends Expression> queryFreeVariables = getKeys();
			
			SetOfVariables setOfVariablesForQueryFreeVariables = 
					makeSetOfVariablesWithRanges(queryFreeVariables, v -> 25, getContext());
			
			aggregatorFunction = 
					new AggregatorFunction(
							setOfVariablesForQueryFreeVariables,
							assignment -> computeDiscretizedFunctionForSubSolution(assignment));
		}
		return aggregatorFunction;
	}

	private com.sri.ai.util.function.api.functions.Function computeDiscretizedFunctionForSubSolution(
			com.sri.ai.util.function.api.variables.Assignment assignment) {
		
		Assignment expressoAssignment = VariableExpressionDiscretization.fromFunctionAssignmentToExpressoAssigment(assignment);
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
