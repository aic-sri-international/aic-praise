package com.sri.ai.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.freeVariables;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIATION;
import static com.sri.ai.grinder.library.FunctorConstants.LOG;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.applySigmoidTrick;
import static org.apache.commons.math3.util.FastMath.exp;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.optimization.OptimizationWithNonlinearConjugateGradientDescent;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.exactbp.ExactBPExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.util.base.Pair;

/**
 * Parameter Estimation for ExpressionBasedModel when queries and evidences.
 * 
 * @author Sarah Perrin
 *
 */

public class ParameterEstimationWithQueriesForExpressionBasedModel implements ParameterEstimation {

	public ExpressionBasedModel model;
	public List<Pair<Expression, Expression>> pairsQueryEvidence;

	public ParameterEstimationWithQueriesForExpressionBasedModel(ExpressionBasedModel model, List<Pair<Expression, Expression>> pairsQueryEvidence) {
		this.model = model;
		this.pairsQueryEvidence = pairsQueryEvidence;
	}

	/**
	 * Main method to optimize the parameters of the model given the queries and
	 * evidences when the model is Expression based.
	 *
	 */
	public HashMap<Expression, Double> optimize(ExpressionBasedModel expressionBasedModel,
			GoalType goalType, double[] startPoint) {

		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);

		Expression marginalFunctionLog = buildListOfMarginals();

		System.out.println(marginalFunctionLog);

		Set<Expression> listOfVariables = freeVariables(marginalFunctionLog, context);

		if(listOfVariables.size() != startPoint.length) {
			throw new Error("Length of double[] startPoint doesn't match the number of variables to optimize with evidence");
		}

		double[] argmax = returnArgopt(startPoint, goalType, marginalFunctionLog);

		HashMap<Expression, Double> result = buildHashMapContainingResult(listOfVariables, argmax);

		return result;
	}

	/**
	 * Calculate the arg of the optimum and apply the sigmoid operation to have the
	 * result between 0 and 1.
	 *
	 */
	private double[] returnArgopt(double[] startPoint, GoalType goalType, Expression marginalFunctionLog) {
		OptimizationWithNonlinearConjugateGradientDescent optimizer = new OptimizationWithNonlinearConjugateGradientDescent(
				marginalFunctionLog, goalType, startPoint);
		double[] resultSig = optimizer.findArgopt();
		double[] result = new double[resultSig.length];
		for (int i = 0; i < resultSig.length; i++) {
			result[i] = 1 / (1 + exp(-resultSig[i]));
		}
		return result;
	}

	/**
	 * Build the map to have each variable associated with its optimum.
	 *
	 */
	private HashMap<Expression, Double> buildHashMapContainingResult(Set<Expression> listOfVariables, double[] argopt) {
		HashMap<Expression, Double> result = new HashMap<Expression, Double>();
		int k = 0;
		for (Expression e : listOfVariables) {
			result.put(e, argopt[k]);
			k++;
		}
		return result;
	}

	/**
	 * Method to calculate the marginal for each query and evidence and return the
	 * list of marginals.
	 *
	 */
	private Expression buildListOfMarginals() {

		List<Expression> listOfMarginals = new LinkedList<Expression>();
		Map<Pair<Expression, Expression>, Integer> mapPairsToCount = buildMapToCountPairs();
		
		for (Entry<Pair<Expression, Expression>, Integer> entry : mapPairsToCount.entrySet()) {
			Pair<Expression, Expression> pairEvidenceQuery = entry.getKey();
			ExpressionBasedSolver solver = new ExactBPExpressionBasedSolver();
			Expression query = pairEvidenceQuery.first;
			Expression evidence = pairEvidenceQuery.second;
			ExpressionBasedModel conditionedModel = model.getConditionedModel(evidence);
			Expression marginal = solver.solve(query, conditionedModel);

			Context context = model.getContext();
			

			System.out.println(marginal);

			Expression marginalFunction = convertExpression(marginal);
			
			marginalFunction = applySigmoidTrick(marginalFunction, context);
			
			Expression marginalFunctionExponentiate = apply(EXPONENTIATION, marginalFunction, entry.getValue());

			System.out.println(marginalFunctionExponentiate);

			listOfMarginals.add(marginalFunctionExponentiate);
		}
		
		Expression result;
		if (listOfMarginals.size() > 1) {
			result = apply(PLUS, listOfMarginals);
			for (int i = 0; i < listOfMarginals.size(); i++) {
				Expression ithExpression = listOfMarginals.get(i);
				Expression newIthArgument = apply(FunctorConstants.LOG, ithExpression);
				result = result.set(i, newIthArgument);
			}
		} else {
			result = apply(LOG, listOfMarginals.get(0));
		}

		return result;
	}

	/**
	 * Method to have the right form of Expression for the optimization. For
	 * example, convert "if earthquake then Alpha else 1-Alpha" into "Alpha".
	 *
	 */
	public Expression convertExpression(Expression marginal) {
		try {
			String input = marginal.toString();
			input = input.substring(input.lastIndexOf("then") + 5);
			String resultString = input.split("else")[0];
			Expression result = parse(resultString);
			return result;
		}
		catch(Exception e){
			System.out.println("Impossible to convert the marginal : " + marginal);
			return marginal;
		}
	}
	
	private Map<Pair<Expression, Expression>, Integer> buildMapToCountPairs(){
		Map<Pair<Expression, Expression>, Integer> result = new HashMap<Pair<Expression, Expression>, Integer>();
		for(Pair<Expression, Expression> pair : pairsQueryEvidence) {
			if(!result.containsKey(pair)) {
				result.put(pair, 1);
			}
			else {
				System.out.println("ok");
				int currentCount = result.get(pair);
				currentCount++;
				result.put(pair, currentCount);
			}
		}
		System.out.println(result.toString());
		return result;
	}

}



