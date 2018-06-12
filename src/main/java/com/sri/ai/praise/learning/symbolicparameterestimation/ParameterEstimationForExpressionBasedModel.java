package com.sri.ai.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.freeVariables;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIAL;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static java.util.Arrays.asList;
import static org.apache.commons.math3.util.FastMath.exp;

import java.util.HashMap;
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
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.HOGModelToExpressionBasedModel;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.ModelStringToHOGModel;

/**
 * Parameter Estimation.
 * 
 * @author Sarah Perrin
 *
 */

public class ParameterEstimationForExpressionBasedModel {

	static ExpressionBasedModel expressionBasedModel; 
	static HOGModel hogmModel;
	static String modelString;
	
	static Expression[] evidence;
	static Expression[] queryExpression;
	
	/**
	 * Main method to optimize the parameters of the model given the queries and evidences when the model is String based.
	 *
	 */
	public static HashMap<Expression, Double> optimizeWhenModelIsString(String modelString, Expression[] evidence, Expression[] queryExpression, GoalType goalType,
			double[] startPoint){
		
		HOGModel hogmModel = ModelStringToHOGModel.parseModelStringToHOGMModel(modelString);
		
		HashMap<Expression, Double> result = optimizeWhenModelIsHOGModel(
			hogmModel,
			queryExpression,
			evidence,
			goalType,
			startPoint);
		
		return result;
	}
	
	/**
	 * Main method to optimize the parameters of the model given the queries and evidences when the model is HOGModel based.
	 *
	 */
	public static HashMap<Expression, Double> optimizeWhenModelIsHOGModel(HOGModel hogmModel, Expression[] evidence, Expression[] queryExpression, GoalType goalType,
			double[] startPoint){
		
		ExpressionBasedModel expressionBasedModel = HOGModelToExpressionBasedModel.parseModelStringToHOGMModel(hogmModel);
		
		HashMap<Expression, Double> result = optimizeWhenModelIsExpressionBased(
			queryExpression,
			evidence,
			expressionBasedModel,
			goalType,
			startPoint);
		
		return result;
	}

	/**
	 * Main method to optimize the parameters of the model given the queries and evidences when the model is Expression based.
	 *
	 */
	public static HashMap<Expression, Double> optimizeWhenModelIsExpressionBased(
			Expression[] queryExpression,
			Expression[] evidence,
			ExpressionBasedModel expressionBasedModel,
			GoalType goalType,
			double[] startPoint) {

		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression[] listOfMarginals = buildListOfMarginals(queryExpression, evidence,
				expressionBasedModel, context);
		
		Expression marginalFunctionLog = applyLogTransformationProductToSum(listOfMarginals);
		
		System.out.println(marginalFunctionLog);
		
		Set<Expression> listOfVariables = freeVariables(marginalFunctionLog, context);
		
		double[] argopt = returnArgopt(startPoint, goalType, marginalFunctionLog);
		
		HashMap<Expression, Double> result = buildHashMapContainingResult(listOfVariables, argopt);
		
		return result;
	}

	/**
	 * Calculate the arg of the optimum and apply the sigmoid operation to have the result between 0 and 1.
	 *
	 */
	private static double[] returnArgopt(double[] startPoint, GoalType goalType, Expression marginalFunctionLog) {
		OptimizationWithNonlinearConjugateGradientDescent optimizer = new OptimizationWithNonlinearConjugateGradientDescent(marginalFunctionLog, goalType, startPoint);
		double[] resultSig = optimizer.findArgopt();
		double[] result = new double[resultSig.length];
		for (int i = 0; i < resultSig.length; i++) {
			result[i] = 1/(1+exp(-resultSig[i]));
		}
		return result;
	}

	/**
	 * Build the map to have each variable associated with its optimum.
	 *
	 */
	private static HashMap<Expression, Double> buildHashMapContainingResult(Set<Expression> listOfVariables,
			double[] argopt) {
		HashMap<Expression,Double> result = new HashMap<Expression,Double>();
		int k = 0;
		for (Expression e : listOfVariables) {
			result.put(e, argopt[k]);
			k++;
		}
		return result;
	}

	/**
	 * Method to calculate the marginal for each query and evidence and return the list of marginals.
	 *
	 */
	private static Expression[] buildListOfMarginals(Expression[] queryExpression,
			Expression[] evidence, ExpressionBasedModel model,
			Context context) {
		
		Expression[] result = new Expression[queryExpression.length];
		for (int i = 0; i < queryExpression.length; i++) {
			
			ExpressionBasedModel modelAndEvidence = model.getConditionedModel(evidence[i]);
			ExpressionBasedSolver solver = new ExactBPExpressionBasedSolver();
			Expression marginal = solver.solve(queryExpression[i], modelAndEvidence);

			marginal = applySigmoidTrick(marginal, context);
	
			Expression marginalFunction = convertExpression(marginal);
			result[i] = marginalFunction;
		}
		
		return result;
	}
	
	/**
	 * Transform expression = a1*...*an into log(a1)+...+log(an)
	 *
	 */
	private static Expression applyLogTransformationProductToSum(Expression[] listOfMarginals) {
		Expression result;
		if (listOfMarginals.length > 1) {
			result = apply(PLUS, asList(listOfMarginals));
			for (int i = 0; i < listOfMarginals.length; i++) {
				Expression newIthArgument = apply(FunctorConstants.LOG, listOfMarginals[i]);
				result = result.set(i, newIthArgument);
			}
		}
		else {
			result = apply(FunctorConstants.LOG, listOfMarginals[0]);
		}
		return result;
	}

	/**
	 * Sigmoid trick to have the parameters and result between 0 and 1.
	 *
	 */
	private static Expression applySigmoidTrick(Expression marginal, Context context) {
		Set<Expression> variablesInExpression = freeVariables(marginal, context);
		//System.out.println(variablesInExpression);
		for (Expression arg : variablesInExpression) {
			Expression argChanged = apply(DIVISION, 1, 
					apply(PLUS, 1, apply(EXPONENTIAL, apply(MINUS,arg))));
			marginal = marginal.replaceAllOccurrences(arg, argChanged, context);
		}
		return marginal;
	}

	/**
	 * Method to have the right form of Expression for the optimization. For example, convert "if earthquake then Alpha else 1-Alpha" into "Alpha". 
	 *
	 */
	private static Expression convertExpression(Expression marginal) {
		String input = marginal.toString();
		input = input.substring(input.indexOf("then")+5);
		String resultString = input.split("else")[0];
		Expression result = parse(resultString);
		return result;
	}
	
}
