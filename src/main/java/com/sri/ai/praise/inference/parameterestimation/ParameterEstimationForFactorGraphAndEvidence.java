package com.sri.ai.praise.inference.parameterestimation;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.apache.commons.math3.util.FastMath;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.optimization.OptimizationWithNonlinearConjugateGradientDescent;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.inference.expressionbased.InferenceForFactorGraphAndEvidence;
import com.sri.ai.praise.inference.hogm.HOGMFactorsAndTypes;

/**
 * Parameter Estimation.
 * 
 * @author Sarah Perrin
 *
 */

public class ParameterEstimationForFactorGraphAndEvidence {

	// The definitions of categorical types
	static Map<String, String> mapFromCategoricalTypeNameToSizeString;

	// The definitions of other types
	static Collection<Type> additionalTypes;

	// The definitions of variables
	static Map<String, String> mapFromRandomVariableNameToTypeName;

	// The definitions of non-uniquely named constants
	static Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName;

	// The definitions of uniquely named constants
	static Map<String, String> mapFromUniquelyNamedConstantNameToTypeName;

	static boolean isBayesianNetwork;
	static List<Expression> factors;
	static Expression[] evidence;
	static Expression[] queryExpression;
	static Expression expected;
	static Expression expectedWithNoFactorization;
	static Expression expectedWithFactorization;

	/**
	 * Main method to optimize the parameters of the model given the queries and evidences.
	 *
	 */
	public static HashMap<Expression, Double> optimize(boolean useFactorization,
			Expression[] queryExpression,
			Expression[] evidence,
			boolean isBayesianNetwork,
			List<Expression> factors,
			Map<String, String> mapFromRandomVariableNameToTypeName,
			Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes,
			GoalType goalType,
			double[] startPoint) {

		
		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);
		
		Expression[] listOfMarginals = buildListOfMarginals(useFactorization, queryExpression, evidence,
				isBayesianNetwork, factors, mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName, mapFromUniquelyNamedConstantNameToTypeName,
				mapFromCategoricalTypeNameToSizeString, additionalTypes, context);
		
		Expression marginalFunctionLog = applyLogTransformationProductToSum(listOfMarginals);
		
		System.out.println(marginalFunctionLog);
		
		Set<Expression> listOfVariables = Expressions.freeVariables(marginalFunctionLog, context);
		
		double[] argopt = returnArgopt(startPoint, goalType, marginalFunctionLog);
		
		HashMap<Expression, Double> mapResult = buildHashMapContainingResult(listOfVariables, argopt);
		
		return mapResult;
	}

	/**
	 * Calculate the arg of the optimum and apply the sigmoid operation to have the result between 0 and 1.
	 *
	 */
	private static double[] returnArgopt(double[] startPoint, GoalType goalType, Expression marginalFunctionLog) {
		OptimizationWithNonlinearConjugateGradientDescent optimizer = new OptimizationWithNonlinearConjugateGradientDescent(marginalFunctionLog, goalType, startPoint);
		double[] result_sig = optimizer.findArgopt();
		double[] result = new double[result_sig.length];
		for (int i = 0; i < result_sig.length; i++) {
			result[i] = 1/(1+FastMath.exp(-result_sig[i]));
		}
		return result;
	}

	/**
	 * Build the map to have each variable associated with its optimum.
	 *
	 */
	private static HashMap<Expression, Double> buildHashMapContainingResult(Set<Expression> listOfVariables,
			double[] result) {
		HashMap<Expression,Double> mapResult = new HashMap<Expression,Double>();
		int k = 0;
		for (Expression e : listOfVariables) {
			mapResult.put(e, result[k]);
			k++;
		}
		return mapResult;
	}

	/**
	 * Method to calculate the marginal for each query and evidence and return the list of marginals.
	 *
	 */
	private static Expression[] buildListOfMarginals(boolean useFactorization, Expression[] queryExpression,
			Expression[] evidence, boolean isBayesianNetwork, List<Expression> factors,
			Map<String, String> mapFromRandomVariableNameToTypeName,
			Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes,
			Context context) {
		Expression[] listOfMarginals = new Expression[queryExpression.length];
		for (int i = 0; i < queryExpression.length; i++) {
			InferenceForFactorGraphAndEvidence inferencer;
			Expression marginal;
			inferencer = new InferenceForFactorGraphAndEvidence(
					new HOGMFactorsAndTypes(
							factors,
							mapFromRandomVariableNameToTypeName,
							mapFromNonUniquelyNamedConstantNameToTypeName,
							mapFromUniquelyNamedConstantNameToTypeName,
							mapFromCategoricalTypeNameToSizeString,
							additionalTypes),
					isBayesianNetwork,
					evidence[i],
					useFactorization, null);
			marginal = inferencer.solve(queryExpression[i]);

			marginal = applySigmoidTrick(marginal, context);
	
			Expression marginalFunction = convertExpression(marginal);
			listOfMarginals[i] = marginalFunction;
		}
		return listOfMarginals;
	}
	
	/**
	 * Transform expression = a1*...*an into log(a1)+...+log(an)
	 *
	 */
	private static Expression applyLogTransformationProductToSum(Expression[] listOfMarginals) {
		Expression result;
		if (listOfMarginals.length > 1) {
			result = Expressions.apply(FunctorConstants.PLUS, listOfMarginals);
			for (int i = 0; i < listOfMarginals.length; i++) {
				Expression newIthArgument = Expressions.apply(FunctorConstants.LOG, listOfMarginals[i]);
				result = result.set(i, newIthArgument);
			}
		}
		else {
			result = Expressions.apply(FunctorConstants.LOG, listOfMarginals[0]);
		}
		return result;
	}

	/**
	 * Sigmoid trick to have the parameters and result between 0 and 1.
	 *
	 */
	private static Expression applySigmoidTrick(Expression marginal, Context context) {
		Set<Expression> variablesInExpression = Expressions.freeVariables(marginal, context);
		//System.out.println(variablesInExpression);
		for (Expression arg : variablesInExpression) {
			Expression argChanged = apply(DIVISION, 1, 
					apply(PLUS, 1, apply(FunctorConstants.EXPONENTIAL, apply(FunctorConstants.MINUS,arg))));
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
