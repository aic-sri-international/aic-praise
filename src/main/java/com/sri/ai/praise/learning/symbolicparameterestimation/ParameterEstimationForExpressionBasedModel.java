package com.sri.ai.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.freeVariables;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.applySigmoidTrick;
import static org.apache.commons.math3.util.FastMath.exp;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.optimization.OptimizationWithNonlinearConjugateGradientDescent;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.exactbp.ExactBPExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel;

/**
 * Parameter Estimation for ExpressionBasedModel.
 * 
 * @author Sarah Perrin
 *
 */

public class ParameterEstimationForExpressionBasedModel implements ParameterEstimation {

	public ExpressionBasedModel model;
	public List<Expression> evidences;

	public ParameterEstimationForExpressionBasedModel(ExpressionBasedModel model, List<Expression> evidences) {
		this.model = model;
		this.evidences = evidences;
	}

	/**
	 * Main method to optimize the parameters of the model given the queries and
	 * evidences when the model is Expression based.
	 *
	 */
	public HashMap<Expression, Double> optimize(ExpressionBasedModel expressionBasedModel,
			List<Expression> queryExpression, GoalType goalType, double[] startPoint) {

		Theory theory = new CommonTheory();
		Context context = new TrueContext(theory);

		List<Expression> listOfMarginals = buildListOfMarginals(queryExpression, expressionBasedModel, context);

		Expression marginalFunctionLog = applyLogTransformationProductToSum(listOfMarginals);

		System.out.println(marginalFunctionLog);

		Set<Expression> listOfVariables = freeVariables(marginalFunctionLog, context);
		
		if(listOfVariables.size() != startPoint.length) {
			throw new Error("Length of double[] startPoint doesn't match the number of variables to optimize with evidence");
		}

		double[] argopt = returnArgopt(startPoint, goalType, marginalFunctionLog);

		HashMap<Expression, Double> result = buildHashMapContainingResult(listOfVariables, argopt);

		return result;
	}

	/**
	 * Return an ExpressionBasedModel with the parameters replaced with the
	 * optimized values.
	 *
	 */
	public ExpressionBasedModel buildOptimizedExpressionBasedModel(Map<Expression, Double> optimizedParameters) {

		
		List<Expression> factors = new ArrayList<>();
		for (Expression factor : this.model.getFactors()) {
			factors.add(factor);
		}
		
		Map<String, String> mapFromRandomVariableNameToTypeName = new LinkedHashMap<String, String>();
		for (String parameter : this.model.getMapFromRandomVariableNameToTypeName().keySet()) {
			String value = this.model.getMapFromRandomVariableNameToTypeName().get(parameter);
			mapFromRandomVariableNameToTypeName.put(parameter, value);
		}
		
		Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = new LinkedHashMap<String, String>();
		for (String parameter : this.model.getMapFromNonUniquelyNamedConstantNameToTypeName().keySet()) {
			String value = this.model.getMapFromNonUniquelyNamedConstantNameToTypeName().get(parameter);
			mapFromNonUniquelyNamedConstantNameToTypeName.put(parameter, value);
		}
		
		Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = this.model.getMapFromUniquelyNamedConstantNameToTypeName();
		Map<String, String> mapFromCategoricalTypeNameToSizeString = this.model.getMapFromCategoricalTypeNameToSizeString();
		Collection<Type> additionalTypes = this.model.getAdditionalTypes();
		boolean isKnownToBeBayesianNetwork = this.model.isKnownToBeBayesianNetwork();
		
		for (Expression parameter : optimizedParameters.keySet()) {
			//System.out.println("parameter : " + parameter);
			Double value = optimizedParameters.get(parameter);
			
			int k = 0;
			for (Expression factor : factors) {
				String factorString = factor.toString();
				if (factorString.contains(parameter.toString())) {
					
					Double oneMinusValue = 1.0 - value;
					factorString = factorString.replaceAll("\\b1 - "+parameter.toString()+"\\b", oneMinusValue.toString());
					factorString = factorString.replaceAll("\\b"+parameter.toString()+"\\b", value.toString());
					Expression newFactor = parse(factorString);
					factors.set(k,newFactor);
					
				}
				k++;
			}
			mapFromNonUniquelyNamedConstantNameToTypeName.remove(parameter.toString());
		}
		
		ExpressionBasedModel result = new DefaultExpressionBasedModel(factors,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isKnownToBeBayesianNetwork);
		
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
	private List<Expression> buildListOfMarginals(List<Expression> queryExpression, ExpressionBasedModel model,
			Context context) {

		List<Expression> result = new LinkedList<Expression>();
		for (Expression query : queryExpression) {

			ExpressionBasedSolver solver = new ExactBPExpressionBasedSolver();
			Expression marginal = solver.solve(query, model);

			marginal = applySigmoidTrick(marginal, context);
			
			System.out.println(marginal);

			Expression marginalFunction = convertExpression(marginal);
			
			System.out.println(marginalFunction);
			
			Set<Expression> listOfVariables = freeVariables(marginalFunction, context);
			if (listOfVariables.isEmpty()) {
				throw new Error("Nothing to optimized in the expression : " + marginalFunction);
			}
			
			result.add(marginalFunction);
		}

		return result;
	}

	/**
	 * Transform expression = a1*...*an into log(a1)+...+log(an)
	 *
	 */
	private Expression applyLogTransformationProductToSum(List<Expression> listOfMarginals) {
		Expression result;
		if (listOfMarginals.size() > 1) {
			result = apply(PLUS, listOfMarginals);
			for (int i = 0; i < listOfMarginals.size(); i++) {
				Expression newIthArgument = apply(FunctorConstants.LOG, listOfMarginals.get(i));
				result = result.set(i, newIthArgument);
			}
		} else {
			result = apply(FunctorConstants.LOG, listOfMarginals.get(0));
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

}
