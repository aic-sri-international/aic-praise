package com.sri.ai.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.parseModelStringToHOGMModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMProblemError;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation;
import com.sri.ai.util.base.Pair;

/**
 * @author Sarah Perrin
 */
public class ParameterEstimationForHOGModel implements ParameterEstimation {
	
	public String stringModel;
	public HOGModel hogmModel;
	List<HOGMProblemError> modelErrors;
	public List<Pair<Expression, Expression>> pairsQueryEvidence;
	
	public ParameterEstimationForHOGModel(String stringModel, List<Pair<Expression, Expression>> pairsQueryEvidence, List<HOGMProblemError> modelErrors) {
		this.stringModel = stringModel;
		this.modelErrors = new ArrayList<>();
		this.hogmModel = parseModelStringToHOGMModel(stringModel, modelErrors);
		this.pairsQueryEvidence = pairsQueryEvidence;
	}
	
	/**
	 * Main method to optimize the parameters of the model given the queries and evidences when the model is String based.
	 *
	 */
	public HashMap<Expression, Double> optimizeWhenModelIsString(String modelString, GoalType goalType,
			double[] startPoint){
		
		List<HOGMProblemError> modelErrors = new ArrayList<>();
		
		HOGModel hogmModel = parseModelStringToHOGMModel(modelString, modelErrors);
		
		HashMap<Expression, Double> result = optimizeWhenModelIsHOGModel(
			hogmModel,
			goalType,
			startPoint);
		
		return result;
	}
	
	/**
	 * Main method to optimize the parameters of the model given the queries and evidences when the model is HOGModel based.
	 *
	 */
	public HashMap<Expression, Double> optimizeWhenModelIsHOGModel(HOGModel hogmModel, GoalType goalType,
			double[] startPoint){
		
		ExpressionBasedModel expressionBasedModel = UsefulOperationsParameterEstimation.parseHOGModelToExpressionBasedModel(hogmModel);
		
		ParameterEstimationForExpressionBasedModel parameterEstimationForExpressionBasedModel = new ParameterEstimationForExpressionBasedModel(expressionBasedModel, pairsQueryEvidence);
		
		System.out.println("expression based model after parsing : " + expressionBasedModel);
		
		HashMap<Expression, Double> result = parameterEstimationForExpressionBasedModel.optimize(
			startPoint);
		
		return result;
	}
	
	/**
	 * Return the optimized HOGModel.
	 *
	 */
	public HOGModel buildOptimizedHOGModel(Map<Expression, Double> optimizedParameters) {
		
		String optimizedStringModel = buildOptimizedStringModel(optimizedParameters);
		HOGModel result = parseModelStringToHOGMModel(optimizedStringModel, this.modelErrors);
		 
		return result;
		
	}

	private String replaceParameterWithOptimizedValue(Map<Expression, Double> optimizedParameters,
			String stringModelToModify, Expression parameter) {
		
		stringModelToModify = stringModelToModify.replaceAll("constant "+parameter.toString()+": Real;\n", "");
		Double value = optimizedParameters.get(parameter);
		Double oneMinusValue = 1.0 - value;
		stringModelToModify = stringModelToModify.replaceAll("\\b1-"+parameter.toString()+"\\b", oneMinusValue.toString());
		stringModelToModify = stringModelToModify.replaceAll("\\b"+parameter.toString()+"\\b", value.toString());
		return stringModelToModify;
	}

	/**
	 * Return the optimized String model.
	 *
	 */
	public String buildOptimizedStringModel(Map<Expression, Double> optimizedParameters) {
		
		String stringModelToModify = this.stringModel;
		
		System.out.println("stringModelToModify : " + this.stringModel);
		
		for (Expression parameter : optimizedParameters.keySet()) {
			stringModelToModify = replaceParameterWithOptimizedValue(optimizedParameters, stringModelToModify,
					parameter);
		}
		System.out.println(stringModelToModify);
		
		String result = stringModelToModify;
		return result;
		
	}
	
	@Override
	public Expression convertExpression(Expression marginal) {
		try {
			String input = marginal.toString();
			input = input.substring(input.indexOf("then") + 5);
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
