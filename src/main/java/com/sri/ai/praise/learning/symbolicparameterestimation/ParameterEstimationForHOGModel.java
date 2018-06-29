package com.sri.ai.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.parse;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMModelParsing;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMProblemError;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;

public class ParameterEstimationForHOGModel implements ParameterEstimation {
	
	public String stringModel;
	public HOGModel hogmModel;
	List<HOGMProblemError> modelErrors;
	public List<Expression> evidences;
	
	public ParameterEstimationForHOGModel(String stringModel, List<Expression> evidences, List<HOGMProblemError> modelErrors) {
		this.stringModel = stringModel;
		this.modelErrors = new ArrayList<>();
		this.hogmModel = parseModelStringToHOGMModel(stringModel, modelErrors);
		this.evidences = evidences;
	}
	
	public ParameterEstimationForHOGModel(HOGModel hogmModel, List<Expression> evidences, List<HOGMProblemError> modelErrors) {
		this.hogmModel = hogmModel;
		this.evidences = evidences;
		this.modelErrors = modelErrors;
	}
	
	public HOGModel parseModelStringToHOGMModel(String modelString, List<HOGMProblemError> modelErrors) {
		
		//System.out.println(modelString);
		HOGMModelParsing parsingWithErrorCollecting = new HOGMModelParsing(modelString, modelErrors);
		//System.out.println(parsingWithErrorCollecting.toString());
		//System.out.println("errors : " + modelErrors);
		HOGModel result = parsingWithErrorCollecting.getModel();
		//System.out.println("random variable declaration : " + result.getRandomVariableDeclarations());
		return result;
	}
	
	public ExpressionBasedModel parseHOGModelToExpressionBasedModel(HOGModel hogmModel) {
		
		ExpressionBasedModel result = hogmModel == null? null : new HOGMExpressionBasedModel(hogmModel);
		
		return result;
	}
	
	/**
	 * Main method to optimize the parameters of the model given the queries and evidences when the model is String based.
	 *
	 */
	public HashMap<Expression, Double> optimizeWhenModelIsString(String modelString, List<Expression> queryExpression, GoalType goalType,
			double[] startPoint){
		
		List<HOGMProblemError> modelErrors = new ArrayList<>();
		
		HOGModel hogmModel = parseModelStringToHOGMModel(modelString, modelErrors);
		
		HashMap<Expression, Double> result = optimizeWhenModelIsHOGModel(
			hogmModel,
			queryExpression,
			goalType,
			startPoint);
		
		return result;
	}
	
	/**
	 * Main method to optimize the parameters of the model given the queries and evidences when the model is HOGModel based.
	 *
	 */
	public HashMap<Expression, Double> optimizeWhenModelIsHOGModel(HOGModel hogmModel, List<Expression> queryExpression, GoalType goalType,
			double[] startPoint){
		
		ExpressionBasedModel expressionBasedModel = parseHOGModelToExpressionBasedModel(hogmModel);
		
		ParameterEstimationForExpressionBasedModel parameterEstimationForExpressionBasedModel = new ParameterEstimationForExpressionBasedModel(expressionBasedModel, queryExpression);
		
		System.out.println("expression based model after parsing : " + expressionBasedModel);
		
		HashMap<Expression, Double> result = parameterEstimationForExpressionBasedModel.optimize(
			expressionBasedModel,
			queryExpression,
			goalType,
			startPoint);
		
		return result;
	}
	
	/**
	 * Main method to optimize the parameters of the model given the queries and evidences when the model is HOGModel based.
	 *
	 */
	public HOGModel buildOptimizedHOGModel(Map<Expression, Double> optimizedParameters) {
		
		//prend le champ stringModel, le copie et le modifie puis construit un HOGModel a partir. 
		String stringModelToModify = this.stringModel;
		
		System.out.println("stringModelToModify : " + this.stringModel);
		
		//supprimer "constant Alpha: Real;\n", attention ne faire que pour les parametres concernes
		for (Expression parameter : optimizedParameters.keySet()) {
			System.out.println("parameter : "+parameter.toString());
			stringModelToModify = stringModelToModify.replaceAll("constant "+parameter.toString()+": Real;\n", "");
			Double value = optimizedParameters.get(parameter);
			Double oneMinusValue = 1.0 - value;
			stringModelToModify = stringModelToModify.replaceAll("\\b1-"+parameter.toString()+"\\b", oneMinusValue.toString());
			stringModelToModify = stringModelToModify.replaceAll("\\b"+parameter.toString()+"\\b", value.toString());
		}
		System.out.println(stringModelToModify);
		HOGModel result = parseModelStringToHOGMModel(stringModelToModify, this.modelErrors);
		//remplacer les parametres par la valeur apprise 
		return result;
		
	}

	/**
	 * Return the optimized String model.
	 *
	 */
	public String buildOptimizedStringModel(Map<Expression, Double> optimizedParameters) {
		
		//prend le champ stringModel, le copie et le modifie puis construit un HOGModel a partir. 
		String stringModelToModify = this.stringModel;
		
		System.out.println("stringModelToModify : " + this.stringModel);
		
		//supprimer "constant Alpha: Real;\n", attention ne faire que pour les parametres concernes
		for (Expression parameter : optimizedParameters.keySet()) {
			System.out.println("parameter : "+parameter.toString());
			stringModelToModify = stringModelToModify.replaceAll("constant "+parameter.toString()+": Real;\n", "");
			Double value = optimizedParameters.get(parameter);
			Double oneMinusValue = 1.0 - value;
			stringModelToModify = stringModelToModify.replaceAll("\\b1-"+parameter.toString()+"\\b", oneMinusValue.toString());
			stringModelToModify = stringModelToModify.replaceAll("\\b"+parameter.toString()+"\\b", value.toString());
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
