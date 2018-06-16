package com.sri.ai.praise.learning.symbolicparameterestimation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMModelParsing;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMProblemError;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;

public class ParameterEstimationForHOGModel {
	
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
	
	public HOGModel parseModelStringToHOGMModel(String modelString, List<HOGMProblemError> modelErrors) {
		
		//System.out.println(modelString);
		HOGMModelParsing parsingWithErrorCollecting = new HOGMModelParsing(modelString, modelErrors);
		//System.out.println(parsingWithErrorCollecting.toString());
		//System.out.println("errors : " + modelErrors);
		HOGModel result = parsingWithErrorCollecting.getModel();
		//System.out.println("random variable declaration : " + result.getRandomVariableDeclarations());
		return result;
	}
	
	public static ExpressionBasedModel parseHOGModelToExpressionBasedModel(HOGModel hogmModel) {
		
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

}
