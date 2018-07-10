package com.sri.ai.praise.learning.symbolicparameterestimation.util;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.freeVariables;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIAL;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMModelParsing;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.parsing.HOGMProblemError;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.HOGModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;

/**
 * Useful Operations that can be used for Parameter Estimation.
 * 
 * @author Sarah Perrin
 *
 */

public class UsefulOperationsParameterEstimation {
	
	public static List<Expression> findParameters(ExpressionBasedModel expressionBasedModel) {
		List<Expression> result = new LinkedList<Expression>();
		for (String parameterString : expressionBasedModel.getMapFromNonUniquelyNamedConstantNameToTypeName().keySet()) {
			Expression parameter = parse(parameterString);
			if(!result.contains(parameter)) {
				result.add(parameter);
			}
		}
		return result;
	}
	
	/**
	 * Sigmoid trick to have the parameters and result between 0 and 1.
	 *
	 */
	public static Expression applySigmoidTrick(Expression marginal, Context context) {
		Set<Expression> variablesInExpression = freeVariables(marginal, context);
		for (Expression arg : variablesInExpression) {
			Expression argChanged = apply(DIVISION, 1, apply(PLUS, 1, apply(EXPONENTIAL, apply(MINUS, arg))));
			marginal = marginal.replaceAllOccurrences(arg, argChanged, context);
		}
		return marginal;
	}
	
	public static HOGModel parseModelStringToHOGMModel(String modelString, List<HOGMProblemError> modelErrors) {
		
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
	 * Return an ExpressionBasedModel with the parameters replaced with the
	 * optimized values.
	 *
	 */
	public static ExpressionBasedModel buildOptimizedExpressionBasedModel(Map<Expression, Double> optimizedParameters, ExpressionBasedModel model) {

		
		List<Expression> factors = new ArrayList<>();
		for (Expression factor : model.getFactors()) {
			factors.add(factor);
		}
		
		Map<String, String> mapFromRandomVariableNameToTypeName = new LinkedHashMap<String, String>();
		for (String parameter : model.getMapFromRandomVariableNameToTypeName().keySet()) {
			String value = model.getMapFromRandomVariableNameToTypeName().get(parameter);
			mapFromRandomVariableNameToTypeName.put(parameter, value);
		}
		
		Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = new LinkedHashMap<String, String>();
		for (String parameter : model.getMapFromNonUniquelyNamedConstantNameToTypeName().keySet()) {
			String value = model.getMapFromNonUniquelyNamedConstantNameToTypeName().get(parameter);
			mapFromNonUniquelyNamedConstantNameToTypeName.put(parameter, value);
		}
		
		Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = model.getMapFromUniquelyNamedConstantNameToTypeName();
		Map<String, String> mapFromCategoricalTypeNameToSizeString = model.getMapFromCategoricalTypeNameToSizeString();
		Collection<Type> additionalTypes = model.getAdditionalTypes();
		boolean isKnownToBeBayesianNetwork = model.isKnownToBeBayesianNetwork();
		
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

}
