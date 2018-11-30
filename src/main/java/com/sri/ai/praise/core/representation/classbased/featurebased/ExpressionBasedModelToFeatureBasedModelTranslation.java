package com.sri.ai.praise.core.representation.classbased.featurebased;

import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation;

/**
 * @author Sarah Perrin
 */
public class ExpressionBasedModelToFeatureBasedModelTranslation {
	
	public FeatureBasedModel featureBasedModel;
	public ExpressionBasedModel expressionBasedModel; 
	
	public ExpressionBasedModelToFeatureBasedModelTranslation(FeatureBasedModel featureBasedModel, ExpressionBasedModel expressionBasedModel) {
		this.featureBasedModel = featureBasedModel;
		this.expressionBasedModel = expressionBasedModel;
	}
	
	/**
	 * Constructor to build an ExpressionBasedModelToFeatureBasedModelTranslation object from only an expressionBasedModel and the parameters
	 *
	 */
	public ExpressionBasedModelToFeatureBasedModelTranslation(ExpressionBasedModel expressionBasedModel, List<Expression> parameters) {
		this.expressionBasedModel = expressionBasedModel;
		this.featureBasedModel = translateExpressionBasedModelToFeatureBasedModel(expressionBasedModel);
	}
	
	/**
	 * Build the FeatureBasedModel (not yet optimized) from the ExpressionBasedModel and a list of parameters.
	 *
	 */
	public static FeatureBasedModel translateExpressionBasedModelToFeatureBasedModel(ExpressionBasedModel expressionBasedModel) {

		List<Expression> parameters = UsefulOperationsParameterEstimation.findParameters(expressionBasedModel);
		Map<Expression, Expression> map = new HashMap<Expression, Expression>();
		Context context = expressionBasedModel.getContext();
		
		for (Expression factor : expressionBasedModel.getFactors()) {
			
			for (Expression parameter : parameters) {
				
				IndexExpressionsSet indexExpressionsSet = getIndexExpressionsOfFreeVariablesIn(parameter, context); 
				
				Expression F1 = new DefaultUniversallyQuantifiedFormula(indexExpressionsSet, Equality.make(factor, parameter));
				
				Expression condition = context.evaluate(F1);
				
				System.out.println("condition : " + condition);
				
				if(!condition.getValue().equals(true) && !condition.getValue().equals(false)) {
					
					map.put(condition, parameter);
				}
			}
		}
		Map<Expression, Expression> sortedMap = 
			     map.entrySet().stream()
			    .sorted(Entry.comparingByValue())
			    .collect(Collectors.toMap(Entry::getKey, Entry::getValue,
			                              (e1, e2) -> e1, LinkedHashMap::new));
		
		FeatureBasedModel result = new FeatureBasedModel(sortedMap);
		
		return result;
	}
}
