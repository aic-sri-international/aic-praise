package com.sri.ai.praise.core.representation.classbased.featurebased;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getIndexExpressionsOfFreeVariablesIn;
import static com.sri.ai.grinder.library.number.Times.getMultiplicands;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel;

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
		this.featureBasedModel = translateExpressionBasedModelToFeatureBasedModel(expressionBasedModel, parameters);
	}
	
	/**
	 * Build the FeatureBasedModel (not yet optimized) from the ExpressionBasedModel and a list of parameters.
	 *
	 */
	public static FeatureBasedModel translateExpressionBasedModelToFeatureBasedModel(ExpressionBasedModel expressionBasedModel, List<Expression> parameters) {

		
		Map<Expression, Expression> map = new HashMap<Expression, Expression>();
		Context context = expressionBasedModel.getContext();
		
		for (Expression factor : expressionBasedModel.getFactors()) {
			
			for (Expression parameter : parameters) {
				
				IndexExpressionsSet indexExpressionsSet = getIndexExpressionsOfFreeVariablesIn(parameter, context); 
				
				Expression F1 = new DefaultUniversallyQuantifiedFormula(indexExpressionsSet, Equality.make(factor, parameter));
				
				Expression condition = context.evaluate(F1);
				
				System.out.println("condition : " + condition);
				
				if(!condition.getValue().equals(true) && !condition.getValue().equals(false)) {
					Expression logParameter = apply(FunctorConstants.LOG, parameter);
					map.put(condition, logParameter);
				}
			}
		}
		
		FeatureBasedModel result = new FeatureBasedModel(map);
		
		return result;
	}
	
	public static void main(String[] args) {
		
		// The definitions of types
				Map<String, String> mapFromCategoricalTypeNameToSizeString = map();

				// The definitions of variables
				Map<String, String> mapFromRandomVariableNameToTypeName = map(
						"earthquake", "Boolean",
						"burglary",    "Boolean", 
						"alarm",      "Boolean"
						);

				// The definitions of non-uniquely named constants
				Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = map(
						"Alpha", "Real",
						"Beta", "Real"
						);

				// The definitions of non-uniquely named constants
				Map<String, String> mapFromUniquelyNamedConstantNameToTypeName = map();

				// a variant of the earthquake/burglary model in which some burglars are more active than others.
				boolean isBayesianNetwork = true;
				List<Expression> factors = getMultiplicands(parse("" + 
						"(if earthquake then Alpha else 1-Alpha) * " +
						"(if burglary then Beta else 1-Beta) * " +
						// note the division above of the potential by number of remaining values, as the probabilities must sum up to 1
						"(if burglary or earthquake "
						+    "then if alarm then 0.9 else 0.1 "
						+    "else if alarm then 0.05 else 0.95) " +
						""));
				
				ExpressionBasedModel expressionBasedModel = new DefaultExpressionBasedModel(
						factors,
						mapFromRandomVariableNameToTypeName,
						mapFromNonUniquelyNamedConstantNameToTypeName,
						mapFromUniquelyNamedConstantNameToTypeName,
						mapFromCategoricalTypeNameToSizeString,
						list(),
						isBayesianNetwork);
				
			List<Expression> parameters = new LinkedList<Expression>();
			parameters.add(parse("Alpha"));
			parameters.add(parse("Beta"));
			
			FeatureBasedModel test = ExpressionBasedModelToFeatureBasedModelTranslation.translateExpressionBasedModelToFeatureBasedModel(expressionBasedModel, parameters);
			System.out.println(test.toString());
		
		
	}
}
