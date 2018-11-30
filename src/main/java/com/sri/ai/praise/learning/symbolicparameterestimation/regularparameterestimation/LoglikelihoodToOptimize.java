package com.sri.ai.praise.learning.symbolicparameterestimation.regularparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIAL;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.findParameters;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.math3.analysis.MultivariateFunction;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.autodifferentiation.AutomaticDifferentiation;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.featurebased.ExpressionBasedModelToFeatureBasedModelTranslation;
import com.sri.ai.praise.core.representation.classbased.featurebased.FeatureBasedModel;

/**
 * Class to convert an Expression into a MultivariateFunction (from Apache Commons Math). 
 * Used for optimization.
 * @author Sarah Perrin
 *
 */

public class LoglikelihoodToOptimize implements MultivariateFunction {
	
	public ExpressionBasedModel expressionBasedModel; 
	public FeatureBasedModel featureBasedModel;

	public List<Expression> evidences;

	public List<Expression> parameters;
	
	public Theory theory;
	public Context context;
	public AutomaticDifferentiation autoDifferentiator;
	
	public LoglikelihoodToOptimize(ExpressionBasedModel expressionBasedModel, List<Expression> evidences) {
		this.expressionBasedModel = expressionBasedModel;
		this.evidences = evidences;
		this.parameters = findParameters(expressionBasedModel);
		
		ExpressionBasedModelToFeatureBasedModelTranslation translation = new ExpressionBasedModelToFeatureBasedModelTranslation(expressionBasedModel, parameters);
		this.featureBasedModel = translation.featureBasedModel;
		
		this.theory = new CommonTheory();
		this.context = new TrueContext(theory);
		autoDifferentiator = new AutomaticDifferentiation(e -> context.evaluate(e));
	}
	
	/**
	 * Implementation of the method from the interface MultivariateFunction
	 *
	 */
    public double value(double[] variables) {
    	
    	Map<Expression, Double> mapParametersToValue = createMap(variables);
    	
    	System.out.println("mapParametersToValue : " + mapParametersToValue);
    	
    	Expression likelihood = computeLogLikelihoodFunction(mapParametersToValue);
    	
    	Expression result = autoDifferentiator.simplify(likelihood);
    	
    	//System.out.println("value function : " + result);
    	
    	return result.doubleValue();
    }
    
    /**
	 * Create the HashMap which associates every parameter with its current value.
	 *
	 */
	private Map<Expression, Double> createMap(double[] variables) {
		Map<Expression, Double> result = new HashMap<Expression, Double>();
    	int i = 0;
    	for (Expression e : parameters) {
    		result.put(e, variables[i]);
    		i++;
    	}
    	return result;
	}
	
	private Expression computeLogLikelihoodFunction(Map<Expression, Double> mapParametersToValue) {
		
		Context initialContext = expressionBasedModel.getContext().clone();
		
		List<Expression> listToSum = new LinkedList<Expression>();
		
		for (Expression evidence : evidences) {
			
			Context newContext = initialContext.conjoin(evidence);
			
			for(Expression condition : featureBasedModel.mapConditionToWeight.keySet()) {
				
				boolean booleanValueOfConditionGivenEvidence = newContext.evaluate(condition).booleanValue();
				
				//System.out.println(newContext.evaluate(condition));
				Expression parameter = featureBasedModel.mapConditionToWeight.get(condition);
				Expression parameterValue = makeSymbol(mapParametersToValue.get(parameter));
				//System.out.println("parameterValue : " + parameterValue);
				Expression parameterWithSigmoidTrick = applySigmoid(parameterValue, initialContext);
				
				if (booleanValueOfConditionGivenEvidence){
					
					Expression logParameterWithSigmoidTrick = apply(FunctorConstants.LOG, parameterWithSigmoidTrick);
					listToSum.add(logParameterWithSigmoidTrick);
					
				}
				else if (!booleanValueOfConditionGivenEvidence) {
					
					parameterWithSigmoidTrick = apply(MINUS, parameterWithSigmoidTrick);
					parameterWithSigmoidTrick = apply(PLUS, parameterWithSigmoidTrick, 1);
					Expression logParameterWithSigmoidTrick = apply(FunctorConstants.LOG, parameterWithSigmoidTrick);
					listToSum.add(logParameterWithSigmoidTrick);
					
				}
			}
			newContext = initialContext;
		}

		Expression result = apply(DIVISION, apply(PLUS, listToSum), evidences.size());
		//System.out.println("loglikelihood function : " + result);
		return result;
	}
	
	public static Expression applySigmoid(Expression marginal, Context context) {
		Expression result = apply(DIVISION, 1, apply(PLUS, 1, apply(EXPONENTIAL, apply(MINUS, marginal))));
		return result;
	}

}