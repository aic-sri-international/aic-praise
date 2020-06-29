package com.sri.ai.praise.learning.symbolicparameterestimation.regularparameterestimation;

import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.findParameters;
import static java.lang.Math.exp;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.math3.analysis.MultivariateVectorFunction;
import org.apache.commons.math3.optim.InitialGuess;
import org.apache.commons.math3.optim.MaxEval;
import org.apache.commons.math3.optim.PointValuePair;
import org.apache.commons.math3.optim.SimpleValueChecker;
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType;
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction;
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunctionGradient;
import org.apache.commons.math3.optim.nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.featurebased.ExpressionBasedModelToFeatureBasedModelTranslation;
import com.sri.ai.praise.core.representation.classbased.featurebased.FeatureBasedModel;

/**
 * Regular Parameter Estimation. Implementation is inspired by "Graphical Models in a Nutshell" by Daphne Koller and Nir Friedman. 
 *
 *@author Sarah Perrin
 */

public class RegularParameterEstimation {
	
	public ExpressionBasedModel expressionBasedModel;
	public List<Expression> parameters;
	public List<Expression> queries;
	public FeatureBasedModel featureBasedModel;
	
	public RegularParameterEstimation(ExpressionBasedModel expressionBasedModel, List<Expression> queries) {
		this.expressionBasedModel = expressionBasedModel;
		this.parameters = findParameters(expressionBasedModel);
		this.queries = queries;

		ExpressionBasedModelToFeatureBasedModelTranslation translation = new ExpressionBasedModelToFeatureBasedModelTranslation(expressionBasedModel, parameters);
		this.featureBasedModel = translation.featureBasedModel;
	}
	
	/**
	 * Method to optimize the parameters for regular parameter estimation. 
	 * Result is a map of the name of the parameter with its value.
	 *
	 */
	public Map<Expression, Double> optimize() {
		
		Map<Expression, Double> result = new HashMap<Expression, Double>();
		
		System.out.println("featureBasedModel : " + featureBasedModel);
		
		PointValuePair optimum = optimizeModel();
		
		double[] valueOptimum = optimum.getPoint();
		
		int k = 0;
		for(Expression condition : featureBasedModel.mapConditionToWeight.keySet()) {
			double nonSigmoidResult = 1/(1+exp(-valueOptimum[k]));
			//System.out.println(nonSigmoidResult);
			//System.out.println(featureBasedModel.mapConditionToWeight.get(condition));
			result.put(featureBasedModel.mapConditionToWeight.get(condition), nonSigmoidResult);
			k++;
		}
		
		return result;
	}
	
	/**
	 * Call the optimizer of Java Commons Math.
	 *
	 */
	public PointValuePair optimizeModel() {
		
		NonLinearConjugateGradientOptimizer optimizer = new NonLinearConjugateGradientOptimizer(NonLinearConjugateGradientOptimizer.Formula.POLAK_RIBIERE, 
				new SimpleValueChecker(1e-6, 1e-6));
		
		final LoglikelihoodToOptimize f = new LoglikelihoodToOptimize(expressionBasedModel, queries);
		final GradientLoglikelihoodToOptimize gradientToOptimize = new GradientLoglikelihoodToOptimize(expressionBasedModel, queries);
		
		ObjectiveFunction objectiveFunction = new ObjectiveFunction(f);
		MultivariateVectorFunction gradientMultivariateFunction = gradientToOptimize;
		ObjectiveFunctionGradient objectiveFunctionGradient = new ObjectiveFunctionGradient(gradientMultivariateFunction);
		
		double[] startPoint = new double[] {0.5,0.5};
		
		final PointValuePair optimum =
				optimizer.optimize(new MaxEval(10000), objectiveFunction, objectiveFunctionGradient, GoalType.MAXIMIZE, new InitialGuess(startPoint)); 

	    /*System.out.println(Arrays.toString(optimum.getPoint()) + " : "
	            + optimum.getSecond());*/
	        
	    return optimum;
	}

}
