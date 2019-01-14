package com.sri.ai.praise.learning.symbolicparameterestimation.regularparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIAL;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.findParameters;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.apache.commons.math3.analysis.MultivariateVectorFunction;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.autodifferentiation.AutomaticDifferentiation;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.exactbp.ExactBPOnExpressionFactorsExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.featurebased.ExpressionBasedModelToFeatureBasedModelTranslation;
import com.sri.ai.praise.core.representation.classbased.featurebased.FeatureBasedModel;
import com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation;

/**
 * Class to build 
 * @author Sarah Perrin
 *
 */

public class GradientLoglikelihoodToOptimize implements MultivariateVectorFunction {
	
	public ExpressionBasedModel expressionBasedModel;
	public FeatureBasedModel featureBasedModel;
	
	public List<Expression> evidences;
	public List<Expression> parameters;
	
	public Theory theory;
	public Context context;
	public AutomaticDifferentiation autoDifferentiator;
	
	public GradientLoglikelihoodToOptimize(ExpressionBasedModel expressionBasedModel, List<Expression> evidences) {
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
	 * Implementation of the method from the interface MultivariateVectorFunction from Apache Commons Math.
	 *
	 */
    @Override
	public double[] value(double[] variables) {

		Map<Expression, Double> mapParametersToValue = createMap(variables);
    	
		Vector<Expression> gradient = buildGradient(mapParametersToValue);
		
		double[] result = evaluateGradient(gradient);
    	return result;
    }

    
    private double[] evaluateGradient(Vector<Expression> gradient) {
    	double[] result = new double[gradient.size()];
    	int i = 0;
		for (Expression ithTermOfGradient : gradient) {
			Expression simplifiedIthTermOfGradient = autoDifferentiator.simplify(ithTermOfGradient);
			result[i] = simplifiedIthTermOfGradient.doubleValue();
			i++;
		}
		return result;
	}

	/**
	 * Create the HashMap which associates every variable of the expression to its value.
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
    
    /**
	 * Build the gradient following the formula described in "Graphical Models in a Nutshell".
	 *
	 */
    private Vector<Expression> buildGradient(Map<Expression, Double> mapParametersToValue) {
		Vector<Expression> gradient = new Vector<Expression>();
		
		for (Expression condition : featureBasedModel.mapConditionToWeight.keySet()) {
			
			Expression parameter = featureBasedModel.mapConditionToWeight.get(condition);
			
			ExpressionBasedSolver solver = new ExactBPOnExpressionFactorsExpressionBasedSolver();
			
			ExpressionBasedModel newExpressionBasedModel = UsefulOperationsParameterEstimation.buildOptimizedExpressionBasedModel(mapParametersToValue, expressionBasedModel);
			Expression marginal = solver.solve(condition, newExpressionBasedModel);
			
			//System.out.println("marginal : " + marginal);
			
			Expression probabilityOfConditionWithSigmoidTrick = applySigmoid(marginal, expressionBasedModel.getContext());
			
			//System.out.println("probabilityOfConditionWithSigmoidTrick : " + probabilityOfConditionWithSigmoidTrick);
			
			probabilityOfConditionWithSigmoidTrick = convertExpression(probabilityOfConditionWithSigmoidTrick);
			
			//System.out.println("probabilityOfConditionWithSigmoidTrick after conversion : " + probabilityOfConditionWithSigmoidTrick);
			
			int numberOfOccurences = countOccurencesGivenParameter(parameter);
			int numberOfEvidences = evidences.size();
			
			Expression secondTerm = apply(DIVISION, makeSymbol(numberOfOccurences), makeSymbol(numberOfEvidences));
		
			Expression ithTermOfgradient = apply(MINUS, probabilityOfConditionWithSigmoidTrick, secondTerm);
			
			gradient.add(ithTermOfgradient);
		}
		return gradient;
	}
    
    public static Expression applySigmoid(Expression marginal, Context context) {
		Expression result = apply(DIVISION, 1, apply(PLUS, 1, apply(EXPONENTIAL, apply(MINUS, marginal))));
		return result;
	}
    
    public int countOccurencesGivenParameter(Expression parameter) {
		int result = 0;
		Context initialContext = expressionBasedModel.getContext().clone();
		for (Expression evidence : this.evidences) {
			Context newContext = initialContext.conjoin(evidence);
			for (Expression factor : this.expressionBasedModel.getFactors()) { 
				if (newContext.evaluate(factor)==parameter){
					result++;
				}
			}
			newContext = initialContext;
		}
		return result;
	}
    
    /**
	 * Convert an expression. For example, convert if earthquake then Alpha else 1-Alpha into: Alpha.
	 *
	 */
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

