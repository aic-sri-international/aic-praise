package com.sri.ai.praise.learning.symbolicparameterestimation;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.applySigmoidTrick;
import static com.sri.ai.praise.learning.symbolicparameterestimation.util.UsefulOperationsParameterEstimation.findParameters;
import static java.lang.Math.exp;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

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
import com.sri.ai.expresso.optimization.FunctionToOptimize;
import com.sri.ai.expresso.optimization.GradientToOptimize;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.api.ExpressionBasedSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.exactbp.ExactBPExpressionBasedSolver;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.featurebased.ExpressionBasedModelToFeatureBasedModelTranslation;
import com.sri.ai.praise.core.representation.classbased.featurebased.FeatureBasedModel;

/**
 * Symbolic Regular Parameter Estimation (MLE) 
 *
 */

public class RegularSymbolicParameterEstimation implements ParameterEstimation {
	
	public ExpressionBasedModel expressionBasedModel;
	public List<Expression> parameters;
	public List<Expression> evidences;
	
	public RegularSymbolicParameterEstimation(ExpressionBasedModel expressionBasedModel, List<Expression> evidences) {
		this.expressionBasedModel = expressionBasedModel;
		this.parameters = findParameters(expressionBasedModel);
		this.evidences = evidences;
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
	 * Method to optimize the parameters for regular symbolic parameter estimation (MLE, complete data). 
	 * Result is a map of the name of the parameter with its value.
	 *
	 */
	public Map<Expression, Double> optimize() {
		
		Map<Expression, Double> result = new HashMap<Expression, Double>();
		
		List<Expression> parameters = findParameters(expressionBasedModel);
		
		ExpressionBasedModelToFeatureBasedModelTranslation translation = new ExpressionBasedModelToFeatureBasedModelTranslation(expressionBasedModel, parameters);
		FeatureBasedModel featureBasedModel = translation.featureBasedModel;
		
		System.out.println("featureBasedModel : " + featureBasedModel);
		
		Expression objectiveFunction = computeLogLikelyhoodFunction(featureBasedModel);
		
		Vector<Expression> gradient = buildGradient(featureBasedModel);
		
		PointValuePair optimum = optimize(objectiveFunction, gradient);
		
		double[] valueOptimum = optimum.getPoint();
		
		int k = 0;
		for(Expression key : featureBasedModel.mapConditionToWeight.keySet()) {
			double nonSigmoidResult = 1/(1+exp(-valueOptimum[k]));
			result.put(featureBasedModel.mapConditionToWeight.get(key).get(0), nonSigmoidResult);
			k++;
		}
		
		return result;
	}

	private Vector<Expression> buildGradient(FeatureBasedModel featureBasedModel) {
		Vector<Expression> gradient = new Vector<Expression>();
		
		for (Expression condition : featureBasedModel.mapConditionToWeight.keySet()) {
			Expression logParameter = featureBasedModel.mapConditionToWeight.get(condition);
			Expression parameter = logParameter.get(0);
			
			System.out.println("parameter : " + parameter);
			
			ExpressionBasedSolver solver = new ExactBPExpressionBasedSolver();
			Expression marginal = solver.solve(condition, expressionBasedModel);
			
			Expression probabilityOfCondition = convertExpression(marginal);
			
			Expression probabilityOfConditionWithSigmoidTrick = applySigmoidTrick(probabilityOfCondition, expressionBasedModel.getContext());
			
			System.out.println("probabilityOfConditionWithSigmoidTrick : " + probabilityOfCondition);
			
			int numberOfOccurences = countOccurencesGivenParameter(parameter);
			int numberOfEvidences = evidences.size();
			
			Expression secondTerm = apply(DIVISION, makeSymbol(numberOfOccurences), makeSymbol(numberOfEvidences));
		
			Expression ithTermOfgradient = apply(MINUS, probabilityOfConditionWithSigmoidTrick, secondTerm);
			
			System.out.println(ithTermOfgradient);
			
			gradient.add(ithTermOfgradient);
		}
		return gradient;
	}

	private Expression computeLogLikelyhoodFunction(FeatureBasedModel featureBasedModel) {
		
		Context initialContext = expressionBasedModel.getContext().clone();
		
		List<Expression> listToSum = new LinkedList<Expression>();
		
		for (Expression evidence : evidences) {
			
			Context newContext = initialContext.conjoin(evidence);
			
			for(Expression condition : featureBasedModel.mapConditionToWeight.keySet()) {
				
				boolean booleanValueOfConditionGivenEvidence = newContext.evaluate(condition).booleanValue();
				
				//System.out.println(newContext.evaluate(condition));
				Expression parameter = featureBasedModel.mapConditionToWeight.get(condition).get(0);
				Expression parameterWithSigmoidTrick = applySigmoidTrick(parameter, initialContext);
				
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
		System.out.println("loglikelihood function : " + result);
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
	
	/**
	 * Call the optimizer of Java Commons Math with the objectiveFunction and the gradient given in the arguments.
	 *
	 */
	public PointValuePair optimize(Expression expressionToOptimize, Vector<Expression> gradient) {
		
		NonLinearConjugateGradientOptimizer optimizer = new NonLinearConjugateGradientOptimizer(NonLinearConjugateGradientOptimizer.Formula.POLAK_RIBIERE, 
				new SimpleValueChecker(1e-13, 1e-13));
		
		final FunctionToOptimize f = new FunctionToOptimize(expressionToOptimize);
		final GradientToOptimize gradientToOptimize = new GradientToOptimize(expressionToOptimize, gradient);
		
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
