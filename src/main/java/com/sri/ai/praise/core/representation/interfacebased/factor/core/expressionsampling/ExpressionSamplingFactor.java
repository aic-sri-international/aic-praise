package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.makeProxy;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.repeat;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.List;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ConditionedSamplingFactor;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.DefaultSamplingFactorDiscretizedProbabilityDistributionFunction;
import com.sri.ai.util.function.api.variables.SetOfVariables;

/**
 * An interface meant as the API for an {@link Expression} representing the discretized distribution
 * provided by a {@link SamplingFactor}.
 * The usage entry point for the interface and proxies is {@link #expressionSamplingFactor(SamplingFactor, int, Function, int, Context)},
 * which makes an instance based on a sampling factor and information required for discretization.
 * 
 * @author braz
 *
 */
public interface ExpressionSamplingFactor extends Expression, SamplingFactor, ExpressionWithProbabilityFunction {
	
	/**
	 * The maximum number of assignments to factor's discretized values
	 * that will allow generating an expression for it
	 * ({@link #TOO_LARGE_FOR_EXPRESSION_GENERATION} is used otherwise).
	 *
	 */
	public static int maximum_number_of_assignments_for_expression_generation = 200000;
	
	/**
	 * The factor's expression if the number of assignments to factor's discretized values
	 * is greater than {@link #maximum_number_of_assignments_for_expression_generation}.
	 */
	public static final Symbol TOO_LARGE_FOR_EXPRESSION_GENERATION = makeSymbol("too large");

	ExpressionSamplingFactor condition(Sample conditioningSample);
	
	@Override
	DefaultSamplingFactorDiscretizedProbabilityDistributionFunction getDiscretizedConditionalProbabilityDistributionFunction();

	@Override
	public int getDiscretizedConditionalProbabilityDistributionFunctionQueryIndex();
	
	void sample();
	
	boolean averageWeightIsZero();
	
	int getNumberOfSamples();
	
	double getTotalWeight();

	/**
	 * If given expression is an instance of {@link ExpressionSamplingFactor}, samples it a given number of times.
	 * @param expression
	 * @param numberOfSamples
	 */
	public static void sample(Expression expression, int numberOfSamples) {
		if (expression instanceof ExpressionSamplingFactor) {
			repeat(numberOfSamples, () -> ((ExpressionSamplingFactor) expression).sample());
		}
	}

	public static ExpressionSamplingFactor expressionSamplingFactor(
			SamplingFactor samplingFactor, 
			int queryIndex, 
			Function<Expression, Integer> fromVariableToNumberOfDiscreteValues,
			int initialNumberOfSamples,
			Context context) {
		
		return makeProxy(
				ExpressionSamplingFactor.class, 
				samplingFactor, 
				queryIndex, 
				fromVariableToNumberOfDiscreteValues, 
				initialNumberOfSamples, 
				context);
	}

	public static class ExpressionSamplingFactorProxyInvocationHandler implements InvocationHandler {

		private SamplingFactor samplingFactor;
		private int queryIndex;
		private Function<Expression, Integer> fromVariableToNumberOfDiscreteValues;
		private int initialNumberOfSamples;
		private Context context;
		private DefaultSamplingFactorDiscretizedProbabilityDistributionFunction samplingFactorDiscretizedProbabilityDistribution;
		
		public ExpressionSamplingFactorProxyInvocationHandler(SamplingFactor samplingFactor, int queryIndex, Function<Expression, Integer> fromVariableToNumberOfDiscreteValues, int initialNumberOfSamples, Context context) {
			this.samplingFactor = samplingFactor;
			this.queryIndex = queryIndex;
			this.fromVariableToNumberOfDiscreteValues = fromVariableToNumberOfDiscreteValues;
			this.initialNumberOfSamples = initialNumberOfSamples;
			this.context = context;
		}
		
		public Function<Expression, Integer> getFromVariableToNumberOfDiscreteValues() {
			return fromVariableToNumberOfDiscreteValues;
		}

		public void setFromVariableToNumberOfDiscreteValues(Function<Expression, Integer> fromVariableToNumberOfDiscreteValues) {
			this.fromVariableToNumberOfDiscreteValues = fromVariableToNumberOfDiscreteValues;
		}

		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if (method.getDeclaringClass().isAssignableFrom(Expression.class)) {
				return method.invoke(getFactorExpression(), args);
			}
			else if (method.getDeclaringClass().isAssignableFrom(SamplingFactor.class)) {
				Object result = method.invoke(samplingFactor, args);
				return result;
			}
			else if (method.getName().equals("condition")) {
				return condition((Sample) args[0]);
			}
			else if (method.getName().equals("getDiscretizedConditionalProbabilityDistributionFunction")) {
				return getSamplingFactorDiscretizedProbabilityDistributionFunction();
			}
			else if (method.getName().equals("getDiscretizedConditionalProbabilityDistributionFunctionQueryIndex")) {
				return queryIndex;
			}
			else if (method.getName().equals("sample")) {
				sample();
				return null;
			}
			else if (method.getName().equals("getNumberOfSamples")) {
				return getNumberOfSamples();
			}
			else if (method.getName().equals("averageWeightIsZero")) {
				return averageWeightIsZero();
			}
			else if (method.getName().equals("getTotalWeight")) {
				return getTotalWeight();
			}
			else {
				throw new Error(getClass() + " received method '" + method + "' of " + method.getDeclaringClass() + " which it is not prepared to execute.");
			}
		}

		public ExpressionWithProbabilityFunction condition(Sample conditioningSample) {
			
			SamplingFactor conditionedSamplingFactor = 
					ConditionedSamplingFactor.condition(samplingFactor, conditioningSample);
			
			ExpressionWithProbabilityFunction result = 
					expressionSamplingFactor(conditionedSamplingFactor, queryIndex, fromVariableToNumberOfDiscreteValues, initialNumberOfSamples, context);
			
			return result;
			
		}

		public void sample() {
			getSamplingFactorDiscretizedProbabilityDistributionFunction().sample();
		}
		
		public boolean averageWeightIsZero() {
			return getSamplingFactorDiscretizedProbabilityDistributionFunction().averageWeightIsZero();
		}

		public int getNumberOfSamples() {
			return getSamplingFactorDiscretizedProbabilityDistributionFunction().getNumberOfSamples();
		}

		public double getTotalWeight() {
			return getSamplingFactorDiscretizedProbabilityDistributionFunction().getTotalWeight();
		}
		
		private Expression expression;
		
		private Expression getFactorExpression() {
			if (expression == null) {
				FromFunctionToProbabilityExpression expressionMaker = 
						new FromFunctionToProbabilityExpression(
								getSamplingFactorDiscretizedProbabilityDistributionFunction(),
								queryIndex);
				expression = expressionMaker.getFactorExpression();
			}
			return expression;
		}

		public DefaultSamplingFactorDiscretizedProbabilityDistributionFunction getSamplingFactorDiscretizedProbabilityDistributionFunction() {
			if (samplingFactorDiscretizedProbabilityDistribution == null) {
				samplingFactorDiscretizedProbabilityDistribution = makeSamplingFactorDiscretizedProbabilityDistribution();
			}
			return samplingFactorDiscretizedProbabilityDistribution;
		}
		
		private DefaultSamplingFactorDiscretizedProbabilityDistributionFunction makeSamplingFactorDiscretizedProbabilityDistribution() {
			SetOfVariables setOfVariables = makeSetOfVariables();
			DefaultSamplingFactorDiscretizedProbabilityDistributionFunction result =
					new DefaultSamplingFactorDiscretizedProbabilityDistributionFunction(
							samplingFactor,
							setOfVariables,
							queryIndex,
							initialNumberOfSamples);
			return result;
		}

		private SetOfVariables makeSetOfVariables() {
			List<? extends ExpressionVariable> expressionVariables = mapIntoList(samplingFactor.getVariables(), v -> (ExpressionVariable) v);
			SetOfVariables setOfVariables = VariableExpressionDiscretization.makeSetOfVariablesWithRanges(expressionVariables, fromVariableToNumberOfDiscreteValues, context);
			return setOfVariables;
		}

	}

}
