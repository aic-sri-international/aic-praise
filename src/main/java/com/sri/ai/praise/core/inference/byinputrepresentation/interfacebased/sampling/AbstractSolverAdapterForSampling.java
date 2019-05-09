package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.sampling;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.praise.core.PRAiSEUtil.normalize;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionSamplingFactor.expressionSamplingFactor;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.myAssert;

import java.util.Random;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.Solver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ConstantFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.VariableExpressionDiscretization;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;

public abstract class AbstractSolverAdapterForSampling implements Solver {

	protected Function<Expression, Integer> fromExpressionVariableToNumberOfDiscreteValues;

	protected abstract Factor computeUnnormalizedMarginal(Problem problem, Random random);

	protected int initialNumberOfSamples;
	protected Context context;

	public AbstractSolverAdapterForSampling(
			Function<Expression, Integer> fromExpressionVariableToNumberOfDiscreteValues,
			int initialNumberOfSamples,
			Context context) {
		
		this.fromExpressionVariableToNumberOfDiscreteValues = fromExpressionVariableToNumberOfDiscreteValues;
		this.initialNumberOfSamples = initialNumberOfSamples;
		this.context = context;
	}

	@Override
	public Expression solve(Problem problem) {
	
			Random random = new Random();
			
			Factor unnormalizedMarginal = computeUnnormalizedMarginal(problem, random);
			
			SamplingFactor unnormalizedMarginalAsSamplingFactor = ensureItsSamplingFactor(unnormalizedMarginal, (ExpressionVariable) problem.getQueryVariable(), random);
			
			Expression normalizedMarginalExpression = getNormalizedMarginalExpression(unnormalizedMarginalAsSamplingFactor, problem);
			return normalizedMarginalExpression;
		}

	private SamplingFactor ensureItsSamplingFactor(Factor factor, ExpressionVariable queryVariable, Random random) {
		if (factor instanceof ConstantFactor) {
			return VariableExpressionDiscretization.makeUniformSamplingFactor(queryVariable, random, context);
		}
		else if (factor instanceof SamplingFactor) {
			return (SamplingFactor) factor;
		}
		else {
			throw new Error(getClass() + "' inner solver should have received either a ConstantFactor or a SamplingFactor, but got " + factor.getClass().getSimpleName() + " " + factor);
		}
	}

	protected boolean isRemainingVariable(Variable variable, Problem problem) {
		boolean result = 
				variable.equals(problem.getQueryVariable()) 
				|| 
				problem.getIsParameterPredicate().test(variable);
		return result;
	}

	private Expression getNormalizedMarginalExpression(Factor unnormalizedMarginal, Problem problem) {
		Expression normalizedMarginalExpression;
		if (unnormalizedMarginal instanceof ConstantFactor) {
			normalizedMarginalExpression = getNormalizedMarginalExpressionFromConstantFactor(problem);
		}
		else {
			normalizedMarginalExpression = getNormalizedMarginalExpressionFromSamplingFactor(unnormalizedMarginal, problem);
		}
		return normalizedMarginalExpression;
	}

	private Expression getNormalizedMarginalExpressionFromConstantFactor(Problem problem) {
		ExpressionVariable queryVariable = (ExpressionVariable) problem.getQueryVariable();
		Expression normalizedMarginalExpression = normalize(queryVariable, ONE, context);
		return normalizedMarginalExpression;
	}

	private Expression getNormalizedMarginalExpressionFromSamplingFactor(Factor unnormalizedMarginal, Problem problem) {
		SamplingFactor unnormalizedSamplingFactor = getUnnormalizedSamplingFactor(unnormalizedMarginal);
		Expression normalizedMarginalExpression = makeNormalizedMarginalExpression(unnormalizedSamplingFactor, problem);
		return normalizedMarginalExpression;
	}

	private SamplingFactor getUnnormalizedSamplingFactor(Factor unnormalizedMarginal) {
		SamplingFactor unnormalizedSamplingFactor = (SamplingFactor) unnormalizedMarginal; 
		myAssert(isDefinedOnExpressionVariables(unnormalizedSamplingFactor), () -> getClass() + " requires " + ExactBP.class.getSimpleName() + " to return " + SamplingFactor.class.getSimpleName() + " defined on " + ExpressionVariable.class.getSimpleName() + "s only.");
		return unnormalizedSamplingFactor;
	}

	private Expression makeNormalizedMarginalExpression(SamplingFactor unnormalizedSamplingFactor, Problem problem) {
		int queryIndex = unnormalizedSamplingFactor.getVariables().indexOf(problem.getQueryVariable());
		Expression normalizedMarginalExpression = 
				expressionSamplingFactor(
						unnormalizedSamplingFactor, 
						queryIndex, 
						fromExpressionVariableToNumberOfDiscreteValues, 
						initialNumberOfSamples,
						context);
		return normalizedMarginalExpression;
	}

	@Override
	public void interrupt() {
		throw new Error(this.getClass() + ".interrupt not implemented yet");
	}

	private boolean isDefinedOnExpressionVariables(SamplingFactor unnormalizedSamplingFactor) {
		return forAll(unnormalizedSamplingFactor.getVariables(), v -> v instanceof ExpressionVariable);
	}

}