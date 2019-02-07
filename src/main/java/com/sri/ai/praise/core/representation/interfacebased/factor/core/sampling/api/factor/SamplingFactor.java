package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.Random;
import java.util.function.Function;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ConditionedSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;

/**
 * A sampling factor is a factor that represents its potential function by a set of samples.
 * 
 * @author braz
 *
 */
public interface SamplingFactor extends Factor {
	
	/**
	 * Attempts to fill in the current sample and update its potential and importance.
	 * Sampling factors are allowed to do no sampling if the given initial sample does not have enough information.
	 * For example, a sampling factor of a normal distribution will not produce any sampling if
	 * neither the mean nor the dependent (normally distributed) variable are assigned a value.
	 * @param sample
	 */
	void sampleOrWeigh(Sample sample);

	SamplingRuleSet getSamplingRuleSet();
	
	Random getRandom();
	
	default String nestedString(boolean showSamplingRules) {
		return nestedString(0, showSamplingRules);
	}
	
	String nestedString(int level, boolean showSamplingRules);

	default String rulesString(int level, boolean showSamplingRules) {
		if (!showSamplingRules) return "";
		String tab = fill(level*4, ' ');
		return 
				"\n" + tab + "--------------\n"
				+ join("\n", functionIterator(getSamplingRuleSet().getSamplingRules(), r -> tab + r))
				+ "\n" + tab + "--------------";
				
	}

	/**
	 * This method takes a value and a function that, given a variable, creates a sampling factor
	 * representing a function, with the given variable assigned to its result.
	 * It then provides a sampling factor conditioning the result to the given value.
	 * @param makeSamplingFactorGivenResultVariable
	 * @return
	 */
	public static SamplingFactor conditionResult(Object value, Function<Variable, SamplingFactor> makeSamplingFactorGivenResultVariable) {
		Variable functionResult = DefaultExpressionVariable.expressionVariable(makeSymbol(value));
		SamplingFactor samplingFactorAsFunction = makeSamplingFactorGivenResultVariable.apply(functionResult);
		return condition(samplingFactorAsFunction, functionResult, value);
	}

	public static SamplingFactor condition(SamplingFactor samplingFactor, Variable variable, Object value) {
		Sample sample = DefaultSample.makeFreshSample();
		sample.getAssignment().set(variable, value);
		SamplingFactor conditioned = ConditionedSamplingFactor.condition(samplingFactor, sample);
		return conditioned;
	}

}
