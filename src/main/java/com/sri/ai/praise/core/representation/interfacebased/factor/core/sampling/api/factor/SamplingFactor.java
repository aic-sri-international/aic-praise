package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
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
public interface SamplingFactor extends Sampler, Factor {
	
	@Override
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
	 * This method takes a function that, given a variable, creates a sampling factor
	 * representing a boolean function, with the given variable assigned to the truth result,
	 * and provides a sampling factor conditioning the result to 'true'.
	 * @param makeSamplingFactorAsTruthFunction
	 * @return
	 */
	static SamplingFactor conditionToTrue(Function<Variable, SamplingFactor> makeSamplingFactorAsTruthFunction) {
		Variable truth = new DefaultExpressionVariable(TRUE);
		SamplingFactor samplingFactorAsTruthFunction = makeSamplingFactorAsTruthFunction.apply(truth);
		Sample sample = DefaultSample.makeFreshSample();
		sample.getAssignment().set(truth, true);
		SamplingFactor samplingFactorOfFunctionConditionedToTrue = ConditionedSamplingFactor.condition(samplingFactorAsTruthFunction, sample);
		return samplingFactorOfFunctionConditionedToTrue;
	}

}
