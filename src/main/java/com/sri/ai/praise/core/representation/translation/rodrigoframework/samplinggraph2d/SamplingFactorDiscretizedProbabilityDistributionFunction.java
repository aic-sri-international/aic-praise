package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import static com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.AssignmentToSampleConverter.getCorrespondingSample;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.util.distribution.DiscretizedConditionalProbabilityDistributionFunction;
import com.sri.ai.util.function.api.variables.Assignment;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.graph2d.api.GraphSetMaker;

/**
 * Adds {@link com.sri.ai.util.function.api.functions.Function} functionality
 * to {@link SamplingFactorDiscretizedProbabilityDistribution},
 * equipping it with {@link #evaluate(Assignment)} method.
 * Among other things, this allows it to be used by {@link GraphSetMaker}.
 * <p>
 * It also defines a {@link #sample()} method that further samples the inner {@link SamplingFactorDiscretizedProbabilityDistribution}.
 */ 
public class SamplingFactorDiscretizedProbabilityDistributionFunction extends DiscretizedConditionalProbabilityDistributionFunction {
	
	public SamplingFactorDiscretizedProbabilityDistributionFunction(
			SamplingFactor samplingFactor,
			SetOfVariables inputVariablesWithRange,
			int queryVariableIndex,
			int initialNumberOfSamples) {
		
		this(new SamplingFactorDiscretizedProbabilityDistribution(samplingFactor, inputVariablesWithRange, queryVariableIndex, initialNumberOfSamples));
	}
	
	private SamplingFactorDiscretizedProbabilityDistributionFunction(SamplingFactorDiscretizedProbabilityDistribution distribution) {
		super(distribution);
	}
	
	//////////////////////////////

	public SamplingFactor getSamplingFactor() {
		return getConditionalDistribution().getSamplingFactor();
	}
	
	@Override
	public SamplingFactorDiscretizedProbabilityDistribution getConditionalDistribution() {
		return (SamplingFactorDiscretizedProbabilityDistribution) super.getConditionalDistribution();
	}
	
	//////////////////////////////

	public void sample() {
		getConditionalDistribution().sample();
	}

	/**
	 * Overrides the default projection (which would refer to a base sampling factor on multiple variables)
	 * by conditioning on a sample corresponding to the given assignment.
	 * This has the advantage that sampling the new function will generate only samples within the conditioned space,
	 * as opposed to sampling the original factor (which will generate samples mostly out of the conditioned space)
	 * and only using the relevant samples that might generate. 
	 *
	 */
	@Override
	public 
	SamplingFactorDiscretizedProbabilityDistributionSingleInputFunctionAdapter 
	project(Variable variable, Assignment assignmentToRemainingVariables) {
		
		Sample conditioningSample = 
				getCorrespondingSample(getSamplingFactor(), getSetOfVariablesWithRange(), assignmentToRemainingVariables);
		
		SamplingFactorDiscretizedProbabilityDistributionFunction conditionedDistributionFunction = 
				condition(conditioningSample);
		
		SamplingFactorDiscretizedProbabilityDistributionSingleInputFunctionAdapter singleInputDistributionFunction =
				new SamplingFactorDiscretizedProbabilityDistributionSingleInputFunctionAdapter(conditionedDistributionFunction);
		
		return singleInputDistributionFunction;
		
	}

	/**
	 * Conditions the distribution to the values described in a given sample.
	 * This has the advantage that sampling the new function will generate only samples within the conditioned space,
	 * as opposed to sampling the original factor (which will generate samples mostly out of the conditioned space)
	 * and only using the relevant samples that might generate. 
	 *
	 */
	public SamplingFactorDiscretizedProbabilityDistributionFunction condition(Sample conditioningSample) {
		
		SamplingFactorDiscretizedProbabilityDistribution conditionedDistribution = 
				getConditionalDistribution().condition(conditioningSample);
		
		SamplingFactorDiscretizedProbabilityDistributionFunction conditionedDistributionFunction = 
				new SamplingFactorDiscretizedProbabilityDistributionFunction(conditionedDistribution);
		
		return conditionedDistributionFunction;
	}
	
}