package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.distribution.DiscretizedConditionalProbabilityDistributionFunction;
import com.sri.ai.util.function.api.variables.Assignment;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Variable;

/**
 * Takes a function indicating the probability of an assignment to its values (a factor).
 * <p>
 * It does that by generating samples of values for these variables, associated with a weight.
 * We are interested in the probability of one of these variables, the query.
 * <p>
 * That is to say, if the factor is f(query, var2, ..., var_n),
 * them for each value q for query we want P(q) = normalization(sum_{var2, ..., var_n} (q, var2, ..., var_n)),
 * where normalization means to divide each P(q) by Z = sum_q' sum_{var2, ..., var_n} (q', var2, ..., var_n).
 * <p>
 * In other words, this function computes P(query) given a sampling factor.
 * <p>
 * To use the class, create an instance based on sampling factor, a query, and variables.
 * <p>
 * IMPORTANT: You need to provide the graph variables ({@link Variable}) <b>with ranges</b> (sets of values),
 * as that is used to decide how discretized the data will be, since sets of values can establish steps for real values.
 * In construction you also indicate which value is the query by giving its index.
 * <p>
 * IMPORTANT: the order of the Function variables must be the same as the order of the variables in the sampling factor
 * (this is how the correspondence is made).
 * <p>
 * After creation, use {@link #iterate()} to sample it as many times as desired,
 * and use {@link #evaluate(Assignment)}.
 * You can iterate further even after using the function in order to get better estimates of the probabilistic function,
 * so this could be used in a separate thread to refine the graph as initial versions are displayed. 
 * 
 */ 
public class SamplingFactorDiscretizedProbabilityDistributionFunction extends DiscretizedConditionalProbabilityDistributionFunction {
	
	// Rationale of implementation:
	// For each sample, we determine the joint index of all non-query variables,
	// determine a distribution (over the query) specific for that non-query variables combination,
	// and update it with the probability ("potential") of the sample.
	// At evaluation time, the appropriate distribution is picked to provide the probability for the query value.
	
	private SamplingFactor samplingFactor;

	public SamplingFactorDiscretizedProbabilityDistributionFunction(
			SamplingFactor samplingFactor,
			SetOfVariables inputVariablesWithRange,
			int queryVariableIndex) {
		
		super(inputVariablesWithRange, queryVariableIndex);
		this.samplingFactor = samplingFactor;
		myAssert(sameNumberOfVariablesForFunctionAndForSamplingFactor(), numberOfVariablesError());
		
	}

	//////////////////////////////

	public void iterate() {
		Pair<ArrayList<Object>, Double> valuesAndWeight = getValuesAndWeight();
		register(valuesAndWeight.first, valuesAndWeight.second);
	}

	protected Pair<ArrayList<Object>, Double> getValuesAndWeight() {
		Sample sample = getSample(samplingFactor);
		ArrayList<Object> valueObjects = mapIntoArrayList(samplingFactor.getVariables(), v -> sample.getAssignment().get(v));
		double weight = sample.getPotential().doubleValue();
		return new Pair<>(valueObjects, weight);
	}

	private static Sample getSample(SamplingFactor samplingFactor) {
		Sample sample = new DefaultSample(new DoubleImportanceFactory(), new DoublePotentialFactory());
		samplingFactor.sampleOrWeigh(sample);
		return sample;
	}
	
	//////////////////////////////

	protected boolean sameNumberOfVariablesForFunctionAndForSamplingFactor() {
		return getSetOfInputVariables().size() == this.samplingFactor.getVariables().size();
	}
	
	private NullaryFunction<String> numberOfVariablesError() {
		return () -> "Number of Function input variables must be the same as the number of variables in the sampling factor";
	}

}
