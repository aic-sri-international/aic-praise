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
import com.sri.ai.util.distribution.DiscretizedConditionalProbabilityDistribution;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Variable;

/**
 * A {@link java.util.function.Function} from array lists of values to a {@link Value} containing its probability.
 * <p>
 * It does that by {@link #register}ing samples of values for these variables, associated with a weight.
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
 * After creation, use {@link #sample()} to sample it as many times as desired,
 * and use {@link #apply(ArrayList<Object> valueObjects)}.
 * You can iterate further even after using the function in order to get better estimates of the probabilistic function,
 * so this could be used in a separate thread to refine the graph as initial versions are displayed. 
 * 
 */ 
public class SamplingFactorDiscretizedProbabilityDistribution extends DiscretizedConditionalProbabilityDistribution {
	
	private SamplingFactor samplingFactor;

	public SamplingFactorDiscretizedProbabilityDistribution(
			SamplingFactor samplingFactor,
			SetOfVariables inputVariablesWithRange,
			int queryVariableIndex) {
		
		super(inputVariablesWithRange, queryVariableIndex);
		this.samplingFactor = samplingFactor;
		myAssert(sameNumberOfVariablesForFunctionAndForSamplingFactor(), numberOfVariablesError());
		
	}

	//////////////////////////////

	/**
	 * Generates a new sample from sampling factor and registers it in the underlying conditional probability distribution.
	 */
	public void sample() {
		Pair<ArrayList<Object>, Double> valuesAndWeight = getValuesAndWeight();
		// println(getClass().getSimpleName() + ": values and weight: " + valuesAndWeight);
		register(valuesAndWeight.first, valuesAndWeight.second);
		// println("Distributions: " + this);
	}

	protected Pair<ArrayList<Object>, Double> getValuesAndWeight() {
		Sample sample = getSample(samplingFactor);
		return new Pair<>(getValueObjects(sample), getWeight(sample));
	}

	//////////////////////////////

	private static Sample getSample(SamplingFactor samplingFactor) {
		Sample sample = makeFreshSample();
		samplingFactor.sampleOrWeigh(sample);
		return sample;
	}

	private static DefaultSample makeFreshSample() {
		return new DefaultSample(new DoubleImportanceFactory(), new DoublePotentialFactory());
	}
	
	//////////////////////////////

	private ArrayList<Object> getValueObjects(Sample sample) {
		ArrayList<Object> result = mapIntoArrayList(samplingFactor.getVariables(), v -> sample.getAssignment().get(v));
		return result;
	}

	private double getWeight(Sample sample) {
		return sample.getPotential().doubleValue();
	}

	//////////////////////////////

	protected boolean sameNumberOfVariablesForFunctionAndForSamplingFactor() {
		return getSetOfVariablesWithRange().size() == this.samplingFactor.getVariables().size();
	}
	
	private NullaryFunction<String> numberOfVariablesError() {
		return () -> "Number of input variables must be the same as the number of variables in the sampling factor";
	}

}
