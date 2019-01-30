package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample.makeFreshSample;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.repeat;
import static com.sri.ai.util.Util.splice;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ConditionedSamplingFactor;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.distribution.DiscretizedConditionalProbabilityDistribution;
import com.sri.ai.util.function.api.values.Value;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.variables.DefaultSetOfVariables;

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
	private int initialNumberOfSamples;

	private static int globalId = 0;
	private int id;

	public SamplingFactorDiscretizedProbabilityDistribution(
			SamplingFactor samplingFactor,
			SetOfVariables inputVariablesWithRange,
			int queryVariableIndex,
			int initialNumberOfSamples) {
		
		super(inputVariablesWithRange, queryVariableIndex);
		this.samplingFactor = samplingFactor;
		this.initialNumberOfSamples = initialNumberOfSamples;
		myAssert(sameNumberOfVariablesForFunctionAndForSamplingFactor(), numberOfVariablesError());
		
		id = globalId++;
		println("Just created the " + id + "-th " + getClass().getSimpleName());
	}
	
	//////////////////////////////
	
	@Override
	protected void beforeFirstUseOfUnderlyingDistribution() {
		println("Taking " + initialNumberOfSamples + " samples from " + id + "-th " + getClass().getSimpleName() + " on " + samplingFactor);
		repeat(initialNumberOfSamples, () -> sample());
	}
	
	//////////////////////////////

	public SamplingFactor getSamplingFactor() {
		return samplingFactor;
	}
	
	public int getInitialNumberOfSamples() {
		return initialNumberOfSamples;
	}
	
	//////////////////////////////

	private Map<Sample, SamplingFactorDiscretizedProbabilityDistribution> conditionings = map();
	
	public SamplingFactorDiscretizedProbabilityDistribution condition(Sample conditioningSample) {
//		println("Conditioning on " + conditioningSample);
//		println("Within          " + System.identityHashCode(this));
//		println("Id            : " + id);
//		println("Keys in map:\n" + join("\n", conditionings.keySet()));
//		println("Conditioning sample belongs in map: " + conditionings.containsKey(conditioningSample));
		return getValuePossiblyCreatingIt(conditionings, conditioningSample, this::makeConditioning);
	}

	private SamplingFactorDiscretizedProbabilityDistribution makeConditioning(Sample conditioningSample) {
		SamplingFactor conditionedSamplingFactor = 
				ConditionedSamplingFactor.condition(samplingFactor, conditioningSample);
		
		List<Variable> projectedVariablesWithRange =                 /* reduced version of corresponding list */
				getReducedCorresponding(
						samplingFactor.getVariables(),               /* original list      */
						getSetOfVariablesWithRange().getVariables(), /* corresponding list */
						conditionedSamplingFactor.getVariables());   /* reduced version of original list */
		
		SetOfVariables projectedSetOfVariablesWithRange = 
				new DefaultSetOfVariables(projectedVariablesWithRange);
		
		int queryVariableIndexInProjection = 
				projectedVariablesWithRange.indexOf(getQueryVariable());
		
		println("Going to make " + SamplingFactorDiscretizedProbabilityDistribution.class.getSimpleName() + " conditioned on " + conditioningSample.getAssignment());
		
		SamplingFactorDiscretizedProbabilityDistribution result = 
				new SamplingFactorDiscretizedProbabilityDistribution(
						conditionedSamplingFactor, 
						projectedSetOfVariablesWithRange, 
						queryVariableIndexInProjection, 
						getInitialNumberOfSamples());
		
		return result;
		
		// TODO: this resets the underlying condition probability distribution and requires re-sampling
		// the conditioned distribution.
		// It is possible though to re-use the underlying distribution and condition it too to avoid initial re-sampling.
		// Depending on how conditioned the distribution is being, this may not be worth all that much, but it's
		// good to get done when we have the time.
	}

	private Variable getQueryVariable() {
		return getSetOfVariablesWithRange().get(getQueryVariableIndex());
	}

	/**
	 * Given an original list without repeated elements,
	 * another of the same size corresponding to it,
	 * and a reduced version of the original list,
	 * returns a new list containing the elements in the corresponding list
	 * that correspond to the elements in the reduced list.
	 * @param original
	 * @param reduced
	 * @return
	 */
	public static <T1, T2> List<T2> getReducedCorresponding(List<? extends T1> original, List<? extends T2> originalCorresponding, List<? extends T1> reduced) {
		List<Integer> indices = getIndicesOfIn(reduced, original);
		List<T2> reducedCorresponding = splice(originalCorresponding, indices);
		return reducedCorresponding;
	}
	
	/**
	 * Returns the indices (in the second list) of the elements in the first list. 
	 * @param reduced
	 * @param original
	 * @return
	 */
	public static <T> List<Integer> getIndicesOfIn(List<? extends T> reduced, List<? extends T> original) {
		List<Integer> result = mapIntoList(reduced, r -> Util.getIndexOf(original, r));
		return result;
	}

	//////////////////////////////

	/**
	 * Generates a new sample (starting from an initial sample if this distribution is conditioned)
	 * from sampling factor and registers it in the underlying conditional probability distribution.
	 */
	public void sample() {
		Pair<ArrayList<Object>, Double> valuesAndWeight = getValuesAndWeightForNewSample();
		register(valuesAndWeight.first, valuesAndWeight.second);
	}

	protected Pair<ArrayList<Object>, Double> getValuesAndWeightForNewSample() {
		Sample sample = getSample(samplingFactor);
		return getValuesAndWeight(sample);
	}

	private Sample getSample(SamplingFactor samplingFactor) {
		Sample sample = makeFreshSample();
		samplingFactor.sampleOrWeigh(sample);
		return sample;
	}

	private Pair<ArrayList<Object>, Double> getValuesAndWeight(Sample sample) {
		return new Pair<>(getValueObjects(sample), getWeight(sample));
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
