package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.getValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.distribution.WeightedFrequencyArrayDistribution;
import com.sri.ai.util.graph2d.api.variables.Assignment;
import com.sri.ai.util.graph2d.api.variables.DefaultValue;
import com.sri.ai.util.graph2d.api.variables.SetOfVariables;
import com.sri.ai.util.graph2d.api.variables.Unit;
import com.sri.ai.util.graph2d.api.variables.Value;
import com.sri.ai.util.graph2d.api.variables.Variable;
import com.sri.ai.util.graph2d.core.functions.AbstractFunction;
import com.sri.ai.util.graph2d.core.variables.RealVariable;

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
public class SamplingGraphFunction extends AbstractFunction {
	
	// Rationale of implementation:
	// For each sample, we determine the joint index of all non-query variables,
	// determine a distribution (over the query) specific for that non-query variables combination,
	// and update it with the probability ("potential") of the sample.
	// At evaluation time, the appropriate distribution is picked to provide the probability for the query value.
	
	private SamplingFactor samplingFactor;

	private Map<ArrayList<Integer>, WeightedFrequencyArrayDistribution> distributions;
	
	private int queryVariableIndex;

	private Variable queryVariable;

	private com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable querySamplingVariable;

	protected SamplingGraphFunction(
			SamplingFactor samplingFactor,
			SetOfVariables inputVariablesWithRange,
			int queryVariableIndex) {
		
		super(makeOutputVariable(inputVariablesWithRange.get(queryVariableIndex)), inputVariablesWithRange);

		assertVariablesAllHaveADefinedSetOfValues(inputVariablesWithRange);
		
		this.queryVariable = getInputVariables().getVariables().get(queryVariableIndex);
		this.queryVariableIndex = queryVariableIndex;
		this.samplingFactor = samplingFactor;
		this.querySamplingVariable = samplingFactor.getVariables().get(queryVariableIndex);
		this.distributions = map();

		myAssert(sameNumberOfFunctionAndSamplingVariables(), numberOfVariablesError());
		
	}

	/**
	 * Takes a sample and updates internal statistics to be able to evaluate function later.
	 */
	public void iterate() {
		Sample sample = getSample(samplingFactor);
		WeightedFrequencyArrayDistribution distribution = getCorrespondingDistribution(sample);
		int queryValueIndex = getQueryValueIndex(sample);
		if (queryValueIndex != -1) { // out of the graph's range
			distribution.add(queryValueIndex, sample.getPotential().doubleValue());
		}
//		println("Added " + queryValueIndex + " with weight " + sample.getPotential());
//		println("Distribution: " + distribution.getProbabilities());
	}

	@Override
	public Value evaluate(Assignment assignmentToInputVariables) {
		WeightedFrequencyArrayDistribution distribution = getCorrespondingDistribution(assignmentToInputVariables);
		int queryValueIndex = getQueryValueIndex(assignmentToInputVariables);
		ArrayList<Double> probabilities = distribution.getProbabilities();
		Double probability = probabilities.get(queryValueIndex);
		return new DefaultValue(probability);
	}
	
	public WeightedFrequencyArrayDistribution getCorrespondingDistribution(Assignment assignment) {
		ArrayList<Integer> nonQueryValueIndices = getNonQueryValueIndices(assignment);
		WeightedFrequencyArrayDistribution distribution = getDistributionForNonQueryValueIndices(nonQueryValueIndices);
		return distribution;
	}

	public WeightedFrequencyArrayDistribution getCorrespondingDistribution(Sample sample) {
		ArrayList<Integer> nonQueryValueIndices = getNonQueryValueIndices(sample);
		WeightedFrequencyArrayDistribution distribution = getDistributionForNonQueryValueIndices(nonQueryValueIndices);
		return distribution;
	}
	
	private int getQueryValueIndex(Assignment assignment) {
		return getIndexOfValue(assignment, queryVariable);
	}

	private ArrayList<Integer> getNonQueryValueIndices(Assignment assignment) {
		ArrayList<Integer> result = arrayList(numberOfVariables());
		for (int i = 0; i != numberOfVariables(); i++) {
			if (i != queryVariableIndex) {
				int indexOfValueOfIthVariable = getIndexOfValue(assignment, getVariable(i));
				result.add(indexOfValueOfIthVariable);
			}
		}
		return result;
	}

	private int getQueryValueIndex(Sample sample) {
		return getIndexOfValue(sample, queryVariable, querySamplingVariable);
	}

	private ArrayList<Integer> getNonQueryValueIndices(Sample sample) {
		ArrayList<Integer> result = arrayList(numberOfVariables());
		for (int i = 0; i != numberOfVariables(); i++) {
			if (i != queryVariableIndex) {
				int indexOfValueOfIthVariable = getIndexOfValue(sample, getVariable(i), getSamplingVariable(i));
				result.add(indexOfValueOfIthVariable);
			}
		}
		return result;
	}

	private int getIndexOfValue(
			Sample sample,
			Variable variable,
			com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable samplingVariable) {
		
		Object sampleValueObject = sample.getAssignment().get(samplingVariable);
		myAssert(sampleValueObject != null, () -> "Sampling factor has not produced value for variable " + samplingVariable + " in sample " + sample + ". Factor is " + samplingFactor);
		Value sampleValue = Value.value(sampleValueObject);
		return getIndexOfValue(variable, sampleValue);
	}

	private int getIndexOfValue(Assignment assignment, Variable variable) {
		Value value = assignment.get(variable);
		return getIndexOfValue(variable, value);
	}

	public int getIndexOfValue(Variable variable, Value value) {
		int result = variable.getSetOfValuesOrNull().getIndex(value);
		return result;
	}

	private WeightedFrequencyArrayDistribution getDistributionForNonQueryValueIndices(ArrayList<Integer> nonQueryValueIndices) {
		WeightedFrequencyArrayDistribution result = 
				getValuePossiblyCreatingIt(
						distributions, 
						nonQueryValueIndices, 
						key -> new WeightedFrequencyArrayDistribution(queryVariable.getSetOfValuesOrNull().size() + 1, 0.01));
		return result;
	}

	public Variable getVariable(int i) {
		return getInputVariables().getVariables().get(i);
	}

	public com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable getSamplingVariable(int i) {
		return samplingFactor.getVariables().get(i);
	}

	public int numberOfVariables() {
		return samplingFactor.getVariables().size();
	}

	private static Sample getSample(SamplingFactor samplingFactor) {
		Sample sample = new DefaultSample(new DoubleImportanceFactory(), new DoublePotentialFactory());
		samplingFactor.sampleOrWeigh(sample);
		return sample;
	}

	public static RealVariable makeOutputVariable(Variable queryVariable) {
		return new RealVariable("P(" + queryVariable.getName() + " | ...)", Unit.NONE);
	}

	@Override
	public String getName() {
		return "P(" + queryVariable.getName() + " | ...)";
	}

	private NullaryFunction<String> numberOfVariablesError() {
		return () -> "Number of Function input variables must be the same as the number of variables in the sampling factor";
	}

	private boolean sameNumberOfFunctionAndSamplingVariables() {
		return getInputVariables().size() == this.samplingFactor.getVariables().size();
	}

	private void assertVariablesAllHaveADefinedSetOfValues(SetOfVariables inputVariablesWithRange) throws Error {
		List<? extends Variable> variables = inputVariablesWithRange.getVariables();
		Variable withoutSetOfValues = getFirstSatisfyingPredicateOrNull(variables, v -> v.getSetOfValuesOrNull() == null);
		if (withoutSetOfValues != null) {
			throw new Error(getClass() + " requires that all graph2d variables have a defined set of values, but " + withoutSetOfValues + " does not");
		}
	}

}
