package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.samplingRule;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.flatList;
import static com.sri.ai.util.Util.fold;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.number.representation.api.ArithmeticNumber;
import com.sri.ai.util.number.representation.core.ArithmeticDouble;

/**
 * A sampling factor respecting a sum relationship of the type <code>s = x_1 + x_2 + ... + x_n</code>.
 * It will instantiate any of the variables <code>s, x_1, x_2, ..., x_n</code> if the others are instantiated.
 * If all are instantiated, it will update the weight of the sample to 1 if the values are consistent, or 0 if not.
 * If more than one are uninstantiated, it does nothing.
 * <p>
 * Note that this does not solve equations. Given an equation <code>x = y + y</code> with instantiated <code>x</code>,
 * it will <b>not</not> instantiate <code>y</code> even though that would be possible in principle.
 * This choice was made because variables can be arbitrarily complex (consider <code>x = y + z</code> where <code>z</code>
 * is defined somewhere else to be <code>x * y</code>), and going this route is out of scope.
 * Instead, this type of factor guarantees to define one variable (the sum) as a function of others,
 * and the ability to instantiate variables other than then sum is seen as a sampling bonus that may or may not be available.
 *  
 * @author braz
 *
 */
public class SumSamplingFactor extends AbstractSamplingFactor {

	private Variable sum;
	private List<? extends Variable> summands;
	private PotentialFactory potentialFactory;
	
	public SumSamplingFactor(Variable result, List<? extends Variable> summands, PotentialFactory potentialFactory) {
		super(flatList(result, summands), null /* random */);
		this.sum = result;
		this.summands = summands;
		this.potentialFactory = potentialFactory;
	}

	@Override
	public void sampleOrWeigh(Sample sample) {

		int missingSummandIndex = analyzeMissingSummands(sample);
		
		if (missingSummandIndex == -1) {
			allSummandsDefined(sample);
		}
		else if (missingSummandIndex == -2) {
			moreThanOneSummandIsUndefined();
		}
		else {
			exactlyOneMissingSummand(sample, missingSummandIndex);
		}
	}

	/**
	 * Returns -1 if all summands are defined, -2 if more than one summand is undefined,
	 * and the index of the unique undefined summand otherwise.
	 * @param sample
	 * @return
	 */
	private int analyzeMissingSummands(Sample sample) {
		int result = -1; // all summands so far are defined
		int i = 0;
		for (Variable summand : summands) {
			if (isNotDefined(summand, sample)) {
				if (result == -1) { // this is the first undefined summand, so write down its index
					result = i; 
				}
				else { // we have seen an undefined summand before, so there is more than one, break and return -2
					result = -2;
					break;
				}
			}
			i++;
		}
		return result;
	}

	private void allSummandsDefined(Sample sample) {
		if (isDefined(sum, sample)) {
			checkConsistencyOfFullyDefinedVariables(sample);
		}
		else {
			completeSumResult(sample);
		}
	}

	private void moreThanOneSummandIsUndefined() {
		// do nothing; problem is underspecified
	}

	private void exactlyOneMissingSummand(Sample sample, int missingSummandIndex) {
		if (isDefined(sum, sample)) {
			completeMissingSummand(sample, missingSummandIndex);
		}
		else {
			// do nothing; problem is underspecified
		}
	}

	private void checkConsistencyOfFullyDefinedVariables(Sample sample) {
		ArithmeticNumber summandsSum = sum(summandValues(sample));
		if ( ! summandsSum.equals(getValue(sum, sample))) {
			sample.updatePotential(potentialFactory.make(0.0));
		}
	}

	private void completeSumResult(Sample sample) {
		ArithmeticNumber sumValue = sum(summandValues(sample));
		setValue(sum, sumValue, sample);
	}

	private void completeMissingSummand(Sample sample, int missingSummandIndex) {
		ArithmeticNumber missingSummandValue = computeMissingSummandValue(sample, missingSummandIndex);
		setMissingSummandValue(sample, missingSummandIndex, missingSummandValue);
	}

	private ArithmeticNumber computeMissingSummandValue(Sample sample, int missingSummandIndex) {
		Iterator<ArithmeticNumber> summandsButMissingOne = Util.filterByIndexIterator(summandValues(sample), i -> i != missingSummandIndex);
		ArithmeticNumber definedSummandsSum = sum(summandsButMissingOne);
		ArithmeticNumber missingSummandValue = getValue(sum, sample).subtract(definedSummandsSum);
		return missingSummandValue;
	}

	private void setMissingSummandValue(Sample sample, int missingSummandIndex, ArithmeticNumber missingSummandValue) {
		Variable missingSummand = summands.get(missingSummandIndex);
		setValue(missingSummand, missingSummandValue, sample);
	}

	private boolean isDefined(Variable variable, Sample sample) {
		boolean result = getValue(variable, sample) != null;
		return result;
	}

	private boolean isNotDefined(Variable variable, Sample sample) {
		boolean result = getValue(variable, sample) == null;
		return result;
	}

	private void setValue(Variable variable, ArithmeticNumber value, Sample sample) {
		sample.getAssignment().set(variable, value);
	}

	private ArithmeticNumber getValue(Variable variable, Sample sample) {
		ArithmeticNumber result = (ArithmeticNumber) sample.getAssignment().get(variable);
		return result;
	}

	private FunctionIterator<? extends Variable, ArithmeticNumber> summandValues(Sample sample) {
		return functionIterator(summands, v -> getValue(v, sample));
	}

	private ArithmeticNumber sum(Iterator<ArithmeticNumber> values) {
		ArithmeticNumber result = fold(values, ArithmeticNumber::add, new ArithmeticDouble(0.0));
		return result;
	}
	
	@Override
	protected SamplingRuleSet makeSamplingRules() {
		ArrayList<SamplingRule> samplingRules = makeSamplingRulesList();
		SamplingRule[] samplingRulesArray = samplingRules.toArray(new SamplingRule[samplingRules.size()]);
		DefaultSamplingRuleSet result = new DefaultSamplingRuleSet(getVariables(), samplingRulesArray);
		return result;
	}

	private ArrayList<SamplingRule> makeSamplingRulesList() {
		ArrayList<SamplingRule> result = arrayList();
		SamplingRule samplingRuleForSum = samplingRule(this, list(sum), summands, SamplingRule.MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
		result.add(samplingRuleForSum);
		for (int i = 0; i != summands.size(); i++) {
			SamplingRule samplingRule = makeSamplingRuleForSummandAt(i);
			result.add(samplingRule);
		}
		return result;
	}

	private SamplingRule makeSamplingRuleForSummandAt(int i) {
		List<Variable> otherSummandsAndSum = new ArrayList<Variable>(summands);
		otherSummandsAndSum.set(i, sum);
		SamplingRule samplingRule = samplingRule(this, list(summands.get(i)), otherSummandsAndSum, SamplingRule.MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
		return samplingRule;
	}

}
