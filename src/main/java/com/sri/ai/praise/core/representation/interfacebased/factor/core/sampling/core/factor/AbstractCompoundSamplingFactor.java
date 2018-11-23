package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.util.Util.unionArrayList;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.ArrayList;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public abstract class AbstractCompoundSamplingFactor extends AbstractSamplingFactor {
	
	@Override
	public abstract void sample(Sample sample);

	private ArrayList<? extends SamplingFactor> inputFactors;
	
	public AbstractCompoundSamplingFactor(ArrayList<? extends SamplingFactor> inputFactors, Random random) {
		super(unionArrayList(functionIterator(inputFactors, Factor::getVariables)), random);
		this.inputFactors = inputFactors;
	}

	public ArrayList<? extends SamplingFactor> getInputFactors() {
		return inputFactors;
	}
	
}
