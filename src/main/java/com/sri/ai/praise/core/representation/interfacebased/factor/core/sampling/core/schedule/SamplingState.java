package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.getFirstNonNullResultOrNull;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.set;
import static com.sri.ai.util.Util.thereExists;
import static com.sri.ai.util.collect.DefaultManyToManyRelation.manyToManyRelation;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.UnmodifiableSample;
import com.sri.ai.util.collect.ManyToManyRelation;
import com.sri.ai.util.planning.api.State;

public class SamplingState implements State {

	private Sample sample;
	private Collection<? extends SamplingFactor> factors;
	private Collection<SamplingFactor> factorsThatFired;
	private ManyToManyRelation<SamplingFactor, Variable> factorsAndVariablesRelation;
	private Random random;

	public SamplingState(Sample sample, Collection<? extends SamplingFactor> factors, Random random) {
		this.sample = sample;
		this.factors = new LinkedHashSet<>(factors);
		this.factorsThatFired = set();
		this.factorsAndVariablesRelation = manyToManyRelation(factors, Factor::getVariables);
		this.random = random;
	}
	
	public void sampleOrWeight(SamplingFactor factor) {
		if (factors.contains(factor)) {
			factor.sampleOrWeigh(sample);
			factors.remove(factor);
			factorsThatFired.add(factor);
		}
		else {
			// do nothing because factor had already been applied
		}
	}
	
	public void makeSureToConsultAllRelevantInputFactors() {
		// println("Making sure all factors are applied");
		SamplingFactor unfiredButRelevantSamplingFactor;
		while ((unfiredButRelevantSamplingFactor = getUnfiredButRelevantSamplingFactor()) != null) {
			unfiredButRelevantSamplingFactor.sampleOrWeigh(sample);
			factorsThatFired.add(unfiredButRelevantSamplingFactor);
		}

	}
	
	private SamplingFactor getUnfiredButRelevantSamplingFactor() {
		SamplingFactor result = 
				getFirstNonNullResultOrNull(
						sample.getVariables(), 
						v -> getUnfiredButRelevantSamplingFactor(factorsOn(v)));
		return result;
		
	}
	
	private Iterable<? extends SamplingFactor> factorsOn(Variable variable) {
		return factorsAndVariablesRelation.getAsOfB(variable);
	}

	private SamplingFactor getUnfiredButRelevantSamplingFactor(Iterable<? extends SamplingFactor> factors) {
		SamplingFactor result = 
				getFirstSatisfyingPredicateOrNull(
						factors, f -> isUnfiredButRelevantSamplingFactor(f));
		return result;
	}

	private boolean isUnfiredButRelevantSamplingFactor(SamplingFactor factor) {
		boolean result = 
				!factorsThatFired.contains(factor) 
				&& 
				isRelevant(factor);
		return result;
	}

	private boolean isRelevant(SamplingFactor factor) {
		return thereExists(factor.getVariables(), sample::instantiates);
	}

	/**
	 * Returns the state's sample in unmodified form; only the state itself is allowed to modify the sample
	 * in order to do proper bookkeeping on which factors have been used.
	 */
	public Sample getUnmodifiableSample() {
		return new UnmodifiableSample(sample);
	}

	@Override
	public Random getRandom() {
		return random;
	}

}
