package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.util.Util.arrayList;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.util.explanation.tree.ExplanationTree;

public abstract class AbstractSamplingFactor implements SamplingFactor {
	
	@Override
	public abstract void sample(Sample sample);

	private List<? extends Variable> variables;
	
	private Random random;

	public AbstractSamplingFactor(List<? extends Variable> variables, Random random) {
		this.variables = variables;
		this.random = random;
	}

	public Random getRandom() {
		return random;
	}

	@Override
	public boolean contains(Variable variable) {
		boolean result = getVariables().contains(variable);
		return result;
	}

	@Override
	public List<? extends Variable> getVariables() {
		return variables;
	}

	@Override
	public Factor multiply(Factor another) {
		SamplingFactor anotherSamplingFactor = checkType(another);
		SamplingProductFactor result = new SamplingProductFactor(arrayList(this, anotherSamplingFactor), getRandom());
		return result;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		return this;
	}

	@Override
	public boolean isIdentity() {
		return false;
	}

	@Override
	public boolean isZero() {
		return false;
	}

	@Override
	public Double getEntryFor(Map<? extends Variable, ? extends Object> variableInstantiations) {
		throw new Error("getEntryFor not supported for " + getClass());
	}

	@Override
	public Factor normalize() {
		return this; 
	}

	@Override
	public Factor add(Factor another) {
		SamplingFactor anotherSamplingFactor = checkType(another);
		return new SamplingSumFactor(arrayList(this, anotherSamplingFactor), getRandom());
	}

	@Override
	public Factor invert() {
		throw new Error("invert not supported for " + getClass());
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		throw new Error("max not supported for " + getClass());
	}

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
		throw new Error("argmax not supported for " + getClass());
	}

	@Override
	public Factor min(Collection<? extends Variable> variablesToMinimize) {
		throw new Error("min not supported for " + getClass());
	}

	@Override
	public Factor argmin(Collection<? extends Variable> variablesToMinimize) {
		throw new Error("argmin not supported for " + getClass());
	}

	@Override
	public ExplanationTree getExplanation() {
		// TODO: remove explanation awareness from ExactBP and make it a transparent feature of factors.
		return null;
	}

	@Override
	public void setExplanation(ExplanationTree explanation) {
		// TODO: remove explanation awareness from ExactBP and make it a transparent feature of factors.
	}

	private SamplingFactor checkType(Factor another) throws Error {
		SamplingFactor anotherSamplingFactor;
		try {
			anotherSamplingFactor = (SamplingFactor) another;
		}
		catch (ClassCastException e) {
			throw new Error(getClass() + " only interoperates with sampling factors, but got " + another);
		}
		return anotherSamplingFactor;
	}

}
