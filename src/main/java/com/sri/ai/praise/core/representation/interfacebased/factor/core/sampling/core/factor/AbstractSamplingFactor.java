package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor.ZERO_FACTOR;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.util.explanation.tree.ExplanationTree;

public abstract class AbstractSamplingFactor implements SamplingFactor {
	
	@Override
	public abstract void sampleOrWeigh(Sample sample);

	protected abstract SamplingRuleSet makeSamplingRules();
	
	private List<? extends Variable> variables;
	
	private SamplingRuleSet samplingRules;
	
	private Random random;

	public AbstractSamplingFactor(List<? extends Variable> variables, Random random) {
		this.variables = variables;
		this.random = random;
		myAssert(getRandom() != null, () -> this.getClass().getSimpleName() + " must have a Random");
	}

	@Override
	public SamplingRuleSet getSamplingRuleSet() {
		if (samplingRules == null) {
			samplingRules = makeSamplingRules();
		}
		return samplingRules;
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
		if (another.isIdentity()) {
			return this;
		}
		if (another.isZero()) {
			return ZERO_FACTOR;
		}
		SamplingFactor anotherSamplingFactor = checkType(another);
		SamplingProductFactor result = new SamplingProductFactor(arrayList(this, anotherSamplingFactor), getRandom());
		return result;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		Factor result;
		if (variablesToSumOut.isEmpty()) {
			result = this;
		}
		else {
			result = new SamplingMarginalizingFactor(variablesToSumOut, this, getRandom());
		}
		return result;
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

	@Override
	public String nestedString(int level, boolean rules) {
		return fill(level*4, ' ') + this + rulesString(level, rules);
	}
}
