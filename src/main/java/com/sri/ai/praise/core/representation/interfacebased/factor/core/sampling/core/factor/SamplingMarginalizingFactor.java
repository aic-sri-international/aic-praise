package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.ProjectionOfSetOfSamplingRules.project;
import static com.sri.ai.util.Util.fill;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.subtract;

import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;

public class SamplingMarginalizingFactor extends AbstractSamplingFactor {

	private List<? extends Variable> marginalizedVariables;
	
	private SamplingFactor marginalizedFactor;
	
	public SamplingMarginalizingFactor(List<? extends Variable> marginalizedVariables, SamplingFactor marginalizedFactor, Random random) {
		super(subtract(marginalizedFactor.getVariables(), marginalizedVariables), random);
		this.marginalizedVariables = marginalizedVariables;
		this.marginalizedFactor = marginalizedFactor;
	}

	public List<? extends Variable> getMarginalizedVariables() {
		return marginalizedVariables;
	}

	public SamplingFactor getMarginalizedFactor() {
		return marginalizedFactor;
	}

	@Override
	public void sampleOrWeigh(Sample sampleToComplete) {
		if (marginalizedFactor instanceof DynamicSamplingProductFactor) { // TODO: too hard-wired; create interface for factors that can sample a subset of its variables
			((DynamicSamplingProductFactor) marginalizedFactor).sampleOrWeigh(getVariables(), sampleToComplete);
		}
		else {
			marginalizedFactor.sampleOrWeigh(sampleToComplete);
		}
	}

	@Override
	public SamplingRuleSet makeSamplingRules() {
		List<Variable> remainingVariables = subtract(getMarginalizedFactor().getVariables(), getMarginalizedVariables());
		SamplingRuleSet marginalSamplingRules = project(remainingVariables, this, getMarginalizedFactor());
		return marginalSamplingRules;
	}

	@Override
	public String toString() {
		return "sum_{" + join(getMarginalizedVariables()) + "} " + getMarginalizedFactor();
	}

	@Override
	public	String nestedString(int level, boolean rules) {
		String tab = fill(level*4, ' ');
		return
				tab + "sum_{" + join(getMarginalizedVariables()) + "}\n"
				+ getMarginalizedFactor().nestedString(level + 1, rules)
				+ rulesString(level, rules);
	}

}
