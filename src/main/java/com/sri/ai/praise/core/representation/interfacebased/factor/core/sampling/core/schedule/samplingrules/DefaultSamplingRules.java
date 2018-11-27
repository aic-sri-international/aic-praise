package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.samplingrules;

import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRules;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;

public class DefaultSamplingRules implements SamplingRules {

	private ArrayList<? extends SamplingRule> samplingRules;
	
	public DefaultSamplingRules(SamplingRule... samplingRules) {
		this(new ArrayList<>(Arrays.asList(samplingRules)));
	}

	public DefaultSamplingRules(ArrayList<? extends SamplingRule> samplingRules) {
		this.samplingRules = samplingRules;
	}

	@Override
	public ArrayList<? extends SamplingRule> getSamplingRules() {
		return samplingRules;
	}

	@Override
	public SamplingRules replaceFactor(SamplingFactor samplingFactor) {
		ArrayList<? extends SamplingRule> newRules = mapIntoArrayList(getSamplingRules(), r -> r.replaceFactor(samplingFactor));
		DefaultSamplingRules result = new DefaultSamplingRules(newRules);
		return result;
	}

	@Override
	public SamplingRules sumOut(List<? extends Variable> variables) {
		ArrayList<? extends SamplingRule> newRules = mapIntoArrayList(getSamplingRules(), r -> r.sumOut(variables));
		newRules.removeIf(r -> r.getConsequents().isEmpty());
		DefaultSamplingRules result = new DefaultSamplingRules(newRules);
		return result;
	}

}
