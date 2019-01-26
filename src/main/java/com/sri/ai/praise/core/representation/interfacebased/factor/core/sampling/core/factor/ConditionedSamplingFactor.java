package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.collectToArrayList;
import static com.sri.ai.util.Util.makeProxy;
import static java.util.stream.Collectors.toCollection;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map.Entry;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;

public interface ConditionedSamplingFactor extends SamplingFactor {

	public static SamplingFactor condition(SamplingFactor samplingFactor, Sample conditioningSample) {

		if (conditioningSample.size() == 0) {
			return samplingFactor;
		}

		return makeProxy(ConditionedSamplingFactor.class, samplingFactor, conditioningSample);		
	}

	public static class ConditionedSamplingFactorProxyInvocationHandler implements InvocationHandler {

		private SamplingFactor samplingFactor;
		private Sample conditioningSample;
		private ArrayList<Variable> conditionedVariables;
		private SamplingRuleSet conditionedSamplingRuleSet;
		public ConditionedSamplingFactor proxy;

		public ConditionedSamplingFactorProxyInvocationHandler(SamplingFactor samplingFactor, Sample conditioningSample) {
			this.samplingFactor = samplingFactor;
			this.conditioningSample = conditioningSample;
			this.conditionedVariables = makeConditionedVariables(samplingFactor, conditioningSample);
			this.conditionedSamplingRuleSet = makeConditionedSamplingRuleSet();
		}

		private ArrayList<Variable> makeConditionedVariables(SamplingFactor samplingFactor, Sample conditioningSample) {
			Collection<? extends Variable> variablesInAssignment = conditioningSample.getAssignment().mapValue().keySet();
			ArrayList<Variable> conditionedVariables = collectToArrayList(samplingFactor.getVariables(), v -> ! variablesInAssignment.contains(v));
			return conditionedVariables;
		}

		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if (method.getName().equals("sampleOrWeigh")) {
				sampleOrWeigh((Sample) args[0]);
				return null;
			}
			else if (method.getName().equals("getVariables")) {
				return Collections.unmodifiableList(conditionedVariables);
			}
			else if (method.getName().equals("getSamplingRuleSet")) {
				return conditionedSamplingRuleSet;
			}
			else if (method.getDeclaringClass().isAssignableFrom(SamplingFactor.class)) {
				Object result = method.invoke(samplingFactor, args);
				return result;
			}
			else {
				throw new Error(getClass() + " received method '" + method + "' of " + method.getDeclaringClass() + " which it is not prepared to execute.");
			}
		}

		private void sampleOrWeigh(Sample sample) {
			for(Entry<Variable, Object> entry : conditioningSample.getAssignment().mapValue().entrySet()) {
				sample.getAssignment().set(entry.getKey(), entry.getValue());
			}
			samplingFactor.sampleOrWeigh(sample);
		}
		
		private SamplingRuleSet makeConditionedSamplingRuleSet() {
			ArrayList<SamplingRule> rules = 
					getOriginalSamplingRules().stream()
					.map(this::conditionOrNull)
					.filter(c -> c != null)
					.collect(toCollection(() -> arrayList()));
			return new DefaultSamplingRuleSet(rules);
		}

		private ArrayList<? extends SamplingRule> getOriginalSamplingRules() {
			return samplingFactor.getSamplingRuleSet().getSamplingRules();
		}
		
		private SamplingRule conditionOrNull(SamplingRule rule) {
			ArrayList<SamplingGoal> conditionedConsequents = getConditionedConsequents(rule);
			if (conditionedConsequents.isEmpty()) {
				return null;
			}
			else {
				ArrayList<SamplingGoal> conditionedAntecedents = getConditionedAntecedents(rule);
				return new SamplingRule(proxy, conditionedConsequents, conditionedAntecedents, rule.getEstimatedSuccessWeight());
			}
		}

		private ArrayList<SamplingGoal> getConditionedConsequents(SamplingRule rule) {
			return collectToArrayList(rule.getConsequents(), g -> ! conditioningSample.instantiates(getVariable(g)));
		}

		private ArrayList<SamplingGoal> getConditionedAntecedents(SamplingRule rule) {
			return collectToArrayList(rule.getAntecendents(), g -> ! g.isSatisfied(conditioningSample));
		}

		private Variable getVariable(SamplingGoal variableIsDefinedGoal) {
			return ((VariableIsDefinedGoal) variableIsDefinedGoal).getVariable();
		}

	}
}
