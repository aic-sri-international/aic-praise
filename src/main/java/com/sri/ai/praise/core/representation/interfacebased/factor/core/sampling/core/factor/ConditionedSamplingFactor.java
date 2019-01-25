package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.util.Util.collectToArrayList;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map.Entry;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public interface ConditionedSamplingFactor extends SamplingFactor {

	public static SamplingFactor condition(SamplingFactor samplingFactor, Sample conditioningSample) {

		if (conditioningSample.size() == 0) {
			return samplingFactor;
		}

		return (ConditionedSamplingFactor) java.lang.reflect.Proxy.newProxyInstance(
				samplingFactor.getClass().getClassLoader(),
				new Class[] { ConditionedSamplingFactor.class },
				new ConditionedSamplingFactorProxyInvocationHandler(samplingFactor, conditioningSample));		
	}

	public static class ConditionedSamplingFactorProxyInvocationHandler implements InvocationHandler {

		private SamplingFactor samplingFactor;
		private Sample conditioningSample;
		private ArrayList<Variable> variables;

		public ConditionedSamplingFactorProxyInvocationHandler(SamplingFactor samplingFactor, Sample conditioningSample) {
			this.samplingFactor = samplingFactor;
			this.conditioningSample = conditioningSample;
			Collection<? extends Variable> variablesInAssignment = conditioningSample.getAssignment().mapValue().keySet();
			this.variables = collectToArrayList(samplingFactor.getVariables(), v -> ! variablesInAssignment.contains(v));
		}

		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if (method.getName().equals("sampleOrWeigh")) {
				sampleOrWeigh((Sample) args[0]);
				return null;
			}
			else if (method.getName().equals("getVariables")) {
				return Collections.unmodifiableList(variables);
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
	}
}
