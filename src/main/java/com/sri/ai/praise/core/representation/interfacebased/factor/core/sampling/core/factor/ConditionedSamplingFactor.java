package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Map.Entry;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;

public interface ConditionedSamplingFactor extends SamplingFactor {

		public static ConditionedSamplingFactor condition(SamplingFactor samplingFactor, Sample conditioningSample) {
			
		     return (ConditionedSamplingFactor) java.lang.reflect.Proxy.newProxyInstance(
		             samplingFactor.getClass().getClassLoader(),
		             new Class[] { ConditionedSamplingFactor.class },
		             new ConditionedSamplingFactorProxyInvocationHandler(samplingFactor, conditioningSample));		
		}

		public static class ConditionedSamplingFactorProxyInvocationHandler implements InvocationHandler {

			private SamplingFactor samplingFactor;
			private Sample conditioningSample;
			
			public ConditionedSamplingFactorProxyInvocationHandler(SamplingFactor samplingFactor, Sample conditioningSample) {
				this.samplingFactor = samplingFactor;
				this.conditioningSample = conditioningSample;
			}
			
			@Override
			public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
				if (method.getName().equals("sampleOrWeigh")) {
					sampleOrWeigh((Sample) args[0]);
					return null;
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
