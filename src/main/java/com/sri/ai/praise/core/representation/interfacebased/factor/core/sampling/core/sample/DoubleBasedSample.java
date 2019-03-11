package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample;

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Assignment;

public class DoubleBasedSample extends DefaultSample {
	
	public DoubleBasedSample() {
		super(new DoubleImportanceFactory(), new DoublePotentialFactory());
	}
	
	public static DoubleBasedSample doubleBasedSample(Object... variablesAndValues) {
		Map<Variable, Object> assignmentMap = map(variablesAndValues);
		Assignment assignment = new DefaultAssignment(assignmentMap);
		DoubleBasedSample result = new DoubleBasedSample();
		result.setAssignment(assignment);
		return result;
	}

}
