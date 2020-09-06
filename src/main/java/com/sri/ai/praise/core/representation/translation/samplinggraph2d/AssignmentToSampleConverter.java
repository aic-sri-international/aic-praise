package com.sri.ai.praise.core.representation.translation.samplinggraph2d;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

import java.util.LinkedList;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.util.function.api.values.Value;
import com.sri.ai.util.function.api.variables.Assignment;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Variable;

/**
 * A utility converting a (possibly partial) {@link Assignment} to a {@link Sample}.
 * <p>
 * IMPORTANT: the order of the Function variables must be the same as the order of the variables in the sampling factor
 * (this is how the correspondence is made).
 * 
 */ 
public class AssignmentToSampleConverter {
	
	private static final LinkedList<String> BOOLEAN_CONSTANT_STRINGS = list("true", "false");
	private SamplingFactor samplingFactor;
	private SetOfVariables setOfVariables;
	
	public AssignmentToSampleConverter(
			SamplingFactor samplingFactor,
			SetOfVariables setOfVariables) {
		
		this.samplingFactor = samplingFactor;
		this.setOfVariables = setOfVariables;

		myAssert(
				samplingFactor.getVariables().size() == setOfVariables.size(), 
				() -> getClass() + " requires sampling factor variables and and given variables have a one-to-one correspondence.");
		
	}

	//////////////////////////////

	public static Sample getCorrespondingSample(SamplingFactor samplingFactor, SetOfVariables setOfVariables, Assignment assignment) {
		AssignmentToSampleConverter converter = new AssignmentToSampleConverter(samplingFactor, setOfVariables);
		Sample sample = converter.makeCorrespondingSample(assignment);
		return sample;
	}

	//////////////////////////////

	public Sample makeCorrespondingSample(Assignment assignment) {
		int numberOfVariables = samplingFactor.getVariables().size();
		Sample sample = DefaultSample.makeFreshSample();
		putValuesOfAssignedVariablesInSample(assignment, numberOfVariables, sample);
		return sample;
	}

	private void putValuesOfAssignedVariablesInSample(Assignment assignment, int numberOfVariables, Sample sample) {
		for (int i = 0; i != numberOfVariables; i++) {
			putValueOfIthVariableInSample(assignment, i, sample);
		}
	}

	private void putValueOfIthVariableInSample(Assignment assignment, int i, Sample sample) {
		Value value = getValueOfIthVariable(assignment, i);
		if (value != null) {
			setValueInIthSamplingFactorVariableInSample(value, i, sample);
		}
	}

	private Value getValueOfIthVariable(Assignment assignment, int i) {
		Variable variable = setOfVariables.getVariables().get(i);
		Value value = assignment.get(variable);
		return value;
	}

	private void setValueInIthSamplingFactorVariableInSample(Value value, int i, Sample sample) {
		com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable
		samplingFactorVariable = 
				samplingFactor.getVariables().get(i);
		Object valueInTypeUsedByAssignment = value.objectValue();
		Object valueInTypeUsedBySamplingFactors = convertValueForAssignmentsToValueForSamplingFactors(valueInTypeUsedByAssignment);
		sample.getAssignment().set(samplingFactorVariable, valueInTypeUsedBySamplingFactors);
	}

	private Object convertValueForAssignmentsToValueForSamplingFactors(Object valueInTypeUsedByAssignment) {
		Object valueInTypeUsedBySamplingFactors;
		if (valueInTypeUsedByAssignment instanceof Number && !(valueInTypeUsedByAssignment instanceof Integer)) {
			valueInTypeUsedBySamplingFactors = ((Number) valueInTypeUsedByAssignment).doubleValue();
		}
		else if (valueInTypeUsedByAssignment instanceof String && BOOLEAN_CONSTANT_STRINGS.contains(valueInTypeUsedByAssignment)) {
			String string = (String) valueInTypeUsedByAssignment;
			valueInTypeUsedBySamplingFactors = string == "true"? true : false;
		}
		else {
			valueInTypeUsedBySamplingFactors = valueInTypeUsedByAssignment;
		}
		return valueInTypeUsedBySamplingFactors;
	}

}
