package com.sri.ai.praise.learning.parameterlearning;

import java.util.List;
import com.sri.ai.praise.inference.generic.representation.api.Variable;
import com.sri.ai.praise.inference.generic.representation.api.Factor;

public interface BayesianNode extends Factor {
	
	public Variable getChild();
	
	public List<? extends Variable> getParents();
	
	// "Parameter" bellow is understood as the conditional probability P (childVariable = childValue | parentsVariables = parentsValues)
	
	default public void setParametersGivenCompleteData(Dataset dataset) {
		Variable childVariable = this.getChild();
		List<? extends Variable> parentsVariables = this.getParents();
		
		this.setInitialCountsForAllPossibleChildAndParentsAssignments();
		for(Datapoint datapoint : dataset.getDatapoints()) {
			Object childValue = datapoint.getValueOfVariable(childVariable);
			List<? extends Object> parentsValues = datapoint.getValueOfVariables(parentsVariables);
			this.incrementCountForChildAndParentsAssignment(childValue, parentsValues);
		}
		this.normalizeParametersAndFillEntries();
	}
	
	public void setInitialCountsForAllPossibleChildAndParentsAssignments();
	
	public void incrementCountForChildAndParentsAssignment(Object childValue, List<? extends Object> parentsValues);
	
	public void normalizeParametersAndFillEntries();
	
}
