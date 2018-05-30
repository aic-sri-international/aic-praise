package com.sri.ai.praise.learning.parameterlearning;

import java.util.List;
import com.sri.ai.praise.inference.generic.representation.api.Variable;
import com.sri.ai.praise.inference.generic.representation.api.Factor;

public interface BayesianNode extends Factor {
	
	public Variable getChild();
	
	public List<? extends Variable> getParents();
	
	public List<? extends Variable> getAllVariables();
	
	// "Parameter" bellow is understood as the conditional probability P (childVariable = childValue | parentsVariables = parentsValues)
	
	default public void setParametersGivenCompleteData(Dataset dataset) {
		List<? extends Variable> childAndParentsVariables = this.getAllVariables();
		
		this.setInitialCountsForAllPossibleChildAndParentsAssignments();
		
		for(Datapoint datapoint : dataset.getDatapoints()) {
			List<? extends Object> childAndParentsValues = datapoint.getValuesOfVariables(childAndParentsVariables);
			this.incrementCountForChildAndParentsAssignment(childAndParentsValues);
		}
		
		this.normalizeParameters();
	}
	
	public void setInitialCountsForAllPossibleChildAndParentsAssignments();
	
	public void incrementCountForChildAndParentsAssignment(List<? extends Object> childAndParentsValues);
	
	public void normalizeParameters();
	
}
