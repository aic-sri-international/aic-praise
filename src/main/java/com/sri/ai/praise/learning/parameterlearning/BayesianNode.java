package com.sri.ai.praise.learning.parameterlearning;

import java.util.List;

// Would be the equivalent of Factor
public interface BayesianNode {
	
	public Variable getVariable();
	
	public List<? extends Variable> getParents();
	
	// "Parameter" bellow is understood as the conditional probability P (thisNodeVariable = thisNodeValue | parents = parentsValues)
	
	default public void setParametersGivenCompleteData(Dataset dataset) {
		Variable nodeVariable = this.getVariable();
		List<? extends Variable> parentsVariables = this.getParents();
		
		this.setInitialCountsForAllPossibleNodeAndParentsAssignments();
		for(Datapoint datapoint : dataset.getDatapoints()) {
			Object nodeValue = datapoint.getValueOfVariable(nodeVariable);
			List<? extends Object> parentsValues = datapoint.getValueOfVariables(parentsVariables);
			this.incrementCountForNodeAndParentsAssignment(nodeValue, parentsValues);
		}
		this.normalizeParameters();
	}
	
	public void setInitialCountsForAllPossibleNodeAndParentsAssignments();
	
	public void incrementCountForNodeAndParentsAssignment(Object nodeValue, List<? extends Object> parentsValues);
	
	public void normalizeParameters();
	
}
