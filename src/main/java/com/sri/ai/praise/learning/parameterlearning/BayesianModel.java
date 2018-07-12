package com.sri.ai.praise.learning.parameterlearning;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;

public interface BayesianModel extends FactorNetwork {
	
	public List<? extends BayesianNode> getNodes();
	
	default public BayesianModel learnModelParametersFromCompleteData(Dataset dataset) {
		BayesianModel learnedModel = this.copy();
		List<? extends BayesianNode> nodes = learnedModel.getNodes();
		for(BayesianNode node : nodes) {
			node.setParametersGivenCompleteData(dataset);
		}
		
		return learnedModel;
	}
	
	public BayesianModel copy();

}
    

