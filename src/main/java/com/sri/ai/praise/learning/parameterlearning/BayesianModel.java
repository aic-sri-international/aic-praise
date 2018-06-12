package com.sri.ai.praise.learning.parameterlearning;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;

public interface BayesianModel extends FactorNetwork {
	
	public List<? extends BayesianNode> getNodes();
	
	default public void learnModelParametersFromCompleteData(Dataset dataset) {
		List<? extends BayesianNode> nodes = this.getNodes();
		for(BayesianNode node : nodes) {
			node.setParametersGivenCompleteData(dataset);
		}
	}

}
    

