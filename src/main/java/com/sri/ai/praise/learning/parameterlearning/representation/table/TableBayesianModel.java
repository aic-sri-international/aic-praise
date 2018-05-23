package com.sri.ai.praise.learning.parameterlearning.representation.table;

import java.util.List;

import com.sri.ai.praise.learning.parameterlearning.BayesianModel;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;

public class TableBayesianModel implements BayesianModel {
	
	private List<TableBayesianNode> nodes;
	
	public TableBayesianModel(List<TableBayesianNode> nodes) {
		this.nodes = nodes;
	}
	
	public List<? extends BayesianNode> getNodes() {
		return nodes;
	}

}
