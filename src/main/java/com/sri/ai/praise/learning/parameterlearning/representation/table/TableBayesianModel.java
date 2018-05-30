package com.sri.ai.praise.learning.parameterlearning.representation.table;

import java.util.List;

import com.sri.ai.praise.inference.generic.representation.table.TableFactorNetwork;
import com.sri.ai.praise.learning.parameterlearning.BayesianModel;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;

public class TableBayesianModel extends TableFactorNetwork implements BayesianModel {
	
	private List<TableBayesianNode> nodes;
	
	public TableBayesianModel(List<TableBayesianNode> nodes) {
		super(nodes);
		this.nodes = nodes;
	}
	
	public List<? extends BayesianNode> getNodes() {
		return nodes;
	}
	
}
