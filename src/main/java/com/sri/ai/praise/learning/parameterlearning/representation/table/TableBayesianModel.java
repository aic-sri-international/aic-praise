package com.sri.ai.praise.learning.parameterlearning.representation.table;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactorNetwork;
import com.sri.ai.praise.learning.parameterlearning.BayesianModel;

public class TableBayesianModel extends TableFactorNetwork implements BayesianModel {
	
	private List<TableBayesianNode> nodes;
	
	public TableBayesianModel(List<TableBayesianNode> nodes) {
		super(nodes);
		this.nodes = nodes;
	}
	
	public List<? extends TableBayesianNode> getNodes() {
		return nodes;
	}
	
}
