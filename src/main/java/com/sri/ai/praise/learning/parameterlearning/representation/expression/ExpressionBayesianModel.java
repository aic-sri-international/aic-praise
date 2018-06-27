package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork;
import com.sri.ai.praise.learning.parameterlearning.BayesianModel;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;

public class ExpressionBayesianModel extends ExpressionFactorNetwork implements BayesianModel {

	private List<ExpressionBayesianNode> nodes;
	
	public ExpressionBayesianModel(List<ExpressionBayesianNode> nodes) {
		super(nodes);
		this.nodes = nodes;
	}

	@Override
	public List<? extends BayesianNode> getNodes() {
		return nodes;
	}
	
}
