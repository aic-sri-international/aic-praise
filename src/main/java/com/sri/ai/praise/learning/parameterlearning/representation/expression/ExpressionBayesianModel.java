package com.sri.ai.praise.learning.parameterlearning.representation.expression;

import java.util.List;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.ExpressionFactorNetwork;
import com.sri.ai.praise.learning.parameterlearning.BayesianModel;
import com.sri.ai.praise.learning.parameterlearning.BayesianNode;

public class ExpressionBayesianModel extends ExpressionFactorNetwork implements BayesianModel {

	private List<ExpressionBayesianNode> nodes;
	
	public ExpressionBayesianModel(List<ExpressionBayesianNode> nodes, Context context) {
		super(nodes, context);
		this.nodes = nodes;
	}

	@Override
	public List<? extends BayesianNode> getNodes() {
		return nodes;
	}
	
}
