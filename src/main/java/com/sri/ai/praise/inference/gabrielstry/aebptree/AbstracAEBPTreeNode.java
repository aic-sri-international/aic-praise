package com.sri.ai.praise.inference.gabrielstry.aebptree;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.computation.anytime.api.Approximation;

public abstract class AbstracAEBPTreeNode<RootNode, ParentNode> implements AEBPTreeNode<RootNode, ParentNode>{
	boolean updateNeeded;
	Approximation<Factor> currentApproximation;
	
	static Function<Variable, Boolean> isExhausted;

	@Override
	public Approximation<Factor> messageSent() {
		// TODO Auto-generated method stub
		return null;
	}
}
