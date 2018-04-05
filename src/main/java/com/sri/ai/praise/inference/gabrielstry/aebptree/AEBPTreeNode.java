package com.sri.ai.praise.inference.gabrielstry.aebptree;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.util.computation.anytime.api.Approximation;

public interface AEBPTreeNode<RootNode,ParentNode> {
	public Approximation<Factor> messageSent();
}
