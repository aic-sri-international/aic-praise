package com.sri.ai.praise.inference.gabrielstry.aebptree;

import java.util.List;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.util.computation.anytime.api.Approximation;

public interface AEBPTreeNode<RootNode,ParentNode> {
	public Approximation<Factor> messageSent();
	
	RootNode getRoot();
	AEBPTreeNode<ParentNode,RootNode> getParent();
	List<AEBPTreeNode<ParentNode,RootNode>> getChildren();
	
	void addChild(AEBPTreeNode<ParentNode, RootNode> node);
}
