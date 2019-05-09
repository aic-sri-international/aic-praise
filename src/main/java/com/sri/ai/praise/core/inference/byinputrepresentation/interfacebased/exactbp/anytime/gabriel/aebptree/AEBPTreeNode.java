package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebptree;

import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;

public interface AEBPTreeNode<RootNode,ParentNode> {
	
	Polytope messageSent(Predicate<Polytope> boxIt);
	
	RootNode getRoot();
	AEBPTreeNode<ParentNode,RootNode> getParent();
//	List<AEBPTreeNode<ParentNode,RootNode>> getChildren();
	
	void addChild(AEBPTreeNode<ParentNode, RootNode> node);

	
	default boolean isRootAFactor() {
		return getRoot() instanceof Factor;
	}
	default boolean isRootAVariable() {
		return getRoot() instanceof Variable;
	}

}
