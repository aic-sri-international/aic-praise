package com.sri.ai.praise.inference.treebased.gabrielstry.aebpmodel.aebpmodeliterator;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.treebased.gabrielstry.aebpmodel.AEBPModel;
import com.sri.ai.praise.inference.treebased.gabrielstry.aebpmodel.aebpmodeliterator.api.AEBPTreeIterator;
import com.sri.ai.praise.inference.treebased.gabrielstry.aebptree.AEBPQueryTreeNode;
import com.sri.ai.praise.inference.treebased.representation.api.Variable;

public abstract	class AbstractAEBPTreeIterator implements AEBPTreeIterator{
	 AEBPModel model;
	 Function<Variable,Boolean> isExhausted;
	 AEBPQueryTreeNode root;
	
	 public AbstractAEBPTreeIterator(AEBPModel model) {
		 this.model = model;
		 isExhausted = (v) -> this.model.isExhausted(v);
		 root = new AEBPQueryTreeNode(model.getQuery(), isExhausted);
	 }
	
	@Override
	public AEBPQueryTreeNode getRootOfTree() {
		return root;
	}

}
