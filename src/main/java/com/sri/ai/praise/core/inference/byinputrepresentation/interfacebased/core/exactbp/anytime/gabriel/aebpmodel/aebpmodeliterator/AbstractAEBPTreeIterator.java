package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.aebpmodel.aebpmodeliterator;

import java.util.HashMap;

import com.google.common.base.Function;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.aebpmodel.AEBPModel;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.aebpmodel.aebpmodeliterator.api.AEBPTreeIterator;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.aebptree.AEBPQueryTreeNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.aebptree.AEBPTreeNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public abstract	class AbstractAEBPTreeIterator implements AEBPTreeIterator{
	 AEBPModel model;
	 Function<Variable,Boolean> isExhausted;
	 AEBPQueryTreeNode root;
	

	 protected HashMap<Object, AEBPTreeNode<?, ?>> fromNodeToPartition;
	 
	 public AbstractAEBPTreeIterator(AEBPModel model) {
		 this.model = model;
		 isExhausted = (v) -> this.model.isExhausted(v);
		 root = new AEBPQueryTreeNode(model.getQuery(), isExhausted);
		 this.fromNodeToPartition = model.getMapFromNodeToPartition();
	 }
	
	@Override
	public AEBPQueryTreeNode getRootOfTree() {
		return root;
	}

}
