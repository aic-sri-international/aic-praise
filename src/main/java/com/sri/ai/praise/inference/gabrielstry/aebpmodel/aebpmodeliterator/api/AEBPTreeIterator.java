package com.sri.ai.praise.inference.gabrielstry.aebpmodel.aebpmodeliterator.api;

import java.util.Iterator;

import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPFactorTreeNode;
import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPQueryTreeNode;

public interface AEBPTreeIterator extends Iterator<AEBPFactorTreeNode>{
	public AEBPQueryTreeNode getRootOfTree();
}
