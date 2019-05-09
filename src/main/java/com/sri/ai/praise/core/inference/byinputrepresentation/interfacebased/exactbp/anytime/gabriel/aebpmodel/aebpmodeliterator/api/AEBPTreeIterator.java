package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebpmodel.aebpmodeliterator.api;

import java.util.Iterator;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebptree.AEBPFactorTreeNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebptree.AEBPQueryTreeNode;

public interface AEBPTreeIterator extends Iterator<AEBPFactorTreeNode>{
	public AEBPQueryTreeNode getRootOfTree();
}
