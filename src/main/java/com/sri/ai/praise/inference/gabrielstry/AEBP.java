package com.sri.ai.praise.inference.gabrielstry;

import java.util.Iterator;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPTreeNode;
import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPVariableTreeNode;
import com.sri.ai.praise.inference.gabrielstry.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.collect.EZIterator;

public class AEBP extends EZIterator<Factor> {
	AEBPModel model;
	AEBPVariableTreeNode tree;//Tree built from the query
	
	Iterator<AEBPTreeNode<Factor, Variable>> getNextNodeToPutOnTheTree;
	
	public AEBP(EditableFactorNetwork network, Variable query,
			Iterator<AEBPTreeNode<Factor, Variable>> getNextNodeToPutOnTheTree) {
		this.model = new AEBPModel(network, query);
				
		Function<Variable,Boolean> isExhausted = (v) -> this.model.isExhausted(v);
		
		tree = new AEBPVariableTreeNode(query, null, isExhausted);
		
		this.getNextNodeToPutOnTheTree = getNextNodeToPutOnTheTree;
	}

	@Override
	protected Factor calculateNext() {
		expand();
		Factor result = computeInference();
		return result;
	}

	private Factor computeInference() {
		return tree.messageSent();
	}

	private void expand() {
		//Get next factor (and it's children variables)
		AEBPTreeNode<Factor, Variable> nextTreeNodeToAddToTheTree = getNextNodeToPutOnTheTree.next();
		//Add new factor to model
		model.ExpandModel(nextTreeNodeToAddToTheTree.getRoot());
		//Add new factor to the tree: this is equivalent to retrieving the parent node of the 
		//new node (N) and add N as a child of N.parent
		nextTreeNodeToAddToTheTree.getParent().addChild(nextTreeNodeToAddToTheTree);
		//Update separators, and everything else (except the messages themselves)
		updateSeparatorsTheBooleanFlagAndNotToSumSet();
	}

	private void updateSeparatorsTheBooleanFlagAndNotToSumSet() {
		// TODO Auto-generated method stub
		
	}
}
