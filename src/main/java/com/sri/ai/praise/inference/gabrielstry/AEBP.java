package com.sri.ai.praise.inference.gabrielstry;

import java.util.Iterator;

import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPTreeNode;
import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPVariableTreeNode;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.inference.representation.core.AbstractFactorNetwork;
import com.sri.ai.util.collect.EZIterator;
import com.sri.ai.util.computation.anytime.api.Approximation;

public class AEBP extends EZIterator<Approximation<Factor>> {
	AEBPModel model;
	AEBPVariableTreeNode tree;//Tree built from the query
	
	Iterator<AEBPTreeNode<Factor, Variable>> getNextNodeToPutOnTheTree;
	
	public AEBP(AbstractFactorNetwork network, 
			AbstractFactorNetwork emptyNetwork, //TODO : 
			Variable query,Iterator<AEBPTreeNode<Factor, Variable>> getNextNodeToPutOnTheTree) {
		this.model = new AEBPModel(network, emptyNetwork, query);
				
		tree = new AEBPVariableTreeNode(query, null,(v) -> this.model.isExhausted(v));
		
		this.getNextNodeToPutOnTheTree = getNextNodeToPutOnTheTree;
	}

	@Override
	protected Approximation<Factor> calculateNext() {
		expand();
		Approximation<Factor> result = computeInference();
		return result;
	}

	private Approximation<Factor> computeInference() {
		return tree.messageSent();
	}

	private void expand() {
		//Get next factor (and it's children variables)
		AEBPTreeNode<Factor, Variable> nextTreeNodeToAddToTheTree = getNextNodeToPutOnTheTree.next();
		//Add new factor to model
		model.ExpandModel(nextTreeNodeToAddToTheTree.getRoot());
		//add new factor to the tree
		nextTreeNodeToAddToTheTree.getParent().addChild(nextTreeNodeToAddToTheTree);
		//update separators, and everything (except the messages themselves)
		updateSeparatorsAndMessagesThatNeedToBeRecomputed();
	}

	private void updateSeparatorsAndMessagesThatNeedToBeRecomputed() {
		// TODO Auto-generated method stub
		
	}
	
	
	
}
