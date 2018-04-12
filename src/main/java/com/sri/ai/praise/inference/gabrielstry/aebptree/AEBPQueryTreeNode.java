package com.sri.ai.praise.inference.gabrielstry.aebptree;

import java.util.ArrayList;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public class AEBPQueryTreeNode extends AEBPVariableTreeNode{

	public AEBPQueryTreeNode(Variable query,
			Function<Variable, Boolean> isExhausted) {
		super(query, null, isExhausted);
	}

	public AEBPQueryTreeNode(Variable query, 
			Function<Variable, Boolean> isExhausted, ArrayList<AbstractAEBPTreeNode<Factor, Variable>> children) {
		super(query, null, isExhausted, children);
		// TODO Auto-generated constructor stub
	}

	//----------- Add Node -----------
	
	public void addNodeToTheTree(AEBPFactorTreeNode newFactorNode) {
		newFactorNode.getParent().addChild(newFactorNode);
		//check if the children of the factorNode are in the tree already and stuff...
		//we should add all variables that are in phi minus \mT as children of the node 
		
		ArrayList<AbstractAEBPTreeNode<Variable,Factor>> childrenOfNewNode = 
				new ArrayList<>();
		for(Variable v : newFactorNode.getRoot().getVariables()) {
			if(!this.setOfVariables.contains(v)) {
				childrenOfNewNode.add(new AEBPVariableTreeNode(v,newFactorNode,isExhausted));
			}
		}
		
		//update due TODO
		this.updateByRecomputingAllSeparators();
	}
	
	//----------- Update variables -----------
	//"Brute Force" update
	public void updateByRecomputingAllSeparators() {
		this.computeSetOfFactorsAndVariables();		
		this.computeSeparatorsAndN();
	}
}
