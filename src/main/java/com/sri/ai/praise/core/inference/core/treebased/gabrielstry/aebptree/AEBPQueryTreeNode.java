package com.sri.ai.praise.core.inference.core.treebased.gabrielstry.aebptree;

import java.util.ArrayList;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.praise.core.inference.core.treebased.anytimeexactbp.polytope.api.Polytope;
import com.sri.ai.praise.core.inference.core.treebased.anytimeexactbp.polytope.core.Polytopes;
import com.sri.ai.praise.core.model.pure.api.Factor;
import com.sri.ai.praise.core.model.pure.api.Variable;

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

	
	@Override
	public Polytope messageSent(Predicate<Polytope> boxIt) {
		// TODO Auto-generated method stub
		Polytope message = super.messageSent(boxIt);
		Polytope result = Polytopes.getEquivalentAtomicPolytopeOn(this.root, message);
		return result;
	}
	//----------- Add Node -----------
	
	public void addNodeToTheTree(AEBPFactorTreeNode newFactorNode) {
		addNode(newFactorNode);
		//update due TODO
		updateByRecomputingAllSeparators();
	}

	public void addNode(AEBPFactorTreeNode newFactorNode) {
		newFactorNode.getParent().addChild(newFactorNode);
	}
	
	//----------- Update variables -----------
	//"Brute Force" update
	public void updateByRecomputingAllSeparators() {
		this.computeSetOfFactorsAndVariables();		
		this.computeSeparatorsAndN();
	}
}
