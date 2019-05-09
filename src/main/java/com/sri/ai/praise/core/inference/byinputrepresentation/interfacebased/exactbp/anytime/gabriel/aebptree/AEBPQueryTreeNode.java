package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebptree;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.Polytopes;

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
		Polytope message = super.messageSent(boxIt);
		Polytope result = Polytopes.getEquivalentAtomicPolytopeOn(this.root, message);
		return result;
	}
	//----------- Add Node -----------
	public void addNodeToTheTree(AEBPFactorTreeNode newFactorNode, HashMap<Object, AEBPTreeNode<?, ?>> mapFromNodeToPartition) {
		addNodeToTheTree(newFactorNode, mapFromNodeToPartition,true);
	}
	public void addNodeToTheTree(AEBPFactorTreeNode newFactorNode, HashMap<Object, AEBPTreeNode<?, ?>> mapFromNodeToPartition, boolean cleverUpdating) {
		addNode(newFactorNode);
		
		if(!cleverUpdating) {
			updateByRecomputingAllSeparators();
		}
		else {
			updateEverythingByComputingOnlyNecessaryOperations(newFactorNode,mapFromNodeToPartition);
		}
	}

	private void addNode(AEBPFactorTreeNode newFactorNode) {
		newFactorNode.getParent().addChild(newFactorNode);
	}
	
	//----------- Update variables -----------
	//"Brute Force" update
	public void updateByRecomputingAllSeparators() {
		this.computeSetOfFactorsAndVariables();		
		this.computeSeparatorsAndN();
	}
	
	
	/**
	 * we update:
	 * 	- setOfFactors and setOfVariables 
	 * 	- separators
	 * 	- N set (notToSum)
	 * of all nodes that will potentially change
	 * @param newFactorNode
	 * @param mapFromNodeToPartition
	 */
	private void updateEverythingByComputingOnlyNecessaryOperations(AEBPFactorTreeNode newFactorNode, 
			HashMap<Object, AEBPTreeNode<?, ?>> mapFromNodeToPartition) {
		updateNonExhaustedNodes(newFactorNode,mapFromNodeToPartition);	
		updateSepAndN(newFactorNode);
	}
	
	// NOT TESTED
	private void updateNonExhaustedNodes(AEBPFactorTreeNode newFactorNode, HashMap<Object, AEBPTreeNode<?, ?>> mapFromNodeToPartition) {
		// Get the intersection of the non exhausted variables and the variables of the new factor. Those variables were for sure non exhausted (by definition)
		LinkedHashSet<Variable> nonExhaustedVariablesFromNewFactorBeforeAddingTheNewFactor = 
				getNonExhaustedVariablesFromNewFactorBeforeAddingNewFactor(newFactorNode);
				
		// For each one of them, if they are exhausted now, recompute messages
		for(Variable v: nonExhaustedVariablesFromNewFactorBeforeAddingTheNewFactor) {	//set "recompute message" = true
			if(this.isExhausted.apply(v)) {
				AEBPVariableTreeNode varTreeNode = (AEBPVariableTreeNode) mapFromNodeToPartition.get(v);
				setRecomputeNeededForItAndAllAncestors(varTreeNode);
			}
		}
	}

	private <RootNode, ParentNode> void setRecomputeNeededForItAndAllAncestors(AbstractAEBPTreeNode<RootNode, ParentNode>  node) {
		if(node!= null && node.needToUpdateTheApproximation == false) {
			node.needToUpdateTheApproximation = true;
			setRecomputeNeededForItAndAllAncestors(node.parent);
		}
	}

	private LinkedHashSet<Variable> getNonExhaustedVariablesFromNewFactorBeforeAddingNewFactor(
			AEBPFactorTreeNode newFactorNode) {
		LinkedHashSet<Variable> nonExhaustedVariablesFromNewFactorBeforeAddingTheNewFactor = new LinkedHashSet<>(newFactorNode.getRoot().getVariables());
		nonExhaustedVariablesFromNewFactorBeforeAddingTheNewFactor.retainAll(this.setOfVariables);
		return nonExhaustedVariablesFromNewFactorBeforeAddingTheNewFactor;
	}	

	// NOT TESTED
	private void updateSepAndN(AEBPFactorTreeNode newFactorNode) {
		updateSeparatorAndNForNecessaryNodes(newFactorNode);
		updateSetOfVariablesAndFactors(newFactorNode);
	}

	private void updateSetOfVariablesAndFactors(AEBPFactorTreeNode newFactorNode) {
		updateSetOfFactorsOnTheAncestors(newFactorNode,newFactorNode.getRoot());//I think we don't need that
		LinkedHashSet<Variable> variablesFromNewFactor = new LinkedHashSet<>(newFactorNode.getRoot().getVariables());
		updateSetOfVariablesOnTheNodeAndAllAncestors(newFactorNode,variablesFromNewFactor );
	}

	private void updateSeparatorAndNForNecessaryNodes(AEBPFactorTreeNode newFactorNode) {
		//// recompute messages from children
		//for(AbstractAEBPTreeNode<Variable, Factor> variableNode : newFactorNode.getChildren()) {
		//	variableNode.needToUpdateTheApproximation = true;
		//}
		//newFactorNode.needToUpdateTheApproximation = true;
		List<? extends Variable> variablesFromNewFactor = newFactorNode.getRoot().getVariables();
		updateSeparatorAndNForNecessaryNodesOfAncestors(newFactorNode,variablesFromNewFactor);
	}
	
	private <RootNode, ParentNode> void updateSeparatorAndNForNecessaryNodesOfAncestors(
			AbstractAEBPTreeNode<RootNode,ParentNode> node,
			List<? extends Variable> variablesOfNewFactor) {
		
		node.needToUpdateTheApproximation = true;
		if(node.getParent() == null) {
			return;
		}
		AbstractAEBPTreeNode<ParentNode, RootNode> nodeParent = node.parent;

		// We first check if there an increase on the separator
		LinkedHashSet<Variable> variablesToAddToSeparator = 
				getVariablesToAddToSeparator(node, variablesOfNewFactor);
		
		// If the separator is going to increase, we update it and update the N set below
		if(!variablesToAddToSeparator.isEmpty()) {
			nodeParent.separator.addAll(variablesToAddToSeparator);
			for(AbstractAEBPTreeNode<RootNode, ParentNode> ch : nodeParent.getChildren()) {
				LinkedHashSet<Variable> variablesToAddToN = new LinkedHashSet<>(variablesToAddToSeparator);
				updateN(ch,variablesToAddToN);
			}
		}
		updateSeparatorAndNForNecessaryNodesOfAncestors(nodeParent,variablesOfNewFactor);
	}

	private <RootNode, ParentNode> LinkedHashSet<Variable> getVariablesToAddToSeparator(
			AbstractAEBPTreeNode<RootNode, ParentNode> node, List<? extends Variable> variablesOfNewFactor) {
		LinkedHashSet<Variable> variablesToAddToSeparator = new LinkedHashSet<>(variablesOfNewFactor);
		LinkedHashSet<Variable> variablesFromOtherBranches = new LinkedHashSet<>(node.parent.setOfVariables);
		variablesFromOtherBranches.removeAll(node.setOfVariables);
		variablesToAddToSeparator.retainAll(variablesFromOtherBranches);
		return variablesToAddToSeparator;
	}

	private <RootNode, ParentNode> void updateN(AbstractAEBPTreeNode<RootNode, ParentNode> node,
			LinkedHashSet<Variable> variablesToAddToN) {
		variablesToAddToN.removeAll(node.notToSum);
		if(!variablesToAddToN.isEmpty()) {
			node.notToSum.addAll(variablesToAddToN);
			if (necessaryToRecompute(node, variablesToAddToN)) {
				node.needToUpdateTheApproximation = true;
			}
			
			for(AbstractAEBPTreeNode<ParentNode, RootNode> ch : node.children) {
				LinkedHashSet<Variable> variablesToAddToNCh = new LinkedHashSet<>(variablesToAddToN);
				updateN(ch,variablesToAddToNCh);
			}
		}
	}

	private <RootNode, ParentNode> boolean necessaryToRecompute(AbstractAEBPTreeNode<RootNode, ParentNode> node,
			LinkedHashSet<Variable> variablesToAddToN) {
		for(Variable v : variablesToAddToN) {
			if(node.setOfVariables.contains(v)) {
				return true;
			}
		}
		return false;
	}

	// NOT TESTED
	private <RootNode, ParentNode> void updateSetOfVariablesOnTheNodeAndAllAncestors(
			AbstractAEBPTreeNode<RootNode, ParentNode> node, 
			LinkedHashSet<Variable> variablesFromNewFactor) {
		if(node !=null && !variablesFromNewFactor.isEmpty()) {
			//remove variables already on the set of variables: they appear all the way to the top and therefore do not need to be added
			variablesFromNewFactor.removeAll(node.setOfVariables);
			node.setOfVariables.addAll(variablesFromNewFactor);
			AbstractAEBPTreeNode<ParentNode,RootNode> nodeParent = node.parent;
			updateSetOfVariablesOnTheNodeAndAllAncestors( nodeParent, variablesFromNewFactor);
		}
	}
	// NOT TESTED
	private <RootNode,ParentNode> void updateSetOfFactorsOnTheAncestors(AbstractAEBPTreeNode<RootNode, ParentNode> node, Factor newFactor) {
		if(node != null) {
			node.setOfFactors.add(newFactor);
			AbstractAEBPTreeNode<ParentNode,RootNode> nodeParent = node.parent;
			updateSetOfFactorsOnTheAncestors(nodeParent, newFactor);
		}
	}

	
	
}
