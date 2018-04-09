package com.sri.ai.praise.inference.gabrielstry.aebptree;

import static com.sri.ai.praise.inference.representation.core.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.util.collect.NestedIterator.nestedIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Collectors;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.gabrielstry.Approximations.api.Approximation;
import com.sri.ai.praise.inference.gabrielstry.Approximations.core.Simplex;
import com.sri.ai.praise.inference.gabrielstry.factors.Factors;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public abstract class AbstractAEBPTreeNode<RootNode, ParentNode> implements AEBPTreeNode<RootNode, ParentNode>{
	//Node information
	RootNode root;
	AEBPTreeNode<ParentNode,RootNode> parent;
	List<AEBPTreeNode<ParentNode,RootNode>> children;
	
	//Info about the approximation
	boolean needToUpdateTheApproximation;
	Approximation currentApproximation;
	
	//Oracle capable of telling if a variable is exhausted or not...
	Function<Variable, Boolean> isExhausted;

	//Separator and NotToSum Sets
	LinkedHashSet<Variable> separator;
	LinkedHashSet<Variable> notToSum;//Noted as "N" set on the paper
	
	//Variables that make the computations and updates faster
	LinkedHashSet<Variable> setOfVariables;
	LinkedHashSet<Factor> setOfFactors;

	public AbstractAEBPTreeNode(RootNode root,AEBPTreeNode<ParentNode,RootNode> parent, 
			Function<Variable, Boolean> isExhausted) {
		this(root,parent,isExhausted,new ArrayList<>());
	}
	
	public AbstractAEBPTreeNode(RootNode root,AEBPTreeNode<ParentNode,RootNode> parent,
			Function<Variable, Boolean> isExhausted,
			List<AEBPTreeNode<ParentNode,RootNode>> children) {
		needToUpdateTheApproximation = true;
		currentApproximation = null;
		
		this.root = root;
		this.parent = parent;
		this.children = children;
		this.isExhausted = isExhausted;

		setOfVariables = new LinkedHashSet<>();
		setOfFactors   = new LinkedHashSet<>();
	}
	
	// Message
	
	@Override
	public Approximation messageSent() {
		if(!needToUpdateTheApproximation) {
			return currentApproximation;
		}
		
		needToUpdateTheApproximation = false;
		
		Factor product = computeProductOfFactorAtRootTimesTheIncomingMessages();
		List<? extends Variable> allFreeVariablesInProduct = product.getVariables();
		List<? extends Variable> variablesToBeSummedOut = getVariablesToBeSummedOut(allFreeVariablesInProduct);
		Approximation result = (Approximation) product.sumOut(variablesToBeSummedOut);
		return result;
	}

	
	private List<? extends Variable> getVariablesToBeSummedOut(List<? extends Variable> allFreeVariablesInProduct) {
		// TODO Auto-generated method stub
		return null;
	}


	private Factor computeProductOfFactorAtRootTimesTheIncomingMessages() {
		List<Factor> childrenMessages = new ArrayList<>(children.size());
		//for(AEBPTreeNode<ParentNode, RootNode> child : children) {childrenMessages.add(child.messageSent());}
		childrenMessages = children.stream().map(AEBPTreeNode::messageSent).collect(Collectors.toList());

		Iterator<Factor> messagesToMultiply;
		
		Factor aux = IDENTITY_FACTOR;
		if(isRootAFactor()) {
			aux = (Factor) this.root;
		}
		else if(isExhausted.apply((Variable) this.root)){
			aux = new Simplex((Variable) this.root);
		}
		messagesToMultiply = nestedIterator(aux, childrenMessages);
		Factor result = Factors.multiply(messagesToMultiply);
		
		return result;
	}
	
	//Update variables


	//Basic Functions
	public AEBPTreeNode<ParentNode, RootNode> getParent() {
		return this.parent;
	}
	
	public List<AEBPTreeNode<ParentNode,RootNode>> getChildren(){
		return this.children;
	}
	
	public void addChild(AEBPTreeNode<ParentNode, RootNode> node) {
		this.children.add(node);
	}
	
	public RootNode getRoot() {
		return this.root;
	}
	
	@Override
	public String toString() {
		return root.toString();
	}
}
