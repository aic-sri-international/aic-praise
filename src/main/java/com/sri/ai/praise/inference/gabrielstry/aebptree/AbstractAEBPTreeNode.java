package com.sri.ai.praise.inference.gabrielstry.aebptree;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.computation.anytime.api.Approximation;

public abstract class AbstractAEBPTreeNode<RootNode, ParentNode> implements AEBPTreeNode<RootNode, ParentNode>{
	//Node information
	RootNode root;
	AEBPTreeNode<ParentNode,RootNode> parent;
	List<AEBPTreeNode<ParentNode,RootNode>> children;
	
	//Info about the approximation
	boolean needToUpdateTheApproximation;
	Approximation<Factor> currentApproximation;
	
	//Oracle capable of telling if a variable is exhausted or not...
	Function<Variable, Boolean> isExhausted;

	//Separator and NotToSum Sets
	LinkedHashSet<Variable> separator;
	LinkedHashSet<Variable> notToSum;//Noted as "N" set on the paper
	
	//Variables that make the computations and updates faster
	LinkedHashSet<Variable> setOfVariables;
	LinkedHashSet<Factor> setOfFactors;

	public AbstractAEBPTreeNode(RootNode root,AEBPTreeNode<ParentNode,RootNode> parent, Function<Variable, Boolean> isExhausted) {
		this(root,parent,isExhausted,new ArrayList<>());
	}

	
	public AbstractAEBPTreeNode(RootNode root,AEBPTreeNode<ParentNode,RootNode> parent,Function<Variable, Boolean> isExhausted,
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
	
	@Override
	public Approximation<Factor> messageSent() {
		// TODO Auto-generated method stub
		return null;
	}

	
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
