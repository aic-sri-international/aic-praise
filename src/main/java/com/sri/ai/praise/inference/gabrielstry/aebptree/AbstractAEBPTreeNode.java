package com.sri.ai.praise.inference.gabrielstry.aebptree;

import static com.sri.ai.praise.inference.anytimeexactbp.polytope.core.Polytopes.identityPolytope;
import static com.sri.ai.praise.inference.representation.core.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.collect.NestedIterator.nestedIterator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Collectors;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.Polytope;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.Polytopes;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.Simplex;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

public abstract class AbstractAEBPTreeNode<RootNode, ParentNode> implements AEBPTreeNode<RootNode, ParentNode>{
	//Node information
	RootNode root;
	AEBPTreeNode<ParentNode,RootNode> parent;
	List<AEBPTreeNode<ParentNode,RootNode>> children;
	
	//Info about the approximation
	boolean needToUpdateTheApproximation;
	Polytope currentApproximation;
	
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
	public Polytope messageSent() {
		if(!needToUpdateTheApproximation) {
			return currentApproximation;
		}
		
		needToUpdateTheApproximation = false;
		
		Polytope product = computeProductOfFactorAtRootTimesTheIncomingMessages();
		Collection<? extends Variable> allFreeVariablesInProduct = product.getFreeVariables();
		List<? extends Variable> variablesToBeSummedOut = getVariablesToBeSummedOut(allFreeVariablesInProduct);
		Polytope result = Polytopes.sumOut(variablesToBeSummedOut, product);
		return result;
	}

	
	private List<? extends Variable> getVariablesToBeSummedOut(Collection<? extends Variable> allFreeVariablesInProduct) {
		List<Variable> variablesToBeSummedOut = new ArrayList<>(this.separator);
		variablesToBeSummedOut.removeAll(this.notToSum);
		return variablesToBeSummedOut;
	}


	private Polytope computeProductOfFactorAtRootTimesTheIncomingMessages() {
		List<Polytope> polytopesToMultiply= new ArrayList<>(children.size());
		//for(AEBPTreeNode<ParentNode, RootNode> child : children) {childrenMessages.add(child.messageSent());}
		polytopesToMultiply = children.stream().map(AEBPTreeNode::messageSent).collect(Collectors.toList());

		//P.S: if the root is a factor: add {(on:) root} to the list; if is a non exhausted variable, add a Simplex(root)
		addSimplexOrFactortoTheListOfProducts(polytopesToMultiply);
		
		Polytope result = accumulate(polytopesToMultiply, Polytope::multiply, identityPolytope());
		return result;
	}

	public void addSimplexOrFactortoTheListOfProducts(List<Polytope> polytopesToMultiply) {
		IntensionalConvexHullOfFactors singletonConvexHullOfFactorAtRoot = 
				new IntensionalConvexHullOfFactors(list(),(Factor) this.getRoot());
		if(isRootAFactor()) {
			polytopesToMultiply.add(singletonConvexHullOfFactorAtRoot);
		}
		else if (!isExhausted.apply((Variable) this.getRoot())) {
			polytopesToMultiply.add(new Simplex((Variable) this.getRoot()));
		}
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
