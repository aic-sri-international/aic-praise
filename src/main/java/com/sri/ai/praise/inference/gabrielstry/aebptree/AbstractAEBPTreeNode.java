package com.sri.ai.praise.inference.gabrielstry.aebptree;

import static com.sri.ai.praise.inference.anytimeexactbp.polytope.core.Polytopes.identityPolytope;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.Polytope;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.Polytopes;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.Simplex;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.base.NullaryFunction;

public abstract class AbstractAEBPTreeNode<RootNode, ParentNode> implements AEBPTreeNode<RootNode, ParentNode>{
	//Node information
	RootNode root;
	AbstractAEBPTreeNode<ParentNode,RootNode> parent;
	ArrayList<AbstractAEBPTreeNode<ParentNode,RootNode>> children;
	
	//Info about the approximation
	boolean needToUpdateTheApproximation;
	Polytope currentApproximation;
	
	//Oracle capable of telling if a variable is exhausted or not...
	Function<Variable, Boolean> isExhausted;

	//Separator and NotToSum Sets
	LinkedHashSet<Variable> separator;
	LinkedHashSet<Variable> notToSum;//Noted as "N" set on the paper
	
	//Variables that make the computations and updates faster
	LinkedHashSet<Variable> setOfVariables;//those are not the variables on the tree, but Var(setOfFactors)
	LinkedHashSet<Factor> setOfFactors;

	public AbstractAEBPTreeNode(RootNode root, AbstractAEBPTreeNode<ParentNode,RootNode> parent, 
			Function<Variable, Boolean> isExhausted) {
		this(root,parent,isExhausted,new ArrayList<>());
	}
	
	public AbstractAEBPTreeNode(RootNode root, AbstractAEBPTreeNode<ParentNode,RootNode> parent,
			Function<Variable, Boolean> isExhausted,
			ArrayList<AbstractAEBPTreeNode<ParentNode,RootNode>> children) {
		needToUpdateTheApproximation = true;
		currentApproximation = null;
		
		this.root = root;
		this.parent = parent;
		this.children = children;
		this.isExhausted = isExhausted;

		this.separator = null;
		this.notToSum = null;
		this.setOfVariables = null;
		this.setOfFactors = null;
	}
	
	// ----- Compute set Of Factors / set of Variables -------
	
	protected void computeSetOfFactorsAndVariables() {
		for(AbstractAEBPTreeNode<ParentNode, RootNode> ch:children) {
			ch.computeSetOfFactorsAndVariables();
		}
		this.needToUpdateTheApproximation = true;
		this.computeSetOfFactorsAndVariablesAtThisNode();
	}

	private void computeSetOfFactorsAndVariablesAtThisNode() {
		this.setOfFactors = computeSetOfFactors();
		this.setOfVariables = computeSetOfVariables();
	}

	private LinkedHashSet<Factor> computeSetOfFactors() {
		LinkedHashSet<Factor> result = new LinkedHashSet<Factor>();
		if(isRootAFactor()) {
			result.add((Factor) this.root);
		}
		for(AbstractAEBPTreeNode<ParentNode, RootNode> ch : children) {
			result.addAll(ch.setOfFactors);
		}
		return result;
	}

	private LinkedHashSet<Variable> computeSetOfVariables() {
		LinkedHashSet<Variable> result = new LinkedHashSet<>();
		for(Factor f : setOfFactors) {
			if(!f.equals(this.root)) {
				result.addAll(f.getVariables());
			}
		}
		return result;
	}

	// ----- Compute separators and N -----
	
	protected void computeSeparatorsAndN() {
		this.computeSeparatorsAndNAtThisNode();
		for(AbstractAEBPTreeNode<ParentNode, RootNode> ch:children) {
			ch.computeSeparatorsAndN();
		}
	}

	private void computeSeparatorsAndNAtThisNode() {
		this.separator = this.computeSeparatorAtThisNode();
		this.notToSum = this.computeNAtThisNode();
	}

	private LinkedHashSet<Variable> computeSeparatorAtThisNode() {
		LinkedHashSet<Variable> separator = new LinkedHashSet<>();
		for(int i = 0; i < children.size();i++) {
			for(int j = i+1; j < children.size();j++) {
				LinkedHashSet<Variable> aux = new LinkedHashSet<>(children.get(i).setOfVariables);
				aux.retainAll(children.get(j).setOfVariables);
				separator.addAll(aux);
			}
		}
		return separator;
	}

	private LinkedHashSet<Variable> computeNAtThisNode() {
		LinkedHashSet<Variable> result = new LinkedHashSet<>();
		if(this.parent != null) {
			result.addAll(this.parent.notToSum);
			result.addAll(this.parent.separator);
			if(this.parent.isRootAVariable()) {
				result.add((Variable) this.parent.getRoot());
			}
		}
		return result;
	}
	
	//----------- Message -----------
	
	@Override
	public Polytope messageSent(NullaryFunction<Boolean> propagateBoxes) {
		if(!needToUpdateTheApproximation) {
			return currentApproximation;
		}
		
		needToUpdateTheApproximation = false;
		
		Polytope product = computeProductOfFactorAtRootTimesTheIncomingMessages(propagateBoxes);
		//Collection<? extends Variable> allFreeVariablesInProduct = product.getFreeVariables();
		List<? extends Variable> variablesToBeSummedOut = getVariablesToBeSummedOut();
		Polytope result = Polytopes.sumOut(variablesToBeSummedOut, product);
		return result;
	}
	
	private List<? extends Variable> getVariablesToBeSummedOut(){//(Collection<? extends Variable> allFreeVariablesInProduct) {
		List<Variable> variablesToBeSummedOut = new ArrayList<>(this.separator);
		if(this.isRootAFactor()) {
			variablesToBeSummedOut.addAll(((Factor)this.getRoot()).getVariables());//TODO add get variables less parents
			variablesToBeSummedOut.remove((Variable) this.parent.getRoot());
		}
		variablesToBeSummedOut.removeAll(this.notToSum);
		return variablesToBeSummedOut;
	}

	private Polytope computeProductOfFactorAtRootTimesTheIncomingMessages(NullaryFunction<Boolean> propagateBoxes) {
		List<Polytope> polytopesToMultiply= new ArrayList<>(children.size());
		for(AEBPTreeNode<ParentNode, RootNode> child : children) {
			polytopesToMultiply.add(child.messageSent(propagateBoxes));
		}
		
		//P.S: if the root is a factor: add {(on:) root} to the list; if is a non exhausted variable, add a Simplex(root)
		addSimplexOrFactortoTheListOfProducts(polytopesToMultiply,propagateBoxes);
		
		Polytope result = accumulate(polytopesToMultiply, Polytope::multiply, identityPolytope());
		
		return result;
	}

	public void addSimplexOrFactortoTheListOfProducts(List<Polytope> polytopesToMultiply, NullaryFunction<Boolean> propagateBoxes) {
		if(isRootAFactor()) {
			if(propagateBoxes.apply()) {
				//Add n
				//polytopesToMultiply.add(new Box((Factor) this.getRoot(),(Factor) this.getRoot()));
			}
			else {
				IntensionalConvexHullOfFactors singletonConvexHullOfFactorAtRoot = 
						new IntensionalConvexHullOfFactors(list(),(Factor) this.getRoot());
				polytopesToMultiply.add(singletonConvexHullOfFactorAtRoot);
			}
		}
		else if (!isExhausted.apply((Variable) this.getRoot())) {
			polytopesToMultiply.add(new Simplex((Variable) this.getRoot()));
		}
	}
	
	//----------- Basic Functions ----------- 
	public AEBPTreeNode<ParentNode, RootNode> getParent() {
		return this.parent;
	}
	
	public List<AEBPTreeNode<ParentNode,RootNode>> getChildren(){
		return null;//TODO this.children;
	}
	
	public void addChild(AEBPTreeNode<ParentNode, RootNode> node) {
		this.children.add((AbstractAEBPTreeNode<ParentNode, RootNode>) node);
	}
	
	public RootNode getRoot() {
		return this.root;
	}
	
	@Override
	public String toString() {
		return root.toString();
	}
}
