package com.sri.ai.praise.inference.gabrielstry;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.Polytope;
import com.sri.ai.praise.inference.gabrielstry.aebpmodel.AEBPModel;
import com.sri.ai.praise.inference.gabrielstry.aebpmodel.aebpmodeliterator.BFS;
import com.sri.ai.praise.inference.gabrielstry.aebpmodel.aebpmodeliterator.api.AEBPTreeIterator;
import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPFactorTreeNode;
import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPQueryTreeNode;
import com.sri.ai.praise.inference.gabrielstry.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.EZIterator;

/**
 * A AEBP (anytime exact belief propagation) is an iterator of polytopes.
 * 
 * It contains a model (which expands a network and provides a "isExhausted" function).
 * 
 * It also contains a tree, over which the messages are stored.
 * 
 * @author gabriel
 *
 */

public class AEBP extends EZIterator<Polytope> {
	AEBPModel model;
	AEBPQueryTreeNode tree;
	
	NullaryFunction<Boolean> propagateBoxes;
	
	AEBPTreeIterator getNextNodeToPutOnTheTree;// This iterator determines the expansion of the tree:
									// getNextNodeToPutOnTheTree.next() gives a treeNode pointing to the parent it has to be attached to
	
	public AEBP(EditableFactorNetwork network, 
			Variable query,
			Function<AEBPModel,AEBPTreeIterator> getNextNodeToPutOnTheTree,boolean useBoxes) {
		this.model = new AEBPModel(network, query);
		this.getNextNodeToPutOnTheTree = getNextNodeToPutOnTheTree.apply(model);
		tree = this.getNextNodeToPutOnTheTree.getRootOfTree();
		propagateBoxes = () -> useBoxes;
	}
	
	public AEBP(EditableFactorNetwork network, Variable query,boolean useBoxes) {
		this(network, query, model -> new BFS(model), useBoxes);
	}
	public AEBP(EditableFactorNetwork network, Variable query) {
		this(network, query, model -> new BFS(model), false);
	}

	@Override
	protected Polytope calculateNext() {
		if(!getNextNodeToPutOnTheTree.hasNext()) {
			return null;
		}
		expand();
		Polytope result = computeInference();
		return result;
	}

	private Polytope computeInference() {
		return tree.messageSent(propagateBoxes);
		// Normalize ? TODO
	}

	private void expand() {
		//Get next factor (and it's children variables)
		AEBPFactorTreeNode nextTreeNodeToAddToTheTree = getNextNodeToPutOnTheTree.next();
		//Add new factor to model
		model.ExpandModel(nextTreeNodeToAddToTheTree.getRoot());
		//Add new factor to the tree
		tree.addNodeToTheTree(nextTreeNodeToAddToTheTree);
	}
}
