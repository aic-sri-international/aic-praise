package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebpmodel.AEBPModel;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebpmodel.aebpmodeliterator.BFS;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebpmodel.aebpmodeliterator.api.AEBPTreeIterator;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebptree.AEBPFactorTreeNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel.aebptree.AEBPQueryTreeNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
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
	
	Predicate<Polytope> boxIt;
	
	AEBPTreeIterator getNextNodeToPutOnTheTree;// This iterator determines the expansion of the tree:
									// getNextNodeToPutOnTheTree.next() gives a treeNode pointing to the parent it has to be attached to
	
	private boolean cleverTrueBruteForceFalse;
	
	public AEBP(EditableFactorNetwork network, 
			Variable query,
			Function<AEBPModel,AEBPTreeIterator> getNextNodeToPutOnTheTree,
			Predicate<Polytope> boxIt,
			boolean cleverTrueBruteForceFalse) {
		this.model = new AEBPModel(network, query);
		this.getNextNodeToPutOnTheTree = getNextNodeToPutOnTheTree.apply(model);
		tree = this.getNextNodeToPutOnTheTree.getRootOfTree();
		this.boxIt = boxIt;
		cleverTrueBruteForceFalse = true;
	}
	
	public AEBP(EditableFactorNetwork network, Variable query, Predicate<Polytope> boxIt, boolean cleverTrueBruteForceFalse) {
		this(network, query, model -> new BFS(model), boxIt,cleverTrueBruteForceFalse);
	}
	public AEBP(EditableFactorNetwork network, Variable query) {
		this(network, query, model -> new BFS(model), (v)->false,true);
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
		return tree.messageSent(boxIt);
	}

	private void expand() {
		//Get next factor (and it's children variables)
		AEBPFactorTreeNode nextTreeNodeToAddToTheTree = getNextNodeToPutOnTheTree.next();
		//Add new factor to model
		model.ExpandModel(nextTreeNodeToAddToTheTree.getRoot());
		//Add new factor to the tree
		tree.addNodeToTheTree(nextTreeNodeToAddToTheTree,model.getMapFromNodeToPartition(),cleverTrueBruteForceFalse);
	}
}
