package com.sri.ai.praise.inference.gabrielstry;

import static com.sri.ai.util.Util.println;

import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.Polytope;
import com.sri.ai.praise.inference.gabrielstry.aebpmodel.AEBPModel;
import com.sri.ai.praise.inference.gabrielstry.aebpmodel.aebpmodeliterator.BFS;
import com.sri.ai.praise.inference.gabrielstry.aebpmodel.aebpmodeliterator.api.AEBPTreeIterator;
import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPFactorTreeNode;
import com.sri.ai.praise.inference.gabrielstry.aebptree.AEBPQueryTreeNode;
import com.sri.ai.praise.inference.gabrielstry.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.inference.representation.Table.TableFactor;
import com.sri.ai.praise.inference.representation.Table.TableFactorNetwork;
import com.sri.ai.praise.inference.representation.Table.TableVariable;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.collect.EZIterator;

public class AEBP extends EZIterator<Polytope> {
	AEBPModel model;
	AEBPQueryTreeNode tree;//Tree built from the query
	
	AEBPTreeIterator getNextNodeToPutOnTheTree;
	
	public AEBP(EditableFactorNetwork network, 
			Variable query,
			Function<AEBPModel,AEBPTreeIterator> getNextNodeToPutOnTheTree) {
		this.model = new AEBPModel(network, query);
		this.getNextNodeToPutOnTheTree = getNextNodeToPutOnTheTree.apply(model);
		tree = this.getNextNodeToPutOnTheTree.getRootOfTree();
	}
	
	public AEBP(EditableFactorNetwork network, Variable query) {
		this(network, query, model -> new BFS(model));
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
		return tree.messageSent();
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
	
	public static void main(String[] args) {
		List<TableFactor> factors = TestCases.isingModelGridWithRandomWeigthsAndPotetial(3, 2);
		
		Variable query = null;
		for(TableFactor f : factors) {
			for(TableVariable v : f.getVariables()) {
				if(v.getName().equals("1_1") ){
					query = v;
					//Util.println("Query not null");
				}
			}
			//Util.println(f.getVariables());	
		}
		TableFactorNetwork tfn = new TableFactorNetwork(factors);
		
		/*BFS bfs = new BFS(new AEBPModel(tfn, query));
		
		println(bfs.getRootOfTree());
		while(bfs.hasNext()) {
			println(bfs.next().getRoot().getVariables());
		}*/
		
		AEBP aebp = new AEBP(tfn, query);
		while(aebp.hasNext()) {
			Polytope result = aebp.next();
			println(result);
		}
	}
}
