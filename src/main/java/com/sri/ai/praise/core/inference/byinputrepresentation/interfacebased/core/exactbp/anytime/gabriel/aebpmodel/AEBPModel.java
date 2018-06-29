package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.aebpmodel;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.aebptree.AEBPTreeNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.EditableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;

public class AEBPModel {

	EditableFactorNetwork entrireNetwork;//It would be good not to have to get functionality direcly from
										 //many to manyRelations to use a network.
	EditableFactorNetwork partialNetwork;
	
	private HashMap<Object, AEBPTreeNode<?, ?>> fromNodeToPartition;

	Variable query;
	
	public AEBPModel(EditableFactorNetwork network, 
			Variable query) {
		this.entrireNetwork = network;
		this.partialNetwork=  network.makeEmptyNetwork();
		this.query = query;
		
		this.fromNodeToPartition = new LinkedHashMap<>();
	}
	
	public boolean isExhausted(Variable v) {
		return partialNetwork.getNeighbors(v).containsAll(entrireNetwork.getNeighbors(v));
	}
	
	//I think we'll not use it TODO : see and maybe remove it
	public void ExpandModel(Iterator<Factor> it) {
		// BFS, DFS,...
		if (it.hasNext()) {
			Factor factor = it.next();
			ExpandModel(factor);
		}
	}
	
	public void ExpandModel(Factor factor) {
		// BFS, DFS,...
			if(!entrireNetwork.containsFactor(factor)) {
				//throw error
				throw new Error();
			}
			for(Variable v : factor.getVariables()) {
				partialNetwork.add(factor,v);
			}
	}
	
	//Secondary:
	public Variable getQuery() {
		return query;
	}
	public void setQuery(Variable query) {
		this.query = query;
	}
	
	public EditableFactorNetwork getEntireGraph() {
		return this.entrireNetwork;
	}
	
	public HashMap<Object, AEBPTreeNode<?, ?>> getMapFromNodeToPartition() {
		return fromNodeToPartition;
	}
}
