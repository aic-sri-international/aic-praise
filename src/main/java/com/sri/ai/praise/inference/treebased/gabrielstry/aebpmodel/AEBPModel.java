package com.sri.ai.praise.inference.treebased.gabrielstry.aebpmodel;

import java.util.Iterator;

import com.sri.ai.praise.inference.treebased.gabrielstry.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.inference.treebased.representation.api.Factor;
import com.sri.ai.praise.inference.treebased.representation.api.Variable;

public class AEBPModel {

	EditableFactorNetwork entrireNetwork;//It would be good not to have to get functionality direcly from
										 //many to manyRelations to use a network.
	EditableFactorNetwork partialNetwork;
	
	Variable query;
	
	public AEBPModel(EditableFactorNetwork network, 
			Variable query) {
		this.entrireNetwork = network;
		this.partialNetwork=  network.makeEmptyNetwork();
		this.query = query;
	}
	
	public boolean isExhausted(Variable v) {
		return partialNetwork.getNeighbors(v).containsAll(entrireNetwork.getNeighbors(v));
	}
	
	//I think we'll not use it TODO : see and sipa remove it
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
				//throw exception
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
	
}
