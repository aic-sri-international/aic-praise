package com.sri.ai.praise.inference.gabrielstry;

import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import java.util.Iterator;

import com.sri.ai.praise.inference.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.inference.representation.core.AbstractFactorNetwork;

public class AEBPModel {

	AbstractFactorNetwork entrireNetwork;//It'd be good not to have to get functionality direcly from
										 //many to manyRelations to use a network.
	EditableFactorNetwork partialNetwork;
	
	Variable query;
	
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
			if(!entrireNetwork.containsA(identityWrapper(factor))) {
				//throw exception
			}
			partialNetwork.addFactor(factor);
	}
	
	//Secondary:
	public Variable getQuery() {
		return query;
	}
	public void setQuery(Variable query) {
		this.query = query;
	}
	
}
