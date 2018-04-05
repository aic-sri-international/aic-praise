package com.sri.ai.praise.inference.gabrielstry;

import static com.sri.ai.util.base.IdentityWrapper.identityWrapper;

import java.util.Iterator;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.inference.representation.core.AbstractFactorNetwork;

public class AEBPModel {

	AbstractFactorNetwork entrireNetwork;//It would be good not to have to get functionality direcly from
										 //many to manyRelations to use a network.
	AbstractFactorNetwork partialNetwork;
	
	Variable query;
	
	public AEBPModel(AbstractFactorNetwork network, 
			AbstractFactorNetwork emptyNetwork, // TODO I wish to just copy the first network and empty it...
			Variable query) {
		this.entrireNetwork = network;
		this.partialNetwork= emptyNetwork;
		this.partialNetwork.clear();
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
			if(!entrireNetwork.containsA(identityWrapper(factor))) {
				//throw exception
			}
			for(Variable v : factor.getVariables()) {
				partialNetwork.add(identityWrapper(factor),v);
			}
	}
	
	//Secondary:
	public Variable getQuery() {
		return query;
	}
	public void setQuery(Variable query) {
		this.query = query;
	}
	
}
