package com.sri.ai.praise.inference.representation.Table;

import com.sri.ai.praise.inference.representation.api.Variable;

public class TableVariable implements Variable {
	private Integer index;
	private Integer cardinality;
	
	public TableVariable(int index, int cardinality) {
		this.index=index;
		this.cardinality=cardinality;
	}
	public int getIndex() { return this.index;}
	public int getCardinality() { return this.cardinality;}
}
