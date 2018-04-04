package com.sri.ai.praise.inference.representation.Table;

import com.sri.ai.praise.inference.representation.api.Variable;

public class TableVariable implements Variable {
	private String name;
	private Integer cardinality;
	
	public TableVariable(String variableName, int cardinality) {
		this.name = variableName;
		this.cardinality=cardinality;
	}
	public String getName() { return this.name;}
	public int getCardinality() { return this.cardinality;}
	
	@Override
	public String toString() {
		return "{name: " + this.name + ", card: " + this.cardinality + "}";
	}
}
