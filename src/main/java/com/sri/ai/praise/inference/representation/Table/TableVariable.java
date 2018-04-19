package com.sri.ai.praise.inference.representation.Table;

import java.util.ArrayList;
import java.util.List;

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
		return "{" + this.name + ":card=" + this.cardinality + "}";
	}
	
	@Override
	public List<? extends Object> getValues() {
		ArrayList<Integer> result = new ArrayList<>();
		for (int i = 0; i < this.getCardinality(); i++) {
			result.add(i);
		}
		return result;
	}
}
