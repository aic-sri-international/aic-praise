package com.sri.ai.praise.inference.treebased.representation.Table;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.inference.treebased.representation.api.Variable;

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
	
	@Override
	public boolean equals(Object obj) {
		if (obj == this) return true;
        if (!(obj instanceof TableVariable)) {
            return false;
        }
        
        TableVariable v = (TableVariable) obj;
        
		return 	this.name.equals(v.name)
				&& 
				this.cardinality.equals(v.cardinality);
	}
	@Override
	public int hashCode() {
		int result = 17;
		result = 31 * result + name.hashCode();
		result = 31 * result + cardinality.hashCode();
		return result;
	}
}
