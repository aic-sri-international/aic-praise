package com.sri.ai.praise.empiricalevaluation.core;

import com.sri.ai.praise.model.common.io.ModelPage;

public class Problem {
	
	public String query;
	public ModelPage model;
	public String name;
	
	public Problem(String query, ModelPage model) {
		super();
		this.query = query;
		this.model = model;
		this.name = model.getName() + " : " + query;
	}
}