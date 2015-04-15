package com.sri.ai.praise.sgsolver.demo.service;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.model.ConstantDeclaration;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;

public class ParsedModel {
	private String                          inputModel            = null;
	private List<SortDeclaration>           sorts                 = new ArrayList<>();
	private List<ConstantDeclaration>       constants             = new ArrayList<>();
	private List<RandomVariableDeclaration> randoms               = new ArrayList<>();
	private List<Expression>                conditionedPotentials = new ArrayList<>();
	
	public ParsedModel(String inputModel, List<SortDeclaration> sorts, List<ConstantDeclaration> constants, List<RandomVariableDeclaration> randoms, List<Expression> conditionedPotentials) {
		this.inputModel = inputModel;
		this.sorts.addAll(sorts);
		this.constants.addAll(constants);
		this.randoms.addAll(randoms);
		this.conditionedPotentials.addAll(conditionedPotentials);
	}

	public String getInputModel() {
		return inputModel;
	}

	public List<SortDeclaration> getSortDeclarations() {
		return sorts;
	}
	
	public List<ConstantDeclaration> getConstatDeclarations() {
		return constants;
	}

	public List<RandomVariableDeclaration> getRandomVariableDeclarations() {
		return randoms;
	}

	public List<Expression> getConditionedPotentials() {
		return conditionedPotentials;
	}
}
