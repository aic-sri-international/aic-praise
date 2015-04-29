package com.sri.ai.praise.sgsolver.demo.service;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.sgsolver.model.ConstantDeclaration;
import com.sri.ai.praise.sgsolver.model.HOGMRandomVariableDeclaration;
import com.sri.ai.praise.sgsolver.model.HOGMSortDeclaration;

public class ParsedModel {
	private String                              inputModel            = null;
	private List<HOGMSortDeclaration>           sorts                 = new ArrayList<>();
	private List<ConstantDeclaration>           constants             = new ArrayList<>();
	private List<HOGMRandomVariableDeclaration> randoms               = new ArrayList<>();
	private List<Expression>                    conditionedPotentials = new ArrayList<>();
	
	public ParsedModel(String inputModel, List<HOGMSortDeclaration> sorts, List<ConstantDeclaration> constants, List<HOGMRandomVariableDeclaration> randoms, List<Expression> conditionedPotentials) {
		this.inputModel = inputModel;
		this.sorts.addAll(sorts);
		this.constants.addAll(constants);
		this.randoms.addAll(randoms);
		this.conditionedPotentials.addAll(conditionedPotentials);
	}

	public String getInputModel() {
		return inputModel;
	}

	public List<HOGMSortDeclaration> getSortDeclarations() {
		return sorts;
	}
	
	public List<ConstantDeclaration> getConstatDeclarations() {
		return constants;
	}

	public List<HOGMRandomVariableDeclaration> getRandomVariableDeclarations() {
		return randoms;
	}

	public List<Expression> getConditionedPotentials() {
		return conditionedPotentials;
	}
}
