/*
 * Copyright (c) 2015, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-praise nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.model.v1.hogm.antlr;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.model.v1.ConstantDeclaration;
import com.sri.ai.praise.model.v1.HOGMRandomVariableDeclaration;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;

@Beta
public class ParsedHOGModel {
	private String                              inputModel            = null;
	private List<HOGMSortDeclaration>           sorts                 = new ArrayList<>();
	private List<ConstantDeclaration>           constants             = new ArrayList<>();
	private List<HOGMRandomVariableDeclaration> randoms               = new ArrayList<>();
	private List<Expression>                    conditionedPotentials = new ArrayList<>();
	
	public ParsedHOGModel(String inputModel, Expression modelTupleExpr) {
		this(inputModel, 
				extractSorts(modelTupleExpr), 
				extractConstants(modelTupleExpr), 
				extractRandom(modelTupleExpr),
				extractConditionedPotentials(modelTupleExpr));
	}
	
	public ParsedHOGModel(String inputModel, List<HOGMSortDeclaration> sorts, List<ConstantDeclaration> constants, List<HOGMRandomVariableDeclaration> randoms, List<Expression> conditionedPotentials) {
		this.inputModel = inputModel;
		if (!sorts.contains(HOGMSortDeclaration.IN_BUILT_BOOLEAN)) {
			this.sorts.add(HOGMSortDeclaration.IN_BUILT_BOOLEAN);
		}
		if (!sorts.contains(HOGMSortDeclaration.IN_BUILT_NUMBER)) {
			this.sorts.add(HOGMSortDeclaration.IN_BUILT_NUMBER);
		}
		this.sorts.addAll(sorts);
		this.constants.addAll(constants);
		this.randoms.addAll(randoms);
		this.conditionedPotentials.addAll(conditionedPotentials);
		
		this.sorts                 = Collections.unmodifiableList(this.sorts);
		this.constants             = Collections.unmodifiableList(this.constants);
		this.randoms               = Collections.unmodifiableList(this.randoms);
		this.conditionedPotentials = Collections.unmodifiableList(this.conditionedPotentials);
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
	
	//
	// PRIVATE
	//
	private static boolean isLegalModelTuple(Expression modelTupleExpr) {
		boolean result = modelTupleExpr != null && Tuple.isTuple(modelTupleExpr) && modelTupleExpr.numberOfArguments() == 4;
		return result;
	}
	private static List<HOGMSortDeclaration> extractSorts(Expression modelTupleExpr) {
		List<HOGMSortDeclaration> result = new ArrayList<>();
		
		if (isLegalModelTuple(modelTupleExpr)) {
			Expression sortsTuple = Tuple.get(modelTupleExpr, 0);
			Tuple.getElements(sortsTuple).forEach(sortExpr -> result.add(HOGMSortDeclaration.makeSortDeclaration(sortExpr)));
		}
		return result;
	}
	
	private static List<ConstantDeclaration> extractConstants(Expression modelTupleExpr) {
		List<ConstantDeclaration> result = new ArrayList<>();
		
		if (isLegalModelTuple(modelTupleExpr)) {
			Expression constantsTuple = Tuple.get(modelTupleExpr, 1);
			Tuple.getElements(constantsTuple).forEach(constantExpr -> result.add(ConstantDeclaration.makeConstantDeclaration(constantExpr)));
		}
		
		return result;
	}
	
	private static List<HOGMRandomVariableDeclaration> extractRandom(Expression modelTupleExpr) {
		List<HOGMRandomVariableDeclaration> result = new ArrayList<>();
		
		if (isLegalModelTuple(modelTupleExpr)) {
			Expression randomsTuple = Tuple.get(modelTupleExpr, 2);
			Tuple.getElements(randomsTuple).forEach(randomExpr -> result.add(HOGMRandomVariableDeclaration.makeRandomVariableDeclaration(randomExpr)));
		}
		
		return result;
	}
	
	private static List<Expression> extractConditionedPotentials(Expression modelTupleExpr) {
		List<Expression> result = new ArrayList<>();
		if (isLegalModelTuple(modelTupleExpr)) {
			Expression conditionedPotentialsTuple = Tuple.get(modelTupleExpr, 3);
			Tuple.getElements(conditionedPotentialsTuple).forEach(conditionedPotential -> result.add(conditionedPotential));
		}
		return result;
	}
}
