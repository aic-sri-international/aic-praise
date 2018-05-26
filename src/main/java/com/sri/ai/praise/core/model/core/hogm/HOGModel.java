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
package com.sri.ai.praise.core.model.core.hogm;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Tuple;
import com.sri.ai.praise.core.model.api.Model;
import com.sri.ai.praise.core.model.core.hogm.components.HOGMConstantDeclaration;
import com.sri.ai.praise.core.model.core.hogm.components.HOGMRandomVariableDeclaration;
import com.sri.ai.praise.core.model.core.hogm.components.HOGMSortDeclaration;

@Beta
public class HOGModel implements Model {
	private String                              inputModel            = null;
	private List<HOGMSortDeclaration>           sorts                 = new ArrayList<>();
	private List<HOGMConstantDeclaration>       constants             = new ArrayList<>();
	private List<HOGMRandomVariableDeclaration> randoms               = new ArrayList<>();
	private List<Expression>                    conditionedPotentials = new ArrayList<>();
	
	public HOGModel(String inputModel, Expression modelTupleExpr) {
		this(inputModel, 
				extractSorts(modelTupleExpr), 
				extractConstants(modelTupleExpr), 
				extractRandom(modelTupleExpr),
				extractConditionedPotentials(modelTupleExpr));
	}
	
	public HOGModel(String inputModel, List<HOGMSortDeclaration> sorts, List<HOGMConstantDeclaration> constants, List<HOGMRandomVariableDeclaration> randoms, List<Expression> conditionedPotentials) {
		this.inputModel = inputModel;
		// Ensure the in-built sorts are included.
		for (HOGMSortDeclaration inBuiltSort : HOGMSortDeclaration.IN_BUILT_SORTS) {
			if (!sorts.contains(inBuiltSort)) {
				this.sorts.add(inBuiltSort);
			}
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
	
	public List<HOGMConstantDeclaration> getConstatDeclarations() {
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
			Expression sortsTuple = modelTupleExpr.get(0);
			sortsTuple.getArguments().forEach(sortExpr -> result.add(HOGMSortDeclaration.makeSortDeclaration(sortExpr)));
		}
		return result;
	}
	
	private static List<HOGMConstantDeclaration> extractConstants(Expression modelTupleExpr) {
		List<HOGMConstantDeclaration> result = new ArrayList<>();
		
		if (isLegalModelTuple(modelTupleExpr)) {
			Expression constantsTuple = modelTupleExpr.get(1);
			constantsTuple.getArguments().forEach(constantExpr -> result.add(HOGMConstantDeclaration.makeConstantDeclaration(constantExpr)));
		}
		
		return result;
	}
	
	private static List<HOGMRandomVariableDeclaration> extractRandom(Expression modelTupleExpr) {
		List<HOGMRandomVariableDeclaration> result = new ArrayList<>();
		
		if (isLegalModelTuple(modelTupleExpr)) {
			Expression randomsTuple = modelTupleExpr.get(2);
			randomsTuple.getArguments().forEach(randomExpr -> result.add(HOGMRandomVariableDeclaration.makeRandomVariableDeclaration(randomExpr)));
		}
		
		return result;
	}
	
	private static List<Expression> extractConditionedPotentials(Expression modelTupleExpr) {
		List<Expression> result = new ArrayList<>();
		if (isLegalModelTuple(modelTupleExpr)) {
			Expression conditionedPotentialsTuple = modelTupleExpr.get(3);
			conditionedPotentialsTuple.getArguments().forEach(conditionedPotential -> result.add(conditionedPotential));
		}
		return result;
	}
}
