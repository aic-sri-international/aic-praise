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
package com.sri.ai.praise.sgsolver.model;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.sgsolver.model.HOGMModelError.Type;

@Beta
public class HOGMModel {

	public static Expression validateAndConstruct(List<StatementInfo> sortDecs, List<StatementInfo> randomVarDecs, List<StatementInfo> terms) {
		HOGMModelValidator validator = new HOGMModelValidator(sortDecs, randomVarDecs, terms);
		
		if (!validator.isValid()) {
			throw new HOGMModelException("Invalid model", validator.errors);
		}
		
		Expression result = Tuple.make(
				Tuple.make(validator.sortDeclarations), 
				Tuple.make(validator.randomVariableDeclarations), 
				Tuple.make(validator.conditionedPotentials));
		
		return result;
	}
	
	private static class HOGMModelValidator {
		List<Expression> sortDeclarations           = new ArrayList<>();
		List<Expression> randomVariableDeclarations = new ArrayList<>();
		List<Expression> conditionedPotentials      = new ArrayList<>();
		//
		List<HOGMModelError> errors = new ArrayList<>();
		//
		Set<Expression> sortConstants = new LinkedHashSet<>();
		Map<Expression, SortDeclaration> sorts = new LinkedHashMap<>();
		Map<Expression, RandomVariableDeclaration> randoms = new LinkedHashMap<>();
		
		HOGMModelValidator(List<StatementInfo> sortStatements, List<StatementInfo> randomVariableStatements, List<StatementInfo> termStatements) {
			validateSortStatements(sortStatements);
			validateRandomVariableStatements(randomVariableStatements);

// TODO -  validate
			termStatements.forEach(t -> conditionedPotentials.add(t.statement));	
			
			// Map validated values
			sortDeclarations.addAll(sorts.values().stream().map(sd -> sd.getSortDeclaration()).collect(Collectors.toList()));
			randomVariableDeclarations.addAll(randoms.values().stream().map(rd -> rd.getRandomVariableDeclaration()).collect(Collectors.toList()));
		}
		
		boolean isValid() {
			return errors.size() == 0;
		}
		
		void validateSortStatements(List<StatementInfo> sortStatements) {
			sortStatements.forEach(sortStatement -> {
				if (!SortDeclaration.isSortDeclaration(sortStatement.statement)) {
					newError(Type.SORT_DECLARATION_IS_NOT_LEGAL, "", sortStatement);
				}
				else if (SortDeclaration.isInBuilt(sortStatement.statement)) {
					newError(Type.SORT_NAME_PREDEFINED, "", sortStatement);
				}
				else {
					SortDeclaration sortDeclaration = SortDeclaration.makeSortDeclaration(sortStatement.statement);
					if (sorts.containsKey(sortDeclaration.getName())) {
						newError(Type.SORT_NAME_NOT_UNIQUE, sortDeclaration.getName(), sortStatement);
					}
					else {
						sorts.put(sortDeclaration.getName(), sortDeclaration);
					}
					sortDeclaration.getAssignedConstants().forEach(constant -> {
						if (sortConstants.contains(constant)) {
							newError(Type.CONSTANT_NAME_NOT_UNIQUE, constant, sortStatement);
						}
						sortConstants.add(constant);
					});
				}
			});
		}
		
		void validateRandomVariableStatements(List<StatementInfo> randomVariableStatements) {
			randomVariableStatements.forEach(rvStatement -> {
				if (!RandomVariableDeclaration.isRandomVariableDeclaration(rvStatement.statement)) {
					newError(Type.RANDOM_VARIABLE_IS_NOT_LEGAL, "", rvStatement);
				}
				else {
					RandomVariableDeclaration rvDeclaration = RandomVariableDeclaration.makeRandomVariableDeclaration(rvStatement.statement);
					if (randoms.containsKey(rvDeclaration.getName())) {
						newError(Type.RANDOM_VARIABLE_NAME_NOT_UNIQUE, rvDeclaration.getName(), rvStatement);
					}
					else if (sortConstants.contains(rvDeclaration.getName())) {
						newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_CONSTANT, rvDeclaration.getName(), rvStatement);
					}
					else {
						randoms.put(rvDeclaration.getName(), rvDeclaration);
						rvDeclaration.getParameterSorts().forEach(parameterSortName -> {
							if (!sorts.containsKey(parameterSortName) && !SortDeclaration.isNameOfInBuilt(parameterSortName)) {
								newError(Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED, parameterSortName, rvStatement);
							}
						});
						if (!sorts.containsKey(rvDeclaration.getRangeSort()) && !SortDeclaration.isNameOfInBuilt(rvDeclaration.getRangeSort())) {
							newError(Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED, rvDeclaration.getRangeSort(), rvStatement);
						}
					}
				}
			});
		}
		
		void newError(Type errorType, Object msg, StatementInfo statement) {
			errors.add(new HOGMModelError(errorType, msg.toString(), statement));
		}
	}
}