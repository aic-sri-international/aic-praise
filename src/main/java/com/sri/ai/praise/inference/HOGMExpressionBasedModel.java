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
package com.sri.ai.praise.inference;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParserWrapper;
import com.sri.ai.praise.model.v1.hogm.antlr.ParsedHOGModel;

@Beta
public class HOGMExpressionBasedModel extends DefaultExpressionBasedModel {

	public HOGMExpressionBasedModel(String hogmodelString) {
		this(new HOGMParserWrapper().parseModel(hogmodelString));
	}
	
	public HOGMExpressionBasedModel(ParsedHOGModel parsedModel) {
		factors.addAll(parsedModel.getConditionedPotentials());
		
		parsedModel.getRandomVariableDeclarations().forEach(random -> {
			mapFromRandomVariableNameToTypeName.put(random.getName().toString(), random.toTypeRepresentation());
		});
		
		parsedModel.getConstatDeclarations().forEach(constant -> {
			mapFromNonUniquelyNamedConstantNameToTypeName.put(constant.getName().toString(), constant.toTypeRepresentation());
		});
		
		parsedModel.getSortDeclarations().forEach(sortDeclaration -> {
			sortDeclaration.getAssignedConstants().forEach(constant -> {
				mapFromUniquelyNamedConstantNameToTypeName.put(constant.toString(), sortDeclaration.getName().toString());
			});
		});
		
		parsedModel.getSortDeclarations().forEach(sort -> {
			if (!sort.getSize().equals(HOGMSortDeclaration.UNKNOWN_SIZE)) {
				mapFromCategoricalTypeNameToSizeString.put(sort.getName().toString(), sort.getSize().toString());
			}
		});
		
		Set<String> integerIntervalTypes = new LinkedHashSet<>();
		parsedModel.getRandomVariableDeclarations().forEach(random -> {
			integerIntervalTypes.addAll(random.getReferencedIntegerIntervalTypes());
		});
		parsedModel.getConstatDeclarations().forEach(constant -> {
			integerIntervalTypes.addAll(constant.getReferencedIntegerIntervalTypes());
		});
		integerIntervalTypes.forEach(integerIntervalName -> additionalTypes.add(new IntegerInterval(integerIntervalName)));
		
		Set<String> realIntervalTypes = new LinkedHashSet<>();
		parsedModel.getRandomVariableDeclarations().forEach(random -> {
			realIntervalTypes.addAll(random.getReferencedRealIntervalTypes());
		});
		parsedModel.getConstatDeclarations().forEach(constant -> {
			realIntervalTypes.addAll(constant.getReferencedRealIntervalTypes());
		});
		realIntervalTypes.forEach(realIntervalName -> additionalTypes.add(new RealInterval(realIntervalName)));
	}
	
	public HOGMExpressionBasedModel(
			List<Expression> factors,
			Map<String, String> mapFromRandomVariableNameToTypeName,
			Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromUniquelyNamedConstantNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes) {

		super(
				factors,
				mapFromRandomVariableNameToTypeName,
				mapFromNonUniquelyNamedConstantNameToTypeName,
				mapFromUniquelyNamedConstantNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes
				);
	}	
}
