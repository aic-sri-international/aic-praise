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
package com.sri.ai.praise.sgsolver.model.imports.uai;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.sgsolver.model.HOGMSortDeclaration;
import com.sri.ai.praise.sgsolver.model.grounded.common.GraphicalNetwork;
import com.sri.ai.praise.sgsolver.solver.FactorsAndTypes;

@Beta
public class UAIFactorsAndTypes implements FactorsAndTypes {
	private Map<String, String> mapFromRandomVariableNameToTypeName           = new LinkedHashMap<>();
	private Map<String, String> mapFromNonUniquelyNamedConstantNameToTypeName = Collections.emptyMap(); // Not used for Graphical Networks
	private Map<String, String> mapFromUniquelyNamedConstantNameToTypeName    = new LinkedHashMap<>();
	private Map<String, String> mapFromTypeNameToSizeString                   = new LinkedHashMap<>();
	private List<Expression>    factors                                       = new ArrayList<>(); 
	
	public UAIFactorsAndTypes(List<Expression> tables, GraphicalNetwork network) {
		factors.addAll(tables);
		for (int varIdx = 0; varIdx < network.numberVariables(); varIdx++) {
			int varCardinality = network.cardinality(varIdx);
			String varTypeName = UAIUtil.instanceTypeNameForVariable(varIdx, varCardinality);
			mapFromRandomVariableNameToTypeName.put(UAIUtil.instanceVariableName(varIdx), varTypeName);
			if (!varTypeName.equals(HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName().toString())) {
				for (int valIdx = 0; valIdx < varCardinality; valIdx++) {
					mapFromUniquelyNamedConstantNameToTypeName.put(UAIUtil.instanceConstantValueForVariable(valIdx, varIdx, varCardinality), varTypeName);
				}
			}
			mapFromTypeNameToSizeString.put(varTypeName, Integer.toString(varCardinality));
		}
	}
	
	//
	// START-FactorsAndTypes
	@Override
	public List<Expression> getFactors() {
		return factors;
	}
	
	@Override
	public Map<String, String> getMapFromRandomVariableNameToTypeName() {
		return mapFromRandomVariableNameToTypeName;
	}
	
	@Override
	public Map<String, String> getMapFromNonUniquelyNamedConstantNameToTypeName() {
		return mapFromNonUniquelyNamedConstantNameToTypeName;
	}
	
	@Override
	public Map<String, String> getMapFromUniquelyNamedConstantNameToTypeName() {
		return mapFromUniquelyNamedConstantNameToTypeName;
	}
	
	@Override
	public Map<String, String> getMapFromTypeNameToSizeString() {
		return mapFromTypeNameToSizeString;
	}	
	// END-FactorsAndTypes
	//
}
