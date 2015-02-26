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
package com.sri.ai.praise.model.imports.uai;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.model.grounded.common.FunctionTable;
import com.sri.ai.praise.model.grounded.markov.MarkovNetwork;

/**
 * In memory representation of an Uncertainty in Artificial Intelligence (UAI) 
 * <a href="http://www.hlt.utdallas.edu/~vgogate/uai14-competition/modelformat.html">Inference Competition Model</a>.
 * 
 * @author oreilly
 */
@Beta
public class UAIModel implements MarkovNetwork {	
	private File file;
	private UAIModelType type;
	private Map<Integer, Integer> varIdxToCardinality        = new LinkedHashMap<>();
	private Map<Integer, Integer> evidence                   = new LinkedHashMap<>();
	private List<List<Integer>> factorVariableIndexes        = new ArrayList<>();
	private Map<Integer, FunctionTable> factorToTable        = new LinkedHashMap<>();
	private Map<FunctionTable, List<Integer>> tableToFactors = new LinkedHashMap<>();
	private Map<Integer, FunctionTable> tableIdxToTable      = new LinkedHashMap<>();
	private Map<Integer, List<Double>> marSolution           = new LinkedHashMap<>();
	
	public UAIModel(File file, UAIModelType type, 
			Map<Integer, Integer> varIdxToCardinality,
			List<List<Integer>> factorVariableIndexes,
			Map<Integer, FunctionTable> factorToTable) {
		this.file = file;
		this.type = type;
		this.varIdxToCardinality.putAll(varIdxToCardinality);
		this.factorVariableIndexes.addAll(factorVariableIndexes);
		this.factorToTable.putAll(factorToTable);
		
		for (Map.Entry<Integer, FunctionTable> entry : this.factorToTable.entrySet()) {
			List<Integer> factorIndexesForTable = this.tableToFactors.get(entry.getValue());
			if (factorIndexesForTable == null) {
				factorIndexesForTable = new ArrayList<>();
				this.tableToFactors.put(entry.getValue(), factorIndexesForTable);
				this.tableIdxToTable.put(this.tableIdxToTable.size(), entry.getValue());
			}
			factorIndexesForTable.add(entry.getKey());
		}	
	}
	
	public File getFile() {
		return file;
	}
	
	public UAIModelType getType() {
		return type;
	}
	
	//
	// START-GraphicalNetwork
	@Override
	public int numberVariables() {
		return varIdxToCardinality.size();
	}
	
	@Override
	public int cardinality(int varIdx) {
		return varIdxToCardinality.get(varIdx);
	}
	
	@Override
	public int numberUniqueFunctionTables() {
		return tableToFactors.size();
	}
	
	@Override
	public FunctionTable getUniqueFunctionTable(int uniqueFunctionTableIdx) {
		return tableIdxToTable.get(uniqueFunctionTableIdx);
	}
	// END-GraphicalNetwork
	//
	
	//
	// START-MarkovNetwork
	@Override
	public int numberFactors() {
		return factorVariableIndexes.size();
	}
	
	@Override
	public List<Integer> getVariableIndexesForFactor(int factorIdx) {
		return factorVariableIndexes.get(factorIdx);
	}
	
	@Override
	public List<Integer> getFactorIndexes(int uniqueFunctionTableIdx) {
		return tableToFactors.get(getUniqueFunctionTable(uniqueFunctionTableIdx));
	}
	// END-MarkovNetwork
	//
	
	public void clearEvidence() {
		this.evidence.clear();
	}
	
	public Map<Integer, Integer> getEvidence() {
		return Collections.unmodifiableMap(evidence);
	}
	
	public void addEvidence(Integer varIdx, Integer valueIdx) {
		if (varIdx < 0 || varIdx >= numberVariables()) {
			throw new IllegalArgumentException("Not a legal variable index: "+varIdx+" must be in interval [0, "+numberVariables()+")");
		}
		Integer cardinality = cardinality(varIdx);
		if (valueIdx < 0 || valueIdx >= cardinality) {
			throw new IllegalArgumentException("Not a leval value index: "+valueIdx+" must be in interval [0, "+cardinality+")");
		}
		evidence.put(varIdx, valueIdx);
	}
	
	public void clearMARSolution() {
		this.marSolution.clear();
	}
	
	public void addMARSolution(Integer varIdx, List<Double> values) {
		Integer cardinality = varIdxToCardinality.get(varIdx);
		if (cardinality == null) {
			throw new IllegalArgumentException("Variable Index is invalid, give "+ varIdx +" must be in interval [0, "+numberVariables()+")");
		}
		if (cardinality != values.size()) {
			throw new IllegalArgumentException("Size of values given, "+values.size()+", does not match variables cardinality, which is "+cardinality);
		}
		this.marSolution.put(varIdx, new ArrayList<>(values));
	}
	
	public Map<Integer, List<Double>> getMARSolution() {
		return Collections.unmodifiableMap(this.marSolution);
	}
	
	@Override
	public String toString() {
		return "UAI model file="+getFile().getName()+", #vars="+numberVariables()+", #factors="+numberFactors()+", #unique function tables="+numberUniqueFunctionTables()+", ratio="+ratioUniqueFunctionTablesToFactors();
	}
}
