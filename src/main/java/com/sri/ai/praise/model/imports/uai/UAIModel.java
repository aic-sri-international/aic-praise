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
import java.util.Set;

import com.google.common.annotations.Beta;

/**
 * In memory representation of an Uncertainty in Artificial Intelligence (UAI) 
 * <a href="http://www.hlt.utdallas.edu/~vgogate/uai14-competition/modelformat.html">Inference Competition Model</a>.
 * 
 * @author oreilly
 */
@Beta
public class UAIModel {
	enum Type {
		MARKOV, BAYES
	}
	
	private File file;
	private Type type;
	Map<Integer, Integer> varIdxToCardinality        = new LinkedHashMap<>();
	Map<Integer, Integer> evidence                   = new LinkedHashMap<>();
	List<List<Integer>> cliques                      = new ArrayList<>();
	Map<Integer, FunctionTable> cliqueToTable        = new LinkedHashMap<>();
	Map<FunctionTable, List<Integer>> tableToCliques = new LinkedHashMap<>();
	Map<Integer, List<Double>> marSolution           = new LinkedHashMap<>();
	
	public UAIModel(File file, Type type, 
			Map<Integer, Integer> varIdxToCardinality,
			List<List<Integer>> cliques,
			Map<Integer, FunctionTable> cliqueToTable) {
		this.file = file;
		this.type = type;
		this.varIdxToCardinality.putAll(varIdxToCardinality);
		this.cliques.addAll(cliques);
		this.cliqueToTable.putAll(cliqueToTable);
		
		for (Map.Entry<Integer, FunctionTable> entry : this.cliqueToTable.entrySet()) {
			List<Integer> cliquesForTable = this.tableToCliques.get(entry.getValue());
			if (cliquesForTable == null) {
				cliquesForTable = new ArrayList<>();
				this.tableToCliques.put(entry.getValue(), cliquesForTable);
			}
			cliquesForTable.add(entry.getKey());
		}	
	}
	
	public File getFile() {
		return file;
	}
	
	public Type getType() {
		return type;
	}
	
	public int numberVars() {
		return varIdxToCardinality.size();
	}
	
	public int cardinality(int varIdx) {
		return varIdxToCardinality.get(varIdx);
	}
	
	public int numberCliques() {
		return cliques.size();
	}
	
	public List<Integer> getVariableIdxsForClique(int cliqueIdx) {
		return cliques.get(cliqueIdx);
	}
	
	public int numberUniqueFunctionTables() {
		return tableToCliques.size();
	}
	
	public double ratioUniqueFunctionTableToCliques() {
		return ((double) numberUniqueFunctionTables()) / ((double) numberCliques());
	}
	
	public void clearEvidence() {
		this.evidence.clear();
	}
	
	public Set<Map.Entry<Integer, Integer>> getEvidence() {
		return evidence.entrySet();
	}
	
	public Set<Map.Entry<FunctionTable, List<Integer>>> getTableToCliques() {
		return tableToCliques.entrySet();
	}
	
	public void addEvidence(Integer varIdx, Integer valueIdx) {
		if (varIdx < 0 || varIdx >= numberVars()) {
			throw new IllegalArgumentException("Not a legal variable index: "+varIdx+" must be in interval [0, "+numberVars()+")");
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
			throw new IllegalArgumentException("Variable Index is invalid, give "+ varIdx +" must be in interval [0, "+numberVars()+")");
		}
		if (cardinality != values.size()) {
			throw new IllegalArgumentException("Size of values given, "+values.size()+", does not match variables cardinality, which is "+cardinality);
		}
		this.marSolution.put(varIdx, new ArrayList<>(values));
	}
	
	public Map<Integer, List<Double>> getMARSolution() {
		return Collections.unmodifiableMap(this.marSolution);
	}
	
	public int totalNumberEntriesForAllFunctionTables() {
		int result = tableToCliques.keySet().stream().mapToInt(ft -> ft.numberEntries() * tableToCliques.get(ft).size()).reduce((n1, n2) -> n1 + n2).getAsInt();
		return result;
	}
	
	public int largestNumberOfFunctionTableEntries() {		
		int result = tableToCliques.keySet().stream().max((ft1, ft2) -> Integer.compare(ft1.numberEntries(), ft2.numberEntries())).get().numberEntries();
		
		return result;
	}
	
	public int largestCardinality() {
		int result = varIdxToCardinality.values().stream().max((c1, c2) -> Integer.compare(c1, c2)).get().intValue();
		return result;
	}
	
	@Override
	public String toString() {
		return "UAI model file="+getFile().getName()+", #vars="+numberVars()+", #cliques="+numberCliques()+", #unique function tables="+numberUniqueFunctionTables()+", ratio="+ratioUniqueFunctionTableToCliques();
	}
}
