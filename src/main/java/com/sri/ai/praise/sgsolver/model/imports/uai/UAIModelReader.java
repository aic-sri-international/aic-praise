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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.sgsolver.model.grounded.common.FunctionTable;

import static com.sri.ai.praise.sgsolver.model.imports.uai.UAIUtil.readLine;
import static com.sri.ai.praise.sgsolver.model.imports.uai.UAIUtil.split;

/**
 * Utility file for reading in Uncertainty in Artificial Intelligence (UAI) 
 * <a href="http://www.hlt.utdallas.edu/~vgogate/uai14-competition/modelformat.html">Inference Competition Model Format files</a>.
 * 
 * @author oreilly
 */
@Beta
public class UAIModelReader {
	
	public static UAIModel read(File modelFile) throws IOException {
		UAIModel result = null;
		
		try (BufferedReader br = new BufferedReader(new FileReader(modelFile))) {
			Preamble     preamble                       = readPreamble(br);		
			Map<Integer, FunctionTable> tableIdxToTable = readFunctionTables(preamble, br);
		
			result = new UAIModel(modelFile, preamble.type, preamble.variableToCardinality,
									preamble.tableVariableIdxs, tableIdxToTable);
		}
		
		return result;
	}
	
	//
	// PRIVATE
	//
	
	private static Preamble readPreamble(BufferedReader br) throws IOException {
		Preamble result = new Preamble();
		
		//
		// The preamble starts with one line denoting the type of network
		String typeOfNetwork = readLine(br);
		if (!UAIModelType.MARKOV.name().equals(typeOfNetwork)) {
			// NOTE: 2014 competitions files only contain markov networks.
			throw new IllegalArgumentException("Type of network ["+typeOfNetwork+"] is not supported");
		}
		result.type = UAIModelType.MARKOV; // Only type currently supported
		
		//
		// The second line contains the number of variables 
		// (Note: can derive from cardinalities, next line, so will just skip this line as the information is redundant).
		readLine(br); 
		
		//
		// The next line specifies the cardinalities of each variable, one at a time, 
		// separated by a whitespace (note that this implies an order on the variables which will be used throughout the file)
		String[] cardinalities = split(readLine(br)); 
		for (int i = 0; i < cardinalities.length; i++) {
			result.variableToCardinality.put(i, Integer.parseInt(cardinalities[i]));
		}
		
		//
		// The fourth line contains only one integer, denoting the number of cliques in the problem.
		int numberTables = Integer.parseInt(readLine(br));
		
		//
		// Then, one clique per line, the scope of each clique is given as follows: 
		// The first integer in each line specifies the number of variables in the clique, 
		// followed by the actual indexes of the variables. The order of this list is not restricted. 
		// Note that the ordering of variables within a factor will follow the order provided here.
		for (int i = 0; i < numberTables; i++) {
			String   tableData = readLine(br);
			String[] tableInfo = split(tableData);
			// Ensure the table is specified correctly
			if (Integer.parseInt(tableInfo[0]) != (tableInfo.length-1)) {
				throw new IllegalArgumentException("Badly defined table: "+tableData);
			}
			List<Integer> tableVarIdxs = new ArrayList<>();
			for (int t = 1; t < tableInfo.length; t++) {
				tableVarIdxs.add(Integer.parseInt(tableInfo[t]));
			}
			result.tableVariableIdxs.add(tableVarIdxs);
		}
		
		return result;
	}
	
	private static Map<Integer, FunctionTable> readFunctionTables(Preamble preamble, BufferedReader br) throws IOException {
		Map<Integer, FunctionTable> tableIdxToTable = new LinkedHashMap<>();
		
		// In this section each factor is specified by giving its full table (i.e, specifying value for each assignment). 
		// The order of the factor is identical to the one in which they were introduced in the preamble, 
		// the first variable have the role of the 'most significant’ digit. 
		for (int t = 0; t < preamble.numTables(); t++) {		
			FunctionTable functionTable = createFunctionTable(preamble.cardinalitiesForTable(t), br);
			tableIdxToTable.put(t, functionTable);
		}
		
		return tableIdxToTable;
	}
	
	private static FunctionTable createFunctionTable(List<Integer> variableCardinalities, BufferedReader br) throws IOException {
		// For each factor table, first the number of entries is given (this should be equal to the product 
		// of the domain sizes of the variables in the scope). Then, one by one, separated by whitespace, 
		// the values for each assignment to the variables in the function's scope are enumerated.
		String[] entryLineEntries = split(readLine(br));
		int numberEntries = Integer.parseInt(entryLineEntries[0]); 
		
		List<Double> entries = new ArrayList<Double>();
		// Handle table entries on the same line as the #entries information.
		if (entryLineEntries.length > 1) {
			for (int i = 1; i < entryLineEntries.length; i++) {
				entries.add(Double.parseDouble(entryLineEntries[i]));
			}
		}

		while (entries.size() < numberEntries) { 
			String[] entryValues = split(readLine(br));
			for (String entry : entryValues) {
				entries.add(Double.parseDouble(entry));
			}
		}
		
		FunctionTable result = new FunctionTable(variableCardinalities, entries);
		
		return result;
	}
	
	static class Preamble {
		UAIModelType          type;
		Map<Integer, Integer> variableToCardinality = new LinkedHashMap<>();
		List<List<Integer>>   tableVariableIdxs     = new ArrayList<>();
		
		int numVariables() {
			return variableToCardinality.size();
		}
		
		int numTables() {
			return tableVariableIdxs.size();
		}
		
		List<Integer> cardinalitiesForTable(int tableIdx) {
			List<Integer> result = new ArrayList<>();
			
			for (Integer varId : tableVariableIdxs.get(tableIdx)) {
				result.add(variableToCardinality.get(varId));
			}
			
			if (result.size() == 0) {
				throw new IllegalArgumentException("Cardintalitis for table should not be = 0");
			}
			
			return result;
		}
		
		@Override
		public String toString() {
			return ""+type+", #vars="+numVariables()+", #table="+tableVariableIdxs.size();
		}
	}
}
