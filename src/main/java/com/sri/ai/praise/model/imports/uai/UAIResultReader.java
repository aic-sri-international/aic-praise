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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;

import static com.sri.ai.praise.model.imports.uai.UAIUtil.readLine;
import static com.sri.ai.praise.model.imports.uai.UAIUtil.split;

/**
 * Utility file for reading in Uncertainty in Artificial Intelligence (UAI) 
 * <a href="http://www.hlt.utdallas.edu/~vgogate/uai14-competition/resformat.html">Inference Competition Result Format files</a>.
 *
 * GENERAL FORMAT:<br>
 * The first line must contain only the task solved: PR|MAP|MAR|MMAP. The rest of the file will contain the solution for the task. 
 * Solvers can write more then one solution by writing -BEGIN- at the head of the new solution. We will only consider the last solution in the file. 
 * 
 * @author oreilly
 */
@Beta
public class UAIResultReader {
	
	public static final int UAI_PRECISON = 6; // Appears to be the precision used in the solution files provided.
	
	public static void main(String[] args) throws IOException {
		Map<Integer, List<Double>> solution = readMAR(new File(args[0]));
		Map<Integer, List<Double>> computed = readMAR(new File(args[1]));
		
		List<Integer> doNotMatch = compareMAR(solution, computed);
		if (doNotMatch.size() == 0) {
			System.out.println("MATCH");
		}
		else {
			System.out.println("DO NOT MATCH: "+doNotMatch);
		}
	}
	
	/**
	 * Compare two MAR results.
	 * 
	 * @param solution
	 * @param computed
	 * @return a list of the variable indexes whose results do not match.
	 */
	public static List<Integer> compareMAR(Map<Integer, List<Double>> solution, Map<Integer, List<Double>> computed) {
		List<Integer> result = new ArrayList<>();
		
		if (solution.size() != computed.size()) {
			throw new IllegalArgumentException("Solution size of "+solution.size()+" != Computed size of "+computed.size());
		}
		
		MathContext mc = new MathContext(UAI_PRECISON, RoundingMode.DOWN);
		for (int varIdx = 0; varIdx < solution.size(); varIdx++) {
			List<Double> solutionValues = solution.get(varIdx);
			List<Double> computedValues = computed.get(varIdx);
			
			if (solutionValues.size() != computedValues.size()) {
				throw new IllegalArgumentException("Solution values size of "+solutionValues.size()+" does not match the computed values size of "+computedValues.size()+" for var "+varIdx);
			}
			
			for (int valueIdx = 0; valueIdx < solutionValues.size(); valueIdx++) {
				BigDecimal sol = new BigDecimal(solutionValues.get(valueIdx), mc);
				BigDecimal com = new BigDecimal(computedValues.get(valueIdx), mc);
				
				double diff = sol.doubleValue() - com.doubleValue();
				
				if (diff != 0.0) {
					result.add(varIdx);
					break;
				}
			}
		}
		
		
		return result;
	}

	/**
	 * Marginals, MAR: A space separated line that includes:
	 * The number of variables in the model.
	 * 
	 *  A list of marginal approximations of all the variables. For each variable its cardinality is first stated, 
	 *  then the probability of each state is stated. The order of the variables is the same as in the model, 
	 *  all data is space separated.
	 *
	 * @param solutionFile
	 * @return a map containing the solution values for each variable.
	 * @throws IOException
	 */
	public static Map<Integer, List<Double>> readMAR(File solutionFile) throws IOException {
		
		Map<Integer, List<Double>> result = new LinkedHashMap<>();
		
		if (!solutionFile.exists()) {
			System.err.println("Solution file does not exist: "+solutionFile.getAbsolutePath());
		}
		else {

			try (BufferedReader br = new BufferedReader(new FileReader(solutionFile))) {
				String taskSolved = readLine(br);
				if (!UAITask.MAR.name().equals(taskSolved)) {
					throw new IllegalArgumentException("This result file does not contain a MAR soluton : contains ["+taskSolved+"] instead of ["+UAITask.MAR.name()+"] in "+solutionFile.getAbsolutePath());
				}
				String line;
				result.clear();
				while ((line = readLine(br)) != null) {
					if (line.equals("-BEGIN-")) {
						result.clear();
						continue;
					}
					String[] solution = split(line);
					int varIdx   = 0; 
					int numberVariablesLeft = Integer.parseInt(solution[0]);
					for (int i = 1; i < solution.length; i++) {
						int card = Integer.parseInt(solution[i]);
						List<Double> values = new ArrayList<>();
						for (int c = 1; c <= card; c++) {
							values.add(Double.parseDouble(solution[i+c]));
						}
						result.put(varIdx, values);
						i += card; // Move i onto the next cardinality declaration
						varIdx++;  // Move onto the end of the values read
						numberVariablesLeft--;
					}
					if (numberVariablesLeft != 0) {
						throw new IllegalArgumentException("Did not read results for the remaining "+numberVariablesLeft+" variables.");
					}
				}
			}
		}
		
		return result;
	}
}