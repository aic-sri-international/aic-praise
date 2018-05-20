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
package com.sri.ai.praise.lang.grounded.common;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.google.common.annotations.Beta;
import com.sri.ai.util.collect.CartesianProductEnumeration;
import com.sri.ai.util.math.MixedRadixNumber;

/**
 * Basic representation of a function described by a table of values.
 * 
 * @author oreilly
 *
 */
@Beta
public class FunctionTable {
	private List<Integer> varCardinalities = new ArrayList<>();
	private List<Double> entries;
	private MixedRadixNumber entryIndex;
	
	public FunctionTable(List<Integer> varCardinalities, List<Double> entries) {
		this.varCardinalities.addAll(varCardinalities);	
		this.varCardinalities = Collections.unmodifiableList(this.varCardinalities);
		this.entries = new ArrayList<>(entries);
				
		int numEntriesExpected = numEntriesFor(varCardinalities);
		if (numEntriesExpected != this.entries.size()) {
			throw new IllegalArgumentException("#entries "+this.entries.size()+" does not match the expected # of "+numEntriesExpected);
		}

		this.entryIndex = new MixedRadixNumber(BigInteger.ZERO, this.varCardinalities);
	}
	
	public int numberVariables() {
		return varCardinalities.size();
	}
	
	public int cardinality(int varIdx) {
		return varCardinalities.get(varIdx);
	}
	
	public List<Integer> getVariableCardinalities() {
		return varCardinalities; 
	}
	
	public int numberEntries() {
		return entries.size();
	}
	
	public List<Double> getEntries() {
		return entries;
	}
	
	public Double entryFor(List<Integer> varValues) {
		Double result = null;
	
		int[] radixValues = new int[varValues.size()];
		for (int i = 0; i < radixValues.length; i++) {
			radixValues[i] = varValues.get(i);
		}
		result = entryFor(radixValues);
		return result;
	}

	public Double entryFor(int[] varValues) {
		Double result;
		result = entries.get(this.entryIndex.getValueFor(varValues).intValue());
		return result;
	}
	
	public Double valueFor(Map<Integer, Integer> assignmentMap) {
		double result = 0;
		
		// If have all assignments then get the entry straight off as opposed to summing them
		if (assignmentMap.size() == varCardinalities.size()) {
			result = entryFor(IntStream.range(0, varCardinalities.size()).boxed().map(i -> assignmentMap.get(i)).collect(Collectors.toList()));
		}
		else {
			// More than 1 entry value needs to be summed up
			List<List<Integer>> possibleValues = new ArrayList<>(); 
			for (int i = 0; i < varCardinalities.size(); i++) {
				Integer value = assignmentMap.get(i);
				if (value == null) {
					possibleValues.add(IntStream.range(0, varCardinalities.get(i)).boxed().collect(Collectors.toList()));
				}
				else {
					possibleValues.add(Arrays.asList(value));
				}
			}
			
			CartesianProductEnumeration<Integer> cpe = new CartesianProductEnumeration<>(possibleValues);
			while (cpe.hasMoreElements()) {
				result += entryFor(cpe.nextElement());
			}
		}
		
		return result;
	}
	 
	public static int numEntriesFor(List<Integer> varCardinalities) {
		int result = 0;
		if (varCardinalities.size() > 0) {
			result = varCardinalities.stream().reduce((card1, card2) -> card1 * card2).get();
		}
		return result;
	}
	
	public static List<List<Integer>> cardinalityValues(List<Integer> varCardinalities) {
		List<List<Integer>> result = new ArrayList<>();
		
		for (Integer card : varCardinalities) {
			result.add(IntStream.range(0, card).boxed().collect(Collectors.toList()));
		}
		
		return result;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj != null && obj instanceof FunctionTable) {
			FunctionTable other = (FunctionTable) obj;
			return this.varCardinalities.equals(other.varCardinalities) && this.entries.equals(other.entries);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.varCardinalities.hashCode() + this.entries.hashCode();
	}
}
