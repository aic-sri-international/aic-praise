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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.util.math.MixedRadixNumber;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class FunctionTable {
	private List<Integer> varCardinalities = new ArrayList<>();
	private int numEntries;
	private List<Double> entries;
	private MixedRadixNumber entryIndex;
	
	public FunctionTable(List<Integer> varCardinalities) {
		this.varCardinalities.addAll(varCardinalities);		
		this.numEntries = varCardinalities.stream().reduce((card1, card2) -> card1 * card2).get();
		this.entries    = new ArrayList<>(numEntries);
		this.entryIndex = new MixedRadixNumber(BigInteger.ZERO, this.varCardinalities);
	}
	
	public int numberVariables() {
		return varCardinalities.size();
	}
	
	public int cardinality(int varIdx) {
		return varCardinalities.get(varIdx);
	}
	
	public int numberEntries() {
		return numEntries;
	}
	
	public List<Double> getEntries() {
		return entries;
	}
	
	public void addEntry(Double entry) {
		entries.add(entry);
		if (entries.size() > numEntries) {
			throw new IllegalStateException("Trying to add too many entries");
		}
	}
	
	public Double entryFor(List<Integer> values) {
		Double result = null;
	
		int[] radixValues = new int[values.size()];
		for (int i = 0; i < radixValues.length; i++) {
			radixValues[i] = values.get(i);
		}
		result = entries.get(this.entryIndex.getValueFor(radixValues).intValue());
		
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
