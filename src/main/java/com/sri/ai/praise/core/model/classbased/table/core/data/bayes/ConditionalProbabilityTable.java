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
package com.sri.ai.praise.core.model.classbased.table.core.data.bayes;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.core.model.classbased.table.core.data.FunctionTable;
import com.sri.ai.util.collect.CartesianProductEnumeration;

/**
 * Representation of a Bayesian Network Conditional Probability Table - P(C | P1,...,Pn ).
 * 
 * @author oreilly
 *
 */
@Beta
public class ConditionalProbabilityTable {
	
	public final double DEFAULT_ROUNDING_THRESHOLD = 1e-8;
	
	private List<Integer> parentVariableIndices = new ArrayList<>();
	private Integer childVariableIndex;
	private FunctionTable functionTable;
	
	/**
	 * Constructor.
	 * @param parentVariableIndices
	 *        the parent variable indexes (must match up with the first n cardinality positions
	 *        in the passed in table).
	 * @param childVariableIndex
	 *        the child variable index (i.e. P(C | P1,...,Pn))
	 * @param table
	 *        a function table representation of the CPT. Note: The child index should correspond to the
	 *        last cardinality value position on the function table provided.
	 */
	public ConditionalProbabilityTable(List<Integer> parentVariableIndices, int childVariableIndex, FunctionTable table) {
		if (parentVariableIndices.contains(childVariableIndex)) {
			throw new IllegalArgumentException("Child variable index, " + childVariableIndex + ", is also listed as a parent index " + parentVariableIndices);
		}
		
		this.parentVariableIndices.addAll(parentVariableIndices);
		this.childVariableIndex   = childVariableIndex;
		this.functionTable = table;
	}
	
	public int numberParentVariables() {
		return parentVariableIndices.size();
	}
	
	public List<Integer> getParentVariableIndexes() {
		return parentVariableIndices;
	}
	
	public Integer getChildVariableIndex() {
		return childVariableIndex;
	}
	
	/**
	 * NOTE: the parent indexes map first to the cardinality values in the function table. The child index
	 * corresponds to the last cardinality value position on the function table.
	 * 
	 * @return the FunctionTable that provides the underlying representation for this CPT.
	 */
	public FunctionTable getTable() {
		return functionTable;
	}
	
	/**
	 * 
	 * @return true if the underlying function table represents a legal CPT (i.e. child values sum to 1 for each combination of parent values), false otherwise.
	 */
	public boolean isValid() {
		boolean result = true;
	
		if (numberParentVariables() == 0) {
			Double sum = getTable().getEntries().stream().collect(Collectors.summingDouble(e -> e));
			if (Math.abs(sum - 1.0) > DEFAULT_ROUNDING_THRESHOLD) {
				result = false;
			}
		} 
		else {
			Map<Integer, Integer> assignmentMap = new LinkedHashMap<>();
			CartesianProductEnumeration<Integer> cpe = new CartesianProductEnumeration<>(FunctionTable.cardinalityValues(getTable().getVariableCardinalities().subList(0, numberParentVariables())));
			while (cpe.hasMoreElements()) {
				List<Integer> parentAssignments = cpe.nextElement();
				for (int i = 0; i < parentAssignments.size(); i++) {
					assignmentMap.put(i, parentAssignments.get(i));
				}
				Double sum = getTable().valueFor(assignmentMap);
				if (Math.abs(sum - 1.0) > DEFAULT_ROUNDING_THRESHOLD) {
					result = false;
					break;
				}
			}
		}
		
		return result;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj != null && obj instanceof ConditionalProbabilityTable) {
			ConditionalProbabilityTable other = (ConditionalProbabilityTable) obj;
			return this.childVariableIndex.equals(other.childVariableIndex) && this.parentVariableIndices.equals(other.parentVariableIndices) && this.functionTable.equals(other.functionTable);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.childVariableIndex.hashCode() + this.parentVariableIndices.hashCode() + this.functionTable.hashCode();
	}
}
