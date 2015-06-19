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
package com.sri.ai.praise.model.export;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.praise.lang.grounded.common.FunctionTable;
import com.sri.ai.praise.lang.grounded.markov.FactorTable;
import com.sri.ai.praise.lang.grounded.markov.MarkovNetwork;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.model.GetRandomVariables;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.ModelGrounding;
import com.sri.ai.praise.model.ParfactorsDeclaration;
import com.sri.ai.util.collect.CartesianProductEnumeration;

/**
 * 
 * @author oreilly
 */
@Beta
public class GroundedModelMarkovNetwork implements MarkovNetwork {
	
	private List<FactorTable>        factors   = new ArrayList<>();
	private Map<Expression, Integer> rvToIdx   = new LinkedHashMap<>();
	private Map<Integer, Expression> idxToRv   = new LinkedHashMap<>();
	private Map<Integer, Integer>    idxToCard = new LinkedHashMap<>(); 
	//
	private Map<Integer, FunctionTable> uniqueFunctionTables = new LinkedHashMap<>();   
	private Map<Integer, List<Integer>> uniqueTableIdxs      = new LinkedHashMap<>();

	public GroundedModelMarkovNetwork(ModelGrounding.GroundedModelResult groundedModelResult) {
		initialize(groundedModelResult);
	}
	
	public Expression getRandomVariable(int variableIndex) {
		return idxToRv.get(variableIndex);
	}
	
	//
	// START-MarkovNetwork
	@Override
	public int numberVariables() {
		return idxToRv.size();
	}
	
	@Override
	public int cardinality(int variableIndex) {
		return idxToCard.get(variableIndex);
	}
	
	@Override
	public int numberUniqueFunctionTables() {
		return uniqueFunctionTables.size();
	}
	
	@Override
	public FunctionTable getUniqueFunctionTable(int uniqueFunctionTableIdx) {
		return uniqueFunctionTables.get(uniqueFunctionTableIdx);
	}
	
	@Override
	public int numberTables() {
		return factors.size();
	}
	
	@Override
	public FunctionTable getTable(int tableIdx) {
		return factors.get(tableIdx).getTable();
	}
	
	@Override
	public List<Integer> getVariableIndexesForTable(int tableIdx) {
		return factors.get(tableIdx).getVariableIndexes();
	}
	
	@Override
	public List<Integer> getTableIndexes(int uniqueFunctionTableIdx) {
		return uniqueTableIdxs.get(uniqueFunctionTableIdx);
	}
	
	@Override
	public FactorTable getFactor(int factorIdx) {
		return factors.get(factorIdx);
	}
	
	// END-MarkovNetwork
	//
	
	//
	// PRIVATE
	//
	private void initialize(ModelGrounding.GroundedModelResult groundedModelResult) {
		RewritingProcess process           = groundedModelResult.getRewritingProcess();
		
		Model                 groundedModel = groundedModelResult.getGroundedModel();
		ParfactorsDeclaration parfacdecs    = groundedModel.getParfactorsDeclaration();
		Expression            factorsExpr   = parfacdecs.getParfactors().get(0);
		
		for (Expression factorExpr : ExtensionalSet.getElements(factorsExpr)) {
			BracketedExpression factor = (BracketedExpression) factorExpr;
			Expression factorValue = factor.getInnerExpression();
			List<Integer> rvIdxs  = new ArrayList<>();
			for (Expression rv : collectDistinctGroundedRandomVariables(factorValue, process)) {
				if (!rvToIdx.containsKey(rv)) {
					int idx = rvToIdx.size();
					rvToIdx.put(rv, idx);
					idxToRv.put(idx, rv);
					idxToCard.put(idx, Model.range(rv, process).size());
				}
				rvIdxs.add(rvToIdx.get(rv));
				
			}
			
			factors.add(newFactorTable(rvIdxs, factorValue, process));
		}
		
		Set<FunctionTable> functionTables = new LinkedHashSet<>();
		Map<FunctionTable, Integer> uniqueFunctionTableToIdx = new LinkedHashMap<>();
		for (int i = 0; i < factors.size(); i++) {
			FunctionTable table = factors.get(i).getTable();
			if (functionTables.add(table)) {
				int utIdx = uniqueFunctionTables.size();
				uniqueFunctionTables.put(utIdx, table);
				uniqueFunctionTableToIdx.put(table, utIdx);
				uniqueTableIdxs.put(utIdx, new ArrayList<>());
			}
			uniqueTableIdxs.get(uniqueFunctionTableToIdx.get(table)).add(i);
		}
	}
	
	private FactorTable newFactorTable(List<Integer> rvIdxs, Expression factorValue, RewritingProcess process) {
		
		List<Integer> rvCards = new ArrayList<>();
		List<List<Expression>> assignValues       = new ArrayList<>();
		final Map<Integer, Integer>  rvIdxToAssignedIdx = new LinkedHashMap<>();   
		for (int i = 0; i < rvIdxs.size(); i++) {
			List<Expression> range = Model.range(idxToRv.get(rvIdxs.get(i)), process);
			assignValues.add(range);
			rvCards.add(range.size());
			rvIdxToAssignedIdx.put(rvIdxs.get(i), rvIdxToAssignedIdx.size());
		}
		List<Double> entries = new ArrayList<>(FunctionTable.numEntriesFor(rvCards));
		CartesianProductEnumeration<Expression> cpe = new CartesianProductEnumeration<>(assignValues);
		while (cpe.hasMoreElements()) {
			final List<Expression> assignments = cpe.nextElement();
			Expression valueExpr = factorValue.replaceAllOccurrences(new AbstractReplacementFunctionWithContextuallyUpdatedProcess() {					
				@Override
				public Expression apply(Expression expression, RewritingProcess process) {
					Expression result = expression;
					Integer idx = rvToIdx.get(expression);
					if (idx != null) {
						result = assignments.get(rvIdxToAssignedIdx.get(idx));
					}
					return result;
				}
			}, LBPFactory.newLBPProcess());
			entries.add(LBPFactory.newNormalize().rewrite(valueExpr, LBPFactory.newLBPProcess()).doubleValue());
		}
		
		FactorTable result = new FactorTable(rvIdxs, new FunctionTable(rvCards, entries));
		
		return result;
	}
	
	private static Set<Expression> collectDistinctGroundedRandomVariables(Expression factorValue, RewritingProcess process) {
		Set<Expression> result = new LinkedHashSet<>();
		Iterator<Expression> rvvExpressions = GetRandomVariables.getRandomVariableValueExpressionsIterator(factorValue, process);
		while (rvvExpressions.hasNext()) {
			result.add(rvvExpressions.next());
		}
		return result;
	}
}
