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
package com.sri.ai.praise.lang.grounded.transform;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.lang.grounded.bayes.ConditionalProbabilityTable;
import com.sri.ai.praise.lang.grounded.common.FunctionTable;
import com.sri.ai.praise.lang.grounded.markov.FactorTable;
import com.sri.ai.praise.lang.grounded.markov.MarkovNetwork;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.CartesianProductEnumeration;

/**
 * Utility for transforming a Markov Network into an equivalent Bayes Network.
 * 
 * @author oreilly
 *
 */
@Beta
public class XFormMarkovToBayes {
	public static final int MAX_NUM_ALLOWED_PARENTS_IN_CPT = 15;
	
	public static interface BayesOutputListener {
		void newCPT(ConditionalProbabilityTable cpt);
	}
	
	public static void transform(MarkovNetwork markov, BayesOutputListener bayesListener) {
		// Determine the random variable to factor associations
		Map<Integer, List<FactorTable>> randomVariableToFactors = new LinkedHashMap<>();
		List<FactorTable> factors = new ArrayList<>();
		for (int i = 0; i < markov.numberFactors(); i++) {
			FactorTable factor = markov.getFactor(i);
			factors.add(factor);
			for (Integer rvIdx : factor.getVariableIndexes()) {
				List<FactorTable> rvFactors = Util.getValuePossiblyCreatingIt(randomVariableToFactors, rvIdx, ArrayList.class);
				rvFactors.add(factor);
			}
		}
		
		while (!randomVariableToFactors.isEmpty()) {
			// STEP 1:
			// Pick a variable such that number of random variable neighbors
			// (that is, other vars sharing factors) is minimal.
			Integer c = pickMinimal(factors);
		
			// STEP 2:
			// Multiply all factors containing this var - remove them from
			// the factor graph - to give an unnormalized CPT phi for the var
			List<FactorTable> factorsContainingC = randomVariableToFactors.get(c);
			Set<Integer> p = new LinkedHashSet<>();
			for (FactorTable f : factorsContainingC) {
				p.addAll(f.getVariableIndexes());
			}
		
			FactorTable cFactor = newMegaFactor(markov, p, factorsContainingC);
			p.remove(c); // Now ensure c is not included in p
			
			randomVariableToFactors.remove(c);
			factors.removeAll(factorsContainingC);			
			for (Integer pv : p) {
				randomVariableToFactors.get(pv).removeAll(factorsContainingC);
			}
			
			// STEP 3:
			// In the unnormalized CPT phi, let C be the child 
			// (latest in the ordering) and P the other variables in it.
			// For each assignment to parents P, compute a new factor on P,
			// Z(P) = sum_{C} phi(C,P).	
			// Replace each entry phi(C,P) by phi(C,P)/Z(P).
			// Store the now normalized CPT in the Bayesian network, and 
			// add factor Z to the factor network.			
			Pair<FactorTable, ConditionalProbabilityTable> zFactorAndcCPT = sumOutChildAndCreateCPT(markov, c, cFactor);
			
			FactorTable                 zFactor = zFactorAndcCPT.first;
			ConditionalProbabilityTable cCPT    = zFactorAndcCPT.second;
			
			bayesListener.newCPT(cCPT);
			
			if (zFactor != null) {
				factors.add(zFactor);
				for (Integer pv : p) {
					randomVariableToFactors.get(pv).add(zFactor);
				}
			}
			
		} // Repeat for the remaining of the factor graph
	}
	
	//
	// PRIVATE
	//
	private static Integer pickMinimal(List<FactorTable> factors) {		
		if (factors.size() == 0) {
			throw new IllegalArgumentException("Need > 0 factors to pick a minimum");
		}
		
		Map<Integer, Set<Integer>> randomVariableNeighbors = new LinkedHashMap<>();
		
		for (FactorTable factor : factors) {
			for (Integer rv : factor.getVariableIndexes()) {
				Set<Integer> neighbors = Util.getValuePossiblyCreatingIt(randomVariableNeighbors, rv, LinkedHashSet.class);
				neighbors.addAll(factor.getVariableIndexes()); // NOTE: include self for efficiency as all counts will end up being + 1
			}
		}
		
		Map.Entry<Integer, Set<Integer>> smallestEntry = randomVariableNeighbors.entrySet().stream()
					.min((e1, e2) -> Integer.compare(e1.getValue().size(), e2.getValue().size())).get();

		if (smallestEntry.getValue().size() > MAX_NUM_ALLOWED_PARENTS_IN_CPT) {			
			throw new IllegalStateException("Too large a CPT will need be generated as #parents="+(smallestEntry.getValue().size()-1));
		}	
		
		Integer result = smallestEntry.getKey();		
		
		return result;
	}
	
	private static FactorTable newMegaFactor(MarkovNetwork markov, Set<Integer> varIdxs,  List<FactorTable> factorsContainingC) {
		List<Integer> variableIndexes = new ArrayList<>(varIdxs);
		
		Map<Integer, Integer> varIdxToOffset   = new LinkedHashMap<>();
		List<Integer>         varCardinalities = new ArrayList<>();
		for (Integer varIdx : variableIndexes) {
			varIdxToOffset.put(varIdx, varIdxToOffset.size());
			varCardinalities.add(markov.cardinality(varIdx));
		}
		List<Double>  entries   = new ArrayList<>(FunctionTable.numEntriesFor(varCardinalities));
		List<Integer> varValues = new ArrayList<>();
		CartesianProductEnumeration<Integer> cpe = new CartesianProductEnumeration<>(FunctionTable.cardinalityValues(varCardinalities));
		while (cpe.hasMoreElements()) {
			List<Integer> assignments = cpe.nextElement();
			
			double product = 1;
			for (FactorTable ft : factorsContainingC) {
				varValues.clear();
				for (Integer varIdx : ft.getVariableIndexes()) {
					varValues.add(assignments.get(varIdxToOffset.get(varIdx)));
				}
				product *= ft.getTable().entryFor(varValues);
			}
			entries.add(product);
		}
		
		FactorTable result = new FactorTable(variableIndexes, new FunctionTable(varCardinalities, entries));
		
		return result;
	}
	
	private static Pair<FactorTable, ConditionalProbabilityTable> sumOutChildAndCreateCPT(MarkovNetwork markov, Integer c, FactorTable cFactor) {
		List<Integer> parentVarIdxs = new ArrayList<>(cFactor.getVariableIndexes());
		parentVarIdxs.remove(c);
		int cCardinality = markov.cardinality(c);
		int cIdx         = cFactor.getVariableIndexes().indexOf(c);        
		FactorTable   summedOut = null;
		FunctionTable cptTable  = null;
		if (parentVarIdxs.size() == 0) {
			// No summed out factor as no remaining elements but still need to ensure the cpt values are normalized			
			int idx = 0;
			List<Double> factorEntries = cFactor.getTable().getEntries();
			List<Double> cptEntries    = new ArrayList<>(factorEntries.size());
			while (idx < factorEntries.size()) {
				double total = 0;
				for (int i = 0; i < cCardinality; i++) {
					total += factorEntries.get(idx+i);
				}
				for (int i = 0; i < cCardinality; i++) {
					cptEntries.add(factorEntries.get(idx+i)/total);
				}
				idx += cCardinality;
			}
			cptTable = new FunctionTable(Arrays.asList(cCardinality), cptEntries);
		}
		else {
			List<Integer> cptCardinalities       = new ArrayList<>();
			List<Integer> parentVarCardinalities = new ArrayList<>();
			Map<Integer, Integer> parentCardIdxToCFactorIdx = new LinkedHashMap<>();
			int cFactorCardIdx = 0;
			for (Integer varIdx : cFactor.getVariableIndexes()) {
				if (!varIdx.equals(c)) {				
					int card = markov.cardinality(varIdx);
					cptCardinalities.add(card);
					parentVarCardinalities.add(card);
					parentCardIdxToCFactorIdx.put(parentCardIdxToCFactorIdx.size(), cFactorCardIdx);
				}
				
				cFactorCardIdx++;
			}
			// The child index will be placed at the end of the table by convention
			cptCardinalities.add(markov.cardinality(c));
			
			List<Double> cptEntries             = new ArrayList<>(FunctionTable.numEntriesFor(cptCardinalities));
			List<Double> parentSummedOutEntries = new ArrayList<>(FunctionTable.numEntriesFor(parentVarCardinalities));	
			Map<Integer, Integer> assignmentMap = new LinkedHashMap<>();
			CartesianProductEnumeration<Integer> cpe = new CartesianProductEnumeration<>(FunctionTable.cardinalityValues(parentVarCardinalities));
			while (cpe.hasMoreElements()) {
				List<Integer> parentValues = cpe.nextElement();
				assignmentMap.clear();
				for (int i = 0; i < parentValues.size(); i++) {
					assignmentMap.put(parentCardIdxToCFactorIdx.get(i), parentValues.get(i));
				}
				Double sum = cFactor.getTable().valueFor(assignmentMap);
				parentSummedOutEntries.add(sum);
				for (int i = 0; i < cCardinality; i++) {
					assignmentMap.put(cIdx, i);
					if (sum.equals(0.0)) {
// TODO - is this approach correct?						
						// NOTE: This prevents invalid models being generated by assigning an impossibly small probability to an event that should never occur
						cptEntries.add(Double.MIN_NORMAL); 
					}
					else {
						cptEntries.add(cFactor.getTable().valueFor(assignmentMap) / sum);
					}
				}				
			}
			summedOut = new FactorTable(parentVarIdxs, new FunctionTable(parentVarCardinalities, parentSummedOutEntries));
			
			cptTable  = new FunctionTable(cptCardinalities, cptEntries);
		}
		ConditionalProbabilityTable cpt = new ConditionalProbabilityTable(parentVarIdxs, c, cptTable);
		Pair<FactorTable, ConditionalProbabilityTable> result = new Pair<>(summedOut, cpt);		
		return result;
	}
}