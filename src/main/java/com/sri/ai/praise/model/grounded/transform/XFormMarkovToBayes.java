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
package com.sri.ai.praise.model.grounded.transform;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.praise.model.grounded.bayes.ConditionalProbabilityTable;
import com.sri.ai.praise.model.grounded.markov.FactorTable;
import com.sri.ai.praise.model.grounded.markov.MarkovNetwork;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class XFormMarkovToBayes {
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
			
			p.remove(c); // Now ensure c is not included in p
			FactorTable cFactor = newFactor(p, c, factorsContainingC);
			
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
			Pair<FactorTable, ConditionalProbabilityTable> zFactorAndcCPT = sumOutChildAndCreateCPT(c, cFactor); 
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
		if (smallestEntry.getValue().size() > 30) {
			throw new IllegalStateException("Too large a CPT will need be generated as #parents="+(smallestEntry.getValue().size()-1));
		}
		
		Integer result = smallestEntry.getKey();
		
		return result;
	}
	
	private static FactorTable newFactor(Set<Integer> p, Integer c, List<FactorTable> factorsContainingC) {
		FactorTable result = null;
		
// TODO		
		
		return result;
	}
	
	private static Pair<FactorTable, ConditionalProbabilityTable> sumOutChildAndCreateCPT(Integer c, FactorTable cFactor) {
		Pair<FactorTable, ConditionalProbabilityTable> result = null;
// TODO		
		return result;
	}
}