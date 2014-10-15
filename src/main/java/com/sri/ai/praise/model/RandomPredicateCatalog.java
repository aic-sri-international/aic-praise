/*
 * Copyright (c) 2013, SRI International
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
package com.sri.ai.praise.model;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;

/**
 * 
 * @author braz
 *
 */
@Beta
public class RandomPredicateCatalog extends HashMap<Expression, FunctionSignature> {
	private static final long serialVersionUID = 1L;

	public boolean contains(Object randomPredicate) {
		if (randomPredicate instanceof FunctionSignature) {
			boolean result = false;
			FunctionSignature cataloguedPredicateValue = get(((FunctionSignature)randomPredicate).functorOrSymbol);
			if (cataloguedPredicateValue != null) {
				result = cataloguedPredicateValue.equals(randomPredicate);
			}
			return result;
		}
		throw new Error("RandomPredicatesCatalog.contains called on an object that is not a random predicate.");
	}
	
	/**
	 * Determines the set of predicates in the model associated with the process.
	 * This must be a union of intensional sets of factors represented as bracketed expressions.
	 */
	public static RandomPredicateCatalog getFromBracketedModel(RewritingProcess process) {
		RandomPredicateCatalog result = Model.getRandomPredicateCatalog(process);
		return result;
	}

	//
	// PACKAGE PROTECTED - so can be constructed from within Model while not being available for general use
	RandomPredicateCatalog(Collection<FunctionSignature> randomPredicates) {
		for (FunctionSignature rp : randomPredicates) {
			put(rp.functorOrSymbol, rp);
		}
	}
	
	RandomPredicateCatalog(List<Expression> setsOfFactors, RewritingProcess process) {
		for (Expression setOfFactors : setsOfFactors) {

			if (Sets.isIntensionalSet(setOfFactors)) {
				Expression factor = ((IntensionalSet) setOfFactors).getHead();
				Expression factorValue = ((BracketedExpression) factor).getInnerExpression();
				addPredicates(factorValue, process);
			}
			else {
				for (Expression factor : ExtensionalSet.getElements(setOfFactors)) {
					Expression factorValue = ((BracketedExpression) factor).getInnerExpression();
					addPredicates(factorValue, process);
				}
			}
		}
	}

	private void addPredicates(Expression expression, RewritingProcess process) {
		Iterator<FunctionSignature> randomPredicatesIterator =
			GetRandomVariables.determineRandomPredicates(expression, process);
		while (randomPredicatesIterator.hasNext()) {
			FunctionSignature randomPredicate = randomPredicatesIterator.next();
			add(randomPredicate);
		}
	}

	private void add(FunctionSignature randomPredicate) {
		FunctionSignature anotherRandomPredicate = get(randomPredicate.functorOrSymbol);
		if (anotherRandomPredicate != null && anotherRandomPredicate.arity != randomPredicate.arity) {
			throw new Error("Trying to add " + randomPredicate + " to random predicate catalog, but random predicate " + anotherRandomPredicate + " with same functor and different arity is already catalogued.");
		}
		put(randomPredicate.functorOrSymbol, randomPredicate);		
	}
}
