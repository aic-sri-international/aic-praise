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
package com.sri.ai.praise.inference.anytime;

import static com.sri.ai.praise.inference.anytime.setbound.UnionOfSetBounds.union;
import static com.sri.ai.praise.inference.anytime.setbound.UnionOfSetBounds.unionOfAllButTheOneAt;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.Iterator;
import java.util.List;

import com.sri.ai.grinder.library.bounds.Bound;
import com.sri.ai.praise.inference.anytime.setbound.RedirectingSetBound;
import com.sri.ai.praise.inference.anytime.setbound.SetBound;

public abstract class AbstractExactBP
	extends AbstractFunctionOnIterators<Bound>
	implements BoundedMessageIterator {
	
	protected abstract List<Factor> factorsAtRoot();

	protected abstract RedirectingSetBound<Factor> makeInitialFactorsLowerBound();
	protected abstract RedirectingSetBound<Factor> makeInitialFactorsUpperBound();
	
	protected Bound value;

	protected List<? extends AbstractExactBP> subs;

	protected SetBound<Factor> factorsUpperBound;
	final protected RedirectingSetBound<Factor> factorsLowerBound;
	
	public AbstractExactBP(SetBound<Factor> factorsUpperBound) {
		this.value = null; // replace by simplex
		this.factorsUpperBound = factorsUpperBound;
		this.factorsLowerBound = makeInitialFactorsLowerBound();
	}
	
	public AbstractExactBP() {
		this(null); // factors upper bound will be set by parent after all siblings are created
	}
	
	protected void registerSubs(List<? extends AbstractExactBP> subs) {
		this.subs = subs;
		List<SetBound<Factor>> subsLowerBounds = mapIntoList(subs, s -> s.factorsLowerBound);
		setFactorsLowerBound(subsLowerBounds);
		setSubsFactorsUpperBounds(subsLowerBounds);
	}

	private void setFactorsLowerBound(List<SetBound<Factor>> subsLowerBounds) {
		factorsLowerBound.redirectTo(union(subsLowerBounds).union(factorsAtRoot()));
	}

	private void setSubsFactorsUpperBounds(List<SetBound<Factor>> subsLowerBounds) {
		int subIndex = 0;
		for (AbstractExactBP sub : subs) {
			sub.factorsUpperBound = factorsUpperBoundForSubAt(subIndex, subsLowerBounds);
			subIndex++;
		}
	}

	private SetBound<Factor> factorsUpperBoundForSubAt(int subIndex, List<SetBound<Factor>> subsLowerBounds) {
		SetBound<Factor> unionOfSiblingsLowerBounds = unionOfAllButTheOneAt(subsLowerBounds, subIndex);
		SetBound<Factor> upperBoundMinusSiblingsLowerBounds = factorsUpperBound.minus(unionOfSiblingsLowerBounds);
		SetBound<Factor> result = upperBoundMinusSiblingsLowerBounds.minus(factorsAtRoot());
		return result;
	}

	@Override
	public Iterator<Bound> pickNextArgumentIterator() {
		AbstractExactBP result = getFirstSatisfyingPredicateOrNull(subs, Iterator::hasNext);
		return result;
	}

	public Bound getValue() {
		return value;
	}
}