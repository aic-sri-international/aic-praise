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
package com.sri.ai.praise.core.representation.interfacebased.factor.api;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.ZeroFactor.ZERO_FACTOR;
import static com.sri.ai.util.Util.accumulate;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.sri.ai.util.ExplanationTree;

/**
 * A factor is defined up to a non-zero multiplicative constant
 */
public interface Factor {

	boolean contains(Variable variable);

	List<? extends Variable> getVariables();
	
	Factor multiply(Factor another);
	
	/** Sums a variable out of factor up to a constant. */
	Factor sumOut(List<? extends Variable> variablesToSumOut);
	
	boolean isIdentity();
	
	boolean isZero();
	
	static Factor multiply(Iterator<? extends Factor> factors) {
		Factor result = accumulate(factors, Factor::multiply, IDENTITY_FACTOR);
		return result;
	}

	static Factor multiply(Collection<? extends Factor> factors) {
		Factor result = multiply(factors.iterator());
		return result;
	}
	
	Double getEntryFor(Map<? extends Variable,? extends Object> variableInstantiations);
	
	Factor normalize();
	
	Factor add(Factor another);
	
	static Factor add(Iterator<? extends Factor> factors) {
		Factor result = accumulate(factors, Factor::add, ZERO_FACTOR);
		return result;
	}

	static Factor add(Collection<? extends Factor> factors) {
		Factor result = add(factors.iterator());
		return result;
	}
	
	/** if f is a factor, returns 1/f */
	Factor invert();
	
	/** returns the factor obtained by maximizing according to the variables provided */
	Factor max(Collection<? extends Variable> variablesToMaximize);
	
	ExplanationTree getExplanation();
	
	void setExplanation(ExplanationTree explanation);
}