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
package com.sri.ai.praise.core.model.core.treebased.anytimeexactbp.polytope.core;

import static com.sri.ai.util.Util.list;

import java.util.Collection;

import com.sri.ai.praise.core.model.core.treebased.anytimeexactbp.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.model.core.treebased.representation.api.Variable;

/**
 * @author braz
 *
 */
public class Simplex extends AbstractAtomicPolytope {
	
	private Variable variable;
	
	public Simplex(Variable variable) {
		this.variable = variable;
	}
	
	public Variable getVariable() {
		return variable;
	}

	@Override
	public Collection<? extends Variable> getFreeVariables() {
		return list(variable);
	}
	
	@Override
	public boolean isIdentity() {
		return false;
	}

	@Override
	public String toString() {
		return "Simplex(" + variable + ")";
	}

	@Override
	public AtomicPolytope getProductIfItIsANonIdentityAtomicPolytopeOrNullOtherwise(AtomicPolytope another) {
		AtomicPolytope result;
		if (another instanceof Simplex) {
			Simplex anotherSimplex = (Simplex) another;
			if (getVariable().equals(anotherSimplex.getVariable())) {
				result = this;
			}
			else {
				result = null;
			}
		}
		else { 
			result = null;
		}
		return result;
	}
	
	@Override
	public boolean equals(Object another) {
		boolean result =
				another instanceof Simplex
				&&
				((Simplex) another).getVariable().equals(getVariable());
		return result;
	}
	
	@Override
	public int hashCode() {
		return getVariable().hashCode();
	}
}