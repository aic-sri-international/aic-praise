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
package com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.base;

import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;

public class Polytopes {
	
	////////////////////////////////////////////////// SIMPLEX
	
	public static boolean isSimplexOn(AtomicPolytope atomicPolytope, Variable variable) {
		boolean result = 
				atomicPolytope instanceof Simplex
				&&
				((Simplex)atomicPolytope).getVariable().equals(variable);
		return result;
	}
	
	//////////////////////////////////////////// GET NON-IDENTITY ATOMIC POLYTOPES
	
	public static List<? extends AtomicPolytope> getAtomicPolytopes(Iterable<? extends Polytope> polytopes) {
		List<AtomicPolytope> result = list();
		for (Polytope polytope : polytopes) {
			if ( ! polytope.isIdentity()) {
				if (polytope instanceof ProductPolytope) {
					collectAtomicPolytopesInProduct((ProductPolytope) polytope, result);
				}
				else {
					result.add((AtomicPolytope) polytope);
				}
			}
		}
		return result;
	}

	private static void collectAtomicPolytopesInProduct(ProductPolytope productPolytope, List<AtomicPolytope> result) {
		Collection<? extends Polytope> immediateSubPolytopes = productPolytope.getPolytopes();
		for (Polytope polytope : immediateSubPolytopes) {
			if (polytope instanceof ProductPolytope) {
				collectAtomicPolytopesInProduct((ProductPolytope) polytope, result);
			}
			else {
				result.add((AtomicPolytope) polytope);
			}
		}
	}

}