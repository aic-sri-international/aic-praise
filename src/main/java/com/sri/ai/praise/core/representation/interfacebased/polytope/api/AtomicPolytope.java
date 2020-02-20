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
package com.sri.ai.praise.core.representation.interfacebased.polytope.api;

/**
 * Interface for atomic polytopes, which is to say polytopes which are not represented as products of polytopes.
 * @author braz
 *
 */
public interface AtomicPolytope extends Polytope {
	
	/**
	 * A method defining the product of {@link AtomicPolytope}s in cases this is a simplification.
	 * Otherwise, the method is free to return null.
	 * <p>
	 * Some cases in which this should return non-null are:
	 * <ul>
	 * <li> the multiplication of simplices on the same variable, which results in the same simplex
	 * <li> the multiplication of function convex hulls with indices of the same type.
	 * In this case, the indices can be unified and the internal factors multiplied, with the resulting taking as much space as one of the original polytopes.
	 * </ul>
	 * <p>
	 * Products on non-unifiable indices should be kept decoupled for as long as possible (for example, two function convex hulls of different indices),
	 * since an atomic product will be most costly and prevent factorization.
	 * 
	 * @param anotherAtomicPolytope
	 * @return
	 */
	AtomicPolytope getProductIfItIsASimplificationOrNullOtherwise(AtomicPolytope anotherAtomicPolytope);
}
