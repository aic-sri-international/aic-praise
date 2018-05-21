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
package com.sri.ai.praise.inference.generic.anytime.polytope.core;

import static com.sri.ai.util.Util.list;

import com.sri.ai.praise.inference.generic.anytime.polytope.api.AtomicPolytope;
import com.sri.ai.praise.inference.generic.anytime.polytope.api.Polytope;

public abstract class AbstractAtomicPolytope implements AtomicPolytope {

	public AbstractAtomicPolytope() {
		super();
	}

	@Override
	public Polytope multiply(Polytope another) {
		Polytope result;
		if (another.isIdentity()) {
			result = this;
		}
		else if (this.isIdentity()) {
			result = another;
		}
		else if (another instanceof AtomicPolytope) {
			result = multiplyByAnotherAtomicPolytope(another);
		}
		else if (another instanceof ProductPolytope) {
			result = another.multiply(this);
		}
		else {
			throw unrecognizedCase(another);
		}
		return result;
	}

	private Polytope multiplyByAnotherAtomicPolytope(Polytope another) {
		Polytope result;
		AtomicPolytope attempt = this.getProductIfItIsANonIdentityAtomicPolytopeOrNullOtherwise((AtomicPolytope) another);
		if (attempt == null) {
			result = new ProductPolytope(list(this, (AtomicPolytope) another));
		}
		else {
			result = attempt;
		}
		return result;
	}

	private Error unrecognizedCase(Polytope another) {
		return new Error("Multiplying " + this + " by " + another + " but the latter's class is not recognized.");
	}
}