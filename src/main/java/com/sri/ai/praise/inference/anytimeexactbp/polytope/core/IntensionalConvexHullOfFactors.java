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
package com.sri.ai.praise.inference.anytimeexactbp.polytope.core;

import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.subtract;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.Polytope;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;

/**
 * @author braz
 *
 */
public class IntensionalConvexHullOfFactors implements Polytope {
	
	private Collection<? extends Variable> indices;

	private Factor factor;
	
	public IntensionalConvexHullOfFactors(Collection<? extends Variable> indices, Factor factor) {
		Set<Variable> indicesAppearingInFactor = intersection(indices, factor.getVariables());
		this.indices = indicesAppearingInFactor;
		this.factor = factor;
	}

	public Collection<? extends Variable> getIndices() {
		return indices;
	}


	@Override
	public Collection<? extends Variable> getFreeVariables() {
		List<? extends Variable> all = factor.getVariables();
		List<Variable> free = subtract(all, getIndices());
		return free;
	}

	public Factor getFactor() {
		return factor;
	}
	
	@Override
	public boolean isUnit() {
		boolean result = 
				indices.isEmpty()
				&&
				factor.isUnit();
		return result;
	}

	@Override
	public String toString() {
		return "{(on " + join(indices) + ")" + factor + "}";
	}
}