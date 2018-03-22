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

import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.unionOfCollections;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.Polytope;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.inference.representation.core.IdentityFactor;

/**
 * @author braz
 *
 */
public class ProductPolytope implements Polytope {
	
	private Collection<? extends Polytope> nonUnitPolytopes;

	private ProductPolytope(Collection<? extends Polytope> nonUnitPolytopes) {
		super();
		this.nonUnitPolytopes = nonUnitPolytopes;
	}
	
	@Override
	public Collection<? extends Variable> getFreeVariables() {
		
		Collection<Collection<? extends Variable>> listOfFreeVariablesCollections = 
				mapIntoList(nonUnitPolytopes, Polytope::getFreeVariables);
		
		Set<? extends Variable> allFreeVariables = unionOfCollections(listOfFreeVariablesCollections);
		
		return allFreeVariables;
	}

	public Collection<? extends Polytope> getPolytopes() {
		return nonUnitPolytopes;
	}

	@Override
	public String toString() {
		String result = nonUnitPolytopes.isEmpty()? "{(on ) 1}" : join(nonUnitPolytopes, "*");
		return result;
	}

	@Override
	public boolean isUnit() {
		boolean result = 
				nonUnitPolytopes.size() == 0 
				|| 
				(
						nonUnitPolytopes.size() == 1 
						&& 
						getFirst(nonUnitPolytopes).isUnit());
		return result;
	}

	public static Polytope multiply(Collection<? extends Polytope> polytopes) {
		Polytope result;
		List<? extends Polytope> nonUnitPolytopes = collectToList(polytopes, p -> !p.isUnit());
		if (nonUnitPolytopes.isEmpty()) {
			result = new IntensionalConvexHullOfFactors(list(), new IdentityFactor());
		}
		else {
			result = new ProductPolytope(nonUnitPolytopes);
		}
		return result;
	}
}