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
package com.sri.ai.praise.lbp.core;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.set.extensional.NormalizeExtensionalUniSet;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_neigh_f}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class NeighborsFactor extends AbstractLBPHierarchicalRewriter implements LBPRewriter {

	//
	private NormalizeExtensionalUniSet rNormalizeExtensionalSet = new NormalizeExtensionalUniSet();
	
	public NeighborsFactor() {
		updateChildRewriter(null, rNormalizeExtensionalSet);
	}
	
	@Override
	public String getName() {
		return R_neigh_f;
	}
	
	/**
	 * @see LBPRewriter#R_neigh_f
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		// an expression in the form: Neigh([ Ef ])
		if (!expression.hasFunctor(LPIUtil.FUNCTOR_NEIGHBOR) || expression.numberOfArguments() != 1) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
			
		Expression factor = expression.get(0);
		
		Expression result = null;

		Trace.log("return R_basic(R_normalize_extensional_uniset({ v1, ..., vn }))");
		Trace.log("         where v1, ..., vn are the subexpressions of Ef that are random variable value expressions.");

		if (Justification.isEnabled()) {
			Justification.log(expression);
		}
		
		Expression factorValue = LPIUtil.getFactorValueExpression(factor, process);

		Expression uniset = LPIUtil.getRandomVariablesUsedIn(factorValue, process);

		Justification.beginEqualityStep("definition of neighbors of a factor");
		Justification.endEqualityStep(uniset);

		Justification.beginEqualityStep("checking for duplicate neighbors");
		Expression normalizedUniSet = rNormalizeExtensionalSet.rewrite(uniset, process);
		result = process.rewrite(R_basic, normalizedUniSet);
		Justification.endEqualityStep(result);
		
		return result;
	}
}
