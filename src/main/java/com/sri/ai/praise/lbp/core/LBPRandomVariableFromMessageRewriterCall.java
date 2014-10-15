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
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.lbp.RandomVariableFromMessageRewriterCall;

/**
 * An implementation of the {@link RandomVariableFromMessageRewriterCall}
 * interface specific to extracting the random variable argument from calls to
 * message rewriters involved in the Lifted Belief Propagation (LBP) algorithm.
 * 
 * @author oreilly
 * 
 */
@Beta
public class LBPRandomVariableFromMessageRewriterCall implements
		RandomVariableFromMessageRewriterCall {

	@Override
	public Expression getRandomVariableFor(String rewriterName, Expression expression) {
		Expression result = null;
		
		if (rewriterName.equals(LBPRewriter.R_belief)) {
			// R_belief(belief(V))
			result = expression.get(0);
		}
		else if (rewriterName.equals(LBPRewriter.R_m_to_f_from_v )) {
			// R_m_to_f_from_v(m_F<-V, beingComputed)
			result = Tuple.get(expression, 0).get(1);
		}
		else if (rewriterName.equals(LBPRewriter.R_m_to_v_from_f)){
			// R_m_to_v_from_f(m_V<-F, C, I, beingComputed)
			result = Tuple.get(expression, 0).get(0);
		}
		else if (rewriterName.equals(LBPRewriter.R_prod_factor)) {
			// R_prod_factor(prod_F in S m_V<-F, beingComputed)
			result = ((IntensionalSet) Tuple.get(expression, 0).get(0)).getHead().get(0);
		} 
		else if (rewriterName.equals(LBPRewriter.R_prod_m_and_prod_factor)) {
			// R_prod_m_and_prod_factor(m * prod_F in S m_V<-F, beingComputed)
			result = ((IntensionalSet) Tuple.get(expression, 0).get(1).get(0)).getHead().get(0);
		}
		else if (rewriterName.equals(LBPRewriter.R_sum)) {
			// R_sum(sum_N E * prod_{V in N'} m_F<-V, T, beingComputed)
			result = Tuple.get(expression, 3);
		}
		
		return result;
	}
}
