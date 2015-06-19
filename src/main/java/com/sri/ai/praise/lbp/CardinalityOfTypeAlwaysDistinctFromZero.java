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
package com.sri.ai.praise.lbp;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.model.v0.Model;

/**
 * A rewriter for evaluating all cardinality expressions on types of the form:
 * 
 * <pre>
 * | type(.) | = 0  -> false
 * 0 = | type(.) |  -> false
 * | type(.) | > 0  -> true
 * 0 > | type(.) |  -> false
 * </pre>
 * 
 * @author oreilly
 * 
 */
@Beta
public class CardinalityOfTypeAlwaysDistinctFromZero extends AbstractRewriter {

	public final static String FUNCTOR_TYPE = "type";
	
	public CardinalityOfTypeAlwaysDistinctFromZero() {
		this.setReifiedTests(new HasNumberOfArguments(2));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {

		Expression result = expression;
		if (Model.isCardinalityOfTypesAlwaysGreaterThanZero(process)) {
			if (expression.hasFunctor(FunctorConstants.EQUAL) || expression.hasFunctor(FunctorConstants.GREATER_THAN)) {
				Expression arg1 = expression.get(0);
				Expression arg2 = expression.get(1);
				if (arg1.hasFunctor(FunctorConstants.CARDINALITY)
						&& arg1.numberOfArguments() == 1
						&& arg1.get(0).hasFunctor(FUNCTOR_TYPE)
						&& arg2.equals(Expressions.ZERO)) {
					// | type(.) | = 0  -> false
					if (expression.hasFunctor(FunctorConstants.EQUAL)) {
						result = Expressions.FALSE;
					} 
					else if (expression.hasFunctor(FunctorConstants.GREATER_THAN)) {
						// | type(.) | > 0  -> true
						result = Expressions.TRUE;
					}
				} 
				else if (arg2.hasFunctor(FunctorConstants.CARDINALITY)
						&& arg2.numberOfArguments() == 1
						&& arg2.get(0).hasFunctor(FUNCTOR_TYPE)
						&& arg1.equals(Expressions.ZERO)) {
					// 0 = | type(.) | = 0 
					if (expression.hasFunctor(FunctorConstants.EQUAL)) {
						result = Expressions.FALSE;
					} 
					else if (expression.hasFunctor(FunctorConstants.GREATER_THAN)) {
						// 0 > | type(.) | -> false
						result = Expressions.FALSE;
					}
				}
			}
		}

		return result;
	}
}
