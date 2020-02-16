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
package com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core;

import java.util.Collection;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.core.PruningPredicate;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.util.base.TernaryProcedure;

/**
 * A {@link AbstractExpressionFactor} based on a given {@link Expression}.
 * 
 * @author braz
 *
 */
public class DefaultExpressionFactor extends AbstractExpressionFactor implements ExpressionFactor {

	private static final long serialVersionUID = 1L;

	protected Expression innerExpression;

	public DefaultExpressionFactor(Expression expression, Context context) {
		super(context);
		this.innerExpression = expression;
	}

	@Override
	protected Expression computeInnerExpression() {
		return innerExpression;
	}
	
	@Override
	/**
	 * Overriding implementation that keeps explanation. 
	 */
	public Expression replace(Function<Expression, Expression> replacementFunction, boolean onlyTheFirstOne, PruningPredicate prunePredicate, boolean ignoreTopExpression, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {
		Expression newInnerExpression = super.replace(replacementFunction, onlyTheFirstOne, prunePredicate, ignoreTopExpression, listener, registry);
		DefaultExpressionFactor result = new DefaultExpressionFactor(newInnerExpression, getContext());
		result.setExplanation(getExplanation());
		return result;	
	}

	protected void setInnerExpression(Expression newInnerExpression) {
		this.innerExpression = this.cachedInnerExpression = newInnerExpression;
	}

	@Override
	public Factor normalize(Collection<? extends Variable> variablesToNormalize) {
		throw new Error("normalize not supported for " + getClass());
	}

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
		throw new Error("argmax not supported for " + getClass());
	}

	@Override
	public Factor min(Collection<? extends Variable> variablesToMinimize) {
		throw new Error("min not supported for " + getClass());
	}

	@Override
	public Factor argmin(Collection<? extends Variable> variablesToMinimize) {
		throw new Error("argmin not supported for " + getClass());
	}

}