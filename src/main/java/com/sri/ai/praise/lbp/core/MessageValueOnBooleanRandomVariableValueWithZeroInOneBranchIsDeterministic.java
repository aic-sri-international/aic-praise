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
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.LPIUtil;

/**
 * Rewrites, for a specific random variable [ V ] present in the RewritingProcess as a global object under
 * the key {@link LPIUtil#RANDOM_VARIABLE_BEING_NORMALIZED},
 * message values of the form <if V then Alpha else 0> as <if V then 1 else 0> (as well as the flipped case).
 * As usual with using deterministic messages, this assumes the probability distribution involved is consistent.
 * The reason the rewriter requires to know which random variable to operate on is that this operation is only valid
 * for the random variable on which a normalization will be later applied.
 * Actually, even that is not a guarantee of correctness because we are also assuming that this message is
 * part of a product of messages on the same random variable.
 * If it were, say, a summation of them, it would not be correct.
 * 
 * @see LPIUtil#setRandomVariableBeingNormalizedAndReturnPreviousOne(Expression, RewritingProcess)
 * @see LPIUtil#restorePreviousRandomVariableBeingNormalized(Expression, RewritingProcess)

 * @author braz
 * 
 */
@Beta
public class MessageValueOnBooleanRandomVariableValueWithZeroInOneBranchIsDeterministic extends AbstractRewriter {

	
	public MessageValueOnBooleanRandomVariableValueWithZeroInOneBranchIsDeterministic() {
		this.setReifiedTests(new HasKind(FunctorConstants.IF_THEN_ELSE));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		expression = IfThenElse.equivalentWithNonNegatedCondition(expression);
		// this may result is an expression that is no longer an if then else

		if (IfThenElse.isIfThenElse(expression)) {
			Expression condition = IfThenElse.getCondition(expression);

			Expression randomVariableBeingNormalized =
					(Expression) process.getGlobalObject(LPIUtil.RANDOM_VARIABLE_BEING_NORMALIZED);

			if (randomVariableBeingNormalized != null) {
				Expression randomVariableBeingNormalizedValue =
						((BracketedExpression) randomVariableBeingNormalized).getInnerExpression();
				if (condition.equals(randomVariableBeingNormalizedValue)) {
					if (IfThenElse.getThenBranch(expression).equals(Expressions.ZERO)) {
						expression = IfThenElse.make(condition, Expressions.ZERO, Expressions.ONE);
					}
					else if (IfThenElse.getElseBranch(expression).equals(Expressions.ZERO)) {
						expression = IfThenElse.make(condition, Expressions.ONE, Expressions.ZERO);
					}
				}
			}
		}

		return expression;
	}
}
