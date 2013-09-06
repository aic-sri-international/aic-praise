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
package com.sri.ai.praise;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasFunctor;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

/**
 * Re-arranges nested conditionals so that conditions on random variable values
 * are lower in the expression.
 * This normal form is expected by several probabilistic inference rewriters.
 * 
 * More specifically, it rewrites an expression in the following way:
 * <pre>
 * if RV then if LV then Alpha
 *                  else Beta
 *       else Gamma
 * </pre>
 * where RV is a condition on a random variable value, and LV a regular condition (not involving random variable values)
 * <pre>
 * ---->
 * if LV then if RV then Alpha
 *                  else Gamma
 *       else if RV then Beta
 *                  else Gamma
 * </pre>
 * 
 * Note that Alpha, Beta or Gamma may themselves be conditional on regular conditions, in which case
 * the rewriter will apply again, if used exhaustively.
 * 
 * It also does the analogous transformation for the else branch:
 * <pre>
 * if RV then Gamma
 *       else if LV then Alpha
 *                  else Beta
 * ---->
 * if LV then if RV then Gamma
 *                  else Alpha
 *       else if RV then Gamma
 *                  else Beta
 * </pre>
 * 
 * This rewriter has a function similar to {@link BreakConditionsContainingBothLogicalAndRandomVariables}'s,
 * which separates conditions on random and logical variables that are present in the same condition.
 * For the appropriate normal form to be obtained, both rewriters must be used together.
 * 
 * @author braz
 *
 */
@Beta
public class MoveRandomVariableValueExpressionConditionDown extends AbstractRewriter {
	
	public MoveRandomVariableValueExpressionConditionDown() {
		this.setReifiedTests(new HasFunctor(IfThenElse.FUNCTOR));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (LPIUtil.containsRandomVariableValueExpression(IfThenElse.getCondition(expression), process)) {
			
			Expression randomVariableCondition = IfThenElse.getCondition(expression);
			
			Expression thenBranch = IfThenElse.getThenBranch(expression);
			Expression elseBranch = IfThenElse.getElseBranch(expression);
			
			if (IfThenElse.isIfThenElse(thenBranch) &&
					! LPIUtil.containsRandomVariableValueExpression(
							IfThenElse.getCondition(thenBranch), process)) {
				Expression regularCondition = IfThenElse.getCondition(thenBranch); // condition without random variable values
				Expression alpha = IfThenElse.getThenBranch(thenBranch);
				Expression beta  = IfThenElse.getElseBranch(thenBranch);
				Expression gamma = elseBranch;
				
				Expression ifRegularConditionThenBranch =
					IfThenElse.make(randomVariableCondition, alpha, gamma);
				Expression ifRegularConditionElseBranch =
					IfThenElse.make(randomVariableCondition,  beta, gamma);
				
				Expression result =
					IfThenElse.make(
							regularCondition,
							ifRegularConditionThenBranch,
							ifRegularConditionElseBranch);
				
				return result;
			}
			
			if (IfThenElse.isIfThenElse(elseBranch) &&
					! LPIUtil.containsRandomVariableValueExpression(IfThenElse.getCondition(elseBranch), process)) {
				Expression regularCondition = IfThenElse.getCondition(elseBranch); // condition without random variable values
				Expression alpha = IfThenElse.getThenBranch(elseBranch);
				Expression beta  = IfThenElse.getElseBranch(elseBranch);
				Expression gamma = thenBranch;
				
				Expression ifRegularConditionThenBranch =
					IfThenElse.make(randomVariableCondition, gamma, alpha);
				Expression ifRegularConditionElseBranch =
					IfThenElse.make(randomVariableCondition, gamma,  beta);
				
				Expression result =
					IfThenElse.make(
							regularCondition,
							ifRegularConditionThenBranch,
							ifRegularConditionElseBranch);
				
				return result;
			}
		}
		return expression;
	}
}
