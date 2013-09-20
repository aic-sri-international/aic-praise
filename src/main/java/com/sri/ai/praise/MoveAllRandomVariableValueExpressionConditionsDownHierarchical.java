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
 * Neither the name of the aic-expresso nor the names of its
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
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.core.PruningPredicate;
import com.sri.ai.grinder.helper.RewriterFunction;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;

/**
 * Receives a basic expression (basic operators plus products without if then elses in the head)
 * assumed to have a top conditional (all conditionals are externalized) and logical variable conditions
 * separated from random variable ones.
 * Returns an equivalent expression in which all random variable conditions appear under logical variable ones.
 */
@Beta
public class MoveAllRandomVariableValueExpressionConditionsDownHierarchical extends AbstractHierarchicalRewriter {

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = normalize(expression, false, process);
		return result;
	}

	private Expression normalize(Expression expression, boolean subExpressionsAreNormalized, RewritingProcess process) {

		Expression result = expression;
		
		if (IfThenElse.isIfThenElse(expression)) {
			expression = normalizeSubExpressionsIfNeeded(expression, subExpressionsAreNormalized, process);
			if (FormulaUtil.isFormula(IfThenElse.getCondition(expression), process)) {
				// expression is already normalized
				result = expression;
			}
			else {
				int indexOfBranchWithLogicalVariableCondition = pickBranchWithLogicalVariableCondition(expression, process);
				if (indexOfBranchWithLogicalVariableCondition != -1) {
					// In the comments examples, we assume the picked branch is the then branch
					// (the else one is symmetric and the code is written once for both cases by using an index)
					// expression is of the form "if RV then if LV then LVThen else LVElse else RVOtherBranch"
					Expression rvCondition = IfThenElse.getCondition(expression);
					Expression lvBranch = expression.get(indexOfBranchWithLogicalVariableCondition);
					int indexOfTheOtherBranch = IfThenElse.oppositeBranchIndex(indexOfBranchWithLogicalVariableCondition);
					Expression rvOtherBranch = expression.get(indexOfTheOtherBranch);

					Expression lvCondition = IfThenElse.getCondition(lvBranch);
					Expression lvThen      = IfThenElse.getThenBranch(lvBranch);
					Expression lvElse      = IfThenElse.getElseBranch(lvBranch);

					// expression is
					// "if RV then if LV then LVThen else LVElse else RVOtherBranch"
					// ---->
					// "if LV then if RV then/else LVThen else RVOtherBranch else if RV then/else LVElse else RVOtherBranch"
					// where then/else and else/then indicate that the actual position of the top level branches depend on the indices used for symmetric code.
					Expression newLVThenBranch = IfThenElse.make(rvCondition, indexOfBranchWithLogicalVariableCondition, lvThen, indexOfTheOtherBranch, rvOtherBranch);
					Expression newLVElseBranch = IfThenElse.make(rvCondition, indexOfBranchWithLogicalVariableCondition, lvElse, indexOfTheOtherBranch, rvOtherBranch);
					// since these new branches have an RV on top, they may need normalization, but their sub-expressions are known to be normalized already
					newLVThenBranch = normalize(newLVThenBranch, true /* subExpressionsAreNormalized */, process);
					newLVElseBranch = normalize(newLVElseBranch, true /* subExpressionsAreNormalized */, process);
					result = IfThenElse.make(lvCondition, newLVThenBranch, newLVElseBranch);
				}
			}
		}
		return result;
	}

	private int pickBranchWithLogicalVariableCondition(Expression expression, RewritingProcess process) {
		for (int branchIndex = 1; branchIndex != 3; branchIndex++) {
			Expression branch = expression.get(branchIndex);
			if (IfThenElse.isIfThenElse(branch)) {
				if (FormulaUtil.isFormula(IfThenElse.getCondition(branch), process)) {
					return branchIndex;
				}
			}
		}
		return -1;
	}

	private Expression normalizeSubExpressionsIfNeeded(Expression expression, boolean subExpressionsAreNormalized, RewritingProcess process) {
		return expression.replace(new RewriterFunction(this, process), false /* not just the first one */, PRUNE_NON_CONDITIONALS, true /* ignore top expression, expression */, null, process);
	}

	private static PruningPredicate PRUNE_NON_CONDITIONALS = new PruningPredicate() {
		@Override
		public boolean apply(Expression expression, Function<Expression, Expression> replacementFunction, RewritingProcess process) {
			boolean result = ! IfThenElse.isIfThenElse(expression);
			return result;
		}
	};
}
