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
package com.sri.ai.praise.model.imports.church;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.AtomsOnTheoryWithEquality;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.DPLLGeneralizedAndSymbolic;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.EqualityTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.FunctionalTermTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.Sum;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.SymbolTermTheory;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.util.Util;

/**
 * A simplification for equalities and atoms that leaves atoms at the bottom of expressions,
 * and a selected target atom last at the bottom.
 * 
 * This is useful for converting arbitrary expressions to a format useful for
 * the 2014 PRAiSE "high-level syntax" rules.
 * We expect this format to be phased out eventually.
 * 
 * @author braz
 *
 */
@Beta
public class SimplifyWithRelationsAtBottom {

	/*
	 * This class is a little tricky, so here's the explanation.
	 * We want to perform a DPLL on equality on non-boolean symbols theory splitters only first,
	 * so that equalities on logical variables are on top
	 * (the non-boolean requirement ensures propositional boolean atoms are not selected).
	 * For the inner "unconditional" expressions (that is, without equalities on non-boolean symbols,
	 * which are the only conditions under the above theory, but it may contain atoms and equalities on atoms),
	 * we want to perform a second, inner DPLL on all atoms but the ones with the target predicate
	 * (so that the target predicate is always in the lowest portions of the final expression).
	 * Finally, a third DPLL simplification is performed on the expression containing the target predicate only.
	 * 
	 * This is achieved in the following way.
	 * First, we extend DPLLGeneralizedAndSymbolic for the first DPLL defined above
	 * by fixing the theory to an equality theory on terms (and not the one that takes atoms as well),
	 * and also overriding the normalizeUnconditionalExpression method so that it invokes the
	 * second DPLL on the "unconditional" equality-free expressions.
	 * 
	 * We implement the second DPLL by extending AtomsOnTheoryWithEquality's
	 * with an overridden makeSplitterIfPossible that does not consider equalities, or atoms with the target
	 * predicate as splitters; this way, all other atoms will be placed above it. 
	 * 
	 * The third DPLL is on expressions with the target predicate only and can be
	 * the ordinary DPLL on atoms and equalities. 
	 */
	
	public static Expression simplify(Expression expression, Expression targetPredicate, RewritingProcess process) {
		Predicate<Expression> prologConstantPredicate = process.getIsUniquelyNamedConstantPredicate();
		process.setIsUniquelyNamedConstantPredicate(
				e ->
				prologConstantPredicate.apply(e) &&
				! LPIUtil.isRandomVariableValueExpression(e, process));
		DPLLForEqualitiesOnSymbolsAndConstantExpressionWithAtomsButTarget equalitiesSimplifier = new DPLLForEqualitiesOnSymbolsAndConstantExpressionWithAtomsButTarget(targetPredicate);
		Expression result = equalitiesSimplifier.solve(expression, Util.list() /* no indices -- simplification only */, process);
		return result;
	}

	private static class DPLLForEqualitiesOnSymbolsAndConstantExpressionWithAtomsButTarget extends DPLLGeneralizedAndSymbolic {

		private Expression targetPredicate;
		
		public DPLLForEqualitiesOnSymbolsAndConstantExpressionWithAtomsButTarget(Expression targetPredicate) {
			super(new EqualityTheory(new NonRandomSymbolTermTheory()), new Sum());
			this.targetPredicate = targetPredicate;
		}
		
		@Override
		public Expression normalizeUnconditionalExpression(Expression expression, RewritingProcess process) {
			DPLLForAtomsButTarget secondDPLL = new DPLLForAtomsButTarget(targetPredicate);
			Expression result = secondDPLL.solve(expression, Util.list(), process);
			return result;
		}
	}
	
	private static class NonRandomSymbolTermTheory extends SymbolTermTheory {
		public boolean isTerm(Expression expression, RewritingProcess process) {
			if (LPIUtil.isRandomVariableValueExpression(expression, process)) {
				return false;
			}
			return super.isTerm(expression, process);
		}
	}
	
	private static class DPLLForAtomsButTarget extends DPLLGeneralizedAndSymbolic {
		public DPLLForAtomsButTarget(Expression targetPredicate) {
			super(new AtomsOnlyButForTarget(targetPredicate), new Sum());
		}
		
		@Override
		public Expression normalizeUnconditionalExpression(Expression expression, RewritingProcess process) {
			DPLLGeneralizedAndSymbolic thirdDPLL =
					new DPLLGeneralizedAndSymbolic(
							new AtomsOnTheoryWithEquality(new EqualityTheory(new FunctionalTermTheory())),
							new Sum());
			// thirdDPLL accepts equalities and non-target atoms, but in this context it will only ever
			// receive expressions with target atoms only, without other atoms and without equalities
			// (equalities of target and other atoms will be replaced by equalities between target and boolean constants,
			// which will be then simplified to target literals),
			// so we can simply use the default classes.
			Expression result = thirdDPLL.solve(expression, Util.list(), process);
			return result;
		}
	}
	
	private static class AtomsOnlyButForTarget extends AtomsOnTheoryWithEquality {

		private Expression targetPredicate;
		
		public AtomsOnlyButForTarget(Expression targetPredicate) {
			super(new EqualityTheory(new FunctionalTermTheory())); // equality theory is irrelevant because makeSplitterIfPossible below filters everything but atoms
			this.targetPredicate = targetPredicate;
		}

		public Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process) {
			Expression result;
			if (expression.hasFunctor(targetPredicate) || expression.equals(targetPredicate) || expression.hasFunctor(FunctorConstants.EQUALITY)) {
				result = null;
			}
			else {
				result = super.makeSplitterIfPossible(expression, indices, process);
			}
			return result;
		}
	}
}