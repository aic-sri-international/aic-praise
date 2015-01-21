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

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultIntensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.set.Sets;

/**
 * Rewriter solving unions of intensional unisets with unifiable heads.
 * 
 * @author braz
 * 
 */
@Beta
public class UnionOfIntensionalUniSetsWithUnifiableHeads extends AbstractLBPHierarchicalRewriter {

	public UnionOfIntensionalUniSetsWithUnifiableHeads() {
	}
	
	@Override
	public String getName() {
		return R_union_of_intensional_sets_with_unifiable_heads;
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression;
		
		if (expression.hasFunctor(FunctorConstants.UNION) &&
				expression.numberOfArguments() == 2 &&
				Sets.isIntensionalUniSet(expression.get(0)) &&
				Sets.isIntensionalUniSet(expression.get(1))) {
			
			Expression set1 = expression.get(0);
			Expression set2 = expression.get(1);
			set2 = StandardizedApartFrom.standardizedApartFrom(set2, set1, process);
			
			Expression head1 = ((IntensionalSet) set1).getHead();
			Expression head2 = ((IntensionalSet) set2).getHead();
			
			Expression condition1 = ((IntensionalSet) set1).getCondition();
			Expression condition2 = ((IntensionalSet) set2).getCondition();
			
			Expression newCondition =
					And.make(
							Equality.make(head1, head2),
							Or.make(condition1, condition2)
							);
			
			ExtensionalIndexExpressionsSet indexExpressions1;
			ExtensionalIndexExpressionsSet indexExpressions2;
			try {
				indexExpressions1 = (ExtensionalIndexExpressionsSet) ((IntensionalSet) set1).getIndexExpressions();
				indexExpressions2 = (ExtensionalIndexExpressionsSet) ((IntensionalSet) set2).getIndexExpressions();
			}
			catch(ClassCastException e) {
				throw new Error("Union of intensional sets defined for extensional index expressions case only");
			}
			List<Expression> newIndexExpressions = indexExpressions1.getList();
			newIndexExpressions.addAll(indexExpressions2.getList());
			
			result = new DefaultIntensionalUniSet(newIndexExpressions, head1, newCondition);
			
			result = process.rewrite(R_basic, result);
		}
		
		return result;
	}
}
