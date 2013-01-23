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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.lbp.LBPRewriter;

/**
 * Default implementation of {@link LBPRewriter#R_intersection}.
 * 
 * @author oreilly
 *
 */
@Beta
public class Intersection extends AbstractLBPHierarchicalRewriter implements LBPRewriter {
	private static final Expression _emptySet = ExtensionalSet.makeEmptySetExpression();
	
	public Intersection() {
	}
	
	public String getName() {
		return R_intersection;
	}
	
	/**
	 * @see LBPRewriter#R_intersection
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 2) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		
		Expression set1 = Tuple.get(expression, 0);
		Expression set2 = Tuple.get(expression, 1);
		
		if (Sets.isIntensionalMultiSet(set1) && Sets.isIntensionalMultiSet(set2)) {
			Trace.log("Set1 is {{ (on I1) Alpha1 | C1 }} and Set2 is {{ (on I2) Alpha2 | C2 }}");
			Trace.log("    standardize Set1 apart from (I2, Alpha2, C2)");
			Expression i2              = IntensionalSet.getScopingExpression(set2);
			Expression alpha2          = IntensionalSet.getHead(set2);
			Expression c2              = IntensionalSet.getCondition(set2);
			Expression tupleI2Alpha2C2 = Tuple.make(Arrays.asList(i2, alpha2, c2));
			
			Expression saSet1          = StandardizedApartFrom
					.standardizedApartFrom(
							set1, tupleI2Alpha2C2, process);
			
			Trace.log("    C <- R_complete_simplify(Alpha1 = Alpha2 and C1 and C2)");
			Expression alpha1 = IntensionalSet.getHead(saSet1);
			Expression c1     = IntensionalSet.getCondition(saSet1);
			Expression c      = process.rewrite(R_complete_simplify, CardinalityUtil.makeAnd(Equality.make(alpha1, alpha2), CardinalityUtil.makeAnd(c1, c2)));
			if (c.equals(Expressions.FALSE)) {
				Trace.log("    if C is \"false\"") ;
				Trace.log("        return {}");
				result = _emptySet;
			} 
			else {
				Trace.log("    I <- concatenation of I1 and I2");
				Trace.log("    return {{ (on I) Alpha1 | C }}");
				List<Expression> i = new ArrayList<Expression>();
				i.addAll(IntensionalSet.getIndexExpressions(saSet1));
				i.addAll(IntensionalSet.getIndexExpressions(set2));
				
				result = IntensionalSet.makeMultiSetFromIndexExpressionsList(i, alpha1, c);
			}
		} 
		else {
			Trace.log("Else");
			Trace.log("    \"Not currently supported\"");
			throw new IllegalArgumentException("Not currently supported:"+expression);
		}
		
		return result;
	}
}
