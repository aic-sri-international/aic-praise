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
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.CommutativeAssociative;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.util.Util;

/**
 * Default implementation of {@link LBPRewriter#R_union}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class Union extends AbstractLBPHierarchicalRewriter implements LBPRewriter {

	public Union() {
	}
	
	@Override
	public String getName() {
		return R_union;
	}
	
	/**
	 * @see LBPRewriter#R_union
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		// Assert input arguments
		Expression union = expression;
		
		Expression result = null;

		// Cases for U:
		if (IfThenElse.isIfThenElse(union)) {
			// Externalizes conditionals
			Trace.log("if U is if C then Alpha else Beta");
			Trace.log("    return R_basic(if C then R_union(Alpha) else R_union(Beta))");

			Expression condition = IfThenElse.getCondition(union);
			Expression alpha     = IfThenElse.getThenBranch(union);
			Expression beta      = IfThenElse.getElseBranch(union);

			result = GrinderUtil.branchAndMergeOnACondition(
					condition,
					newCallUnionRewrite(), new Expression[] { alpha },
					newCallUnionRewrite(), new Expression[] { beta },
					R_check_branch_reachable, 
					R_basic, process);
		} 
		else if (Sets.isSet(union)) {
			Trace.log("if U is Set");
			Trace.log("    return U");

			result = union;
		} 
		else if (isUnion(union)) {
			if (0 == union.numberOfArguments()) {
				Trace.log("if U is union()");
				Trace.log("    return empty set");

				result = ExtensionalSet.makeEmptySetExpression();
			} 
			else if (union.numberOfArguments() == 1) {
				Trace.log("if U is union(Arg1)");
				Trace.log("     return R_union(Arg1)");

				result = process.rewrite(R_union, union.get(0));
			} 
			else {
				// union(...) > 1 args
				Expression       first     = union.get(0);
				List<Expression> rest      = Util.getRest(union.getArguments());
				Expression       unionRest = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, rest.toArray());

				if (isUnion(first)) {
					// This should not happen as it should be flattened but
					// we'll handle in order to make code robust.
					Trace.log("if U is Union' union Union''");
					Trace.log("    return R_basic(R_union(Union') union R_union(Union''))");
					
					Expression rFirst     = process.rewrite(R_union, first);
					Expression rUnionRest = process.rewrite(R_union, unionRest);

					result = flattenUnion(rFirst, rUnionRest, process);
				} 
				else if (IfThenElse.isIfThenElse(first)) {
					// Externalize Conditionals
					Trace.log("if U is 'If C then Alpha else Beta' union Union'");
					Trace.log("    return R_basic(if C then R_union(Alpha union Union') else R_union(Beta union Union'))");

					Expression condition = IfThenElse.getCondition(first);
					Expression alpha     = IfThenElse.getThenBranch(first);
					Expression beta      = IfThenElse.getElseBranch(first);

					result = GrinderUtil.branchAndMergeOnACondition(
							condition,
							newCallUnionFalttenUnion(), new Expression[] { alpha, unionRest }, 
							newCallUnionFalttenUnion(), new Expression[] { beta, unionRest },
							R_check_branch_reachable, 
							R_basic, process);

				} 
				else if (Sets.isSet(first)) {
					Trace.log("if U is Set union Union'");
					if (ExtensionalSet.isEmptySet(first)) {
						Trace.log("    if Set is empty set");
						Trace.log("        return R_union(Union')");

						result = process.rewrite(R_union, unionRest);
					} 
					else {
						Trace.log("    else if Set is not empty set");
						Trace.log("        return R_basic(Set union R_union(Union'))");

						Expression rUnionRest = process.rewrite(R_union, unionRest);

						result = flattenUnion(first, rUnionRest, process);
					}
				} 
				else {
					throw new IllegalArgumentException(
							"U is not a conditional flattened union of sets: U = " + union);
				}
			}
		} 
		else {
			throw new IllegalArgumentException(
					"U is not a conditional flattened union of sets: U = " + union);
		}

		return result;
	}

	public static Expression make(List<Expression> arguments) {
		Expression result = CommutativeAssociative.make(FunctorConstants.UNION, arguments, Sets.EMPTY_SET, false);
		return result;
	}

	public static boolean isUnion(Expression expression) {
		if (Expressions.hasFunctor(expression, FunctorConstants.UNION)) {
			return true;
		}

		return false;
	}

	//
	// PRIVATE METHODS
	//
	private Expression flattenUnion(Expression expression1, Expression expression2, RewritingProcess process) {
		// Ensure the union of the expressions is flattened and that
		// empty sets are removed where appropriate.
		List<Expression> combinedUnionArgs = new ArrayList<Expression>();
		if (isUnion(expression1)) {
			for (Expression el : expression1.getArguments()) {
				if (!ExtensionalSet.isEmptySet(el)) {
					combinedUnionArgs.add(el);
				}
			}
		} 
		else {
			if (!ExtensionalSet.isEmptySet(expression1)) {
				combinedUnionArgs.add(expression1);
			}
		}
		if (isUnion(expression2)) {
			for (Expression el : expression2.getArguments()) {
				if (!ExtensionalSet.isEmptySet(el)) {
					combinedUnionArgs.add(el);
				}
			}
		} 
		else {
			if (!ExtensionalSet.isEmptySet(expression2)) {
				combinedUnionArgs.add(expression2);
			}
		}
		Expression flattenedUnion = null;

		if (combinedUnionArgs.size() == 0) {
			flattenedUnion = ExtensionalSet.makeEmptySetExpression();
		} 
		else if (combinedUnionArgs.size() == 1) {
			flattenedUnion = combinedUnionArgs.get(0);
		} 
		else {
			flattenedUnion = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.UNION, combinedUnionArgs);
		}

		Expression result = process.rewrite(R_basic, flattenedUnion);

		return result;
	}
	
	//
	private RewriteOnBranch newCallUnionRewrite() {
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				Expression result = process.rewrite(R_union, expressions[0]);
				return result;
			}
		};
	}

	private RewriteOnBranch newCallUnionFalttenUnion() {
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite( Expression[] expressions, RewritingProcess process) {
				
				Expression result = Union.this.flattenUnion(expressions[0], expressions[1], process);
				
				return result;
			}
		};
	}

	public static List<Expression> getEntriesFromUnionOrSet(Expression union) {
		List<Expression> entries;
		
		if (isUnion(union)) {
			entries = union.getArguments();
		} 
		else if ( ! Sets.isEmptySet(union)) {
			entries = Util.list(union);
		}
		else {
			entries = Util.list();
		}
		
		return entries;
	}
}
