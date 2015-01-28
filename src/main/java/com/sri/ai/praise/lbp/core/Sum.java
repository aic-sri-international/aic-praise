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

import static com.sri.ai.util.Util.arrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.core.DefaultLambdaExpression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.util.Util;

/**
 * Default implementation of {@link LBPRewriter#R_sum}.
 * 
 * @author oreilly
 * 
 */
@Beta
public class Sum extends AbstractLBPHierarchicalRewriter implements LBPRewriter {

	private LBPConfiguration      configuration  = null;
	
	public Sum(LBPConfiguration configuration) {
		this.configuration  = configuration;
	}
	
	@Override
	public String getName() {
		return R_sum;
	}

	/**
	 * @see LBPRewriter#R_sum
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		//
		// Assert input arguments
		// a tuple of the form: (N, E, prod_{V in N'} m_F<-V, T, beingComputed)
		if (!Tuple.isTuple(expression) || Tuple.size(expression) != 5) {
			throw new IllegalArgumentException("Invalid input argument expression:"+expression);
		}
		Expression summationIndexN           = Tuple.get(expression, 0);
		Expression E                         = Tuple.get(expression, 1);
		Expression productOfIncomingMessages = Tuple.get(expression, 2);
		Expression T                         = Tuple.get(expression, 3);
		Expression beingComputed             = Tuple.get(expression, 4);
		
		if ( ! IfThenElse.isIfThenElse(summationIndexN) && ! Sets.isExtensionalSet(summationIndexN)) {
			throw new IllegalStateException("Sum.compute's summationIndexN must be a conditional extensional set of random variables, but was " + summationIndexN);
		}

		if ( ! productOfIncomingMessages.equals(Expressions.ONE) &&
				! Sets.isIntensionalSet(productOfIncomingMessages.get(0))) {
			throw new IllegalStateException("Sum.compute's productOfIncomingMessages must be a product on an intensional set or 1, but was " + productOfIncomingMessages);
		}

		/** The current expression being used for justifications and trace. */
		Expression currentExpression = null;
		if (Justification.isEnabled() || Trace.isEnabled()) {
			currentExpression = Expressions.apply(
		 	        FunctorConstants.SUM,
			        new DefaultIntensionalMultiSet(ExtensionalSet.getElements(summationIndexN), Times.make(Arrays.asList(E, productOfIncomingMessages)), Expressions.TRUE));
			Justification.log(currentExpression);
		}
		if (Justification.isEnabled()) {
			Justification.log(currentExpression);
		}
		if (Trace.isEnabled()) {
			Trace.log("Computing message to {} from {}", T, currentExpression);
		}

		Expression result = null;

		// Cases:
		if (externalizeConditionalsOnE(E, process)) {
			// Externalizes conditionals on E
			// if E is 'if C' then E1 else E2'
			// return R_normalize(if C'
			// then R_sum(sum_N E1 prod_{V in N'} m_F<-V, T, beingComputed)  under C'
			// else R_sum(sum_N E2 prod_{V in N'} m_F<-V, T, beingComputed)) under not C'
			result = rewriteExternalizeE(summationIndexN, E, productOfIncomingMessages, currentExpression, T, beingComputed, process);
		} 
		else {
			Trace.log("// E = {}", E);
			Trace.log("RV_in_E <- getRandomVariablesUsedIn(E)");
			Expression rvInE = LPIUtil.getRandomVariablesUsedIn(E, process);
			Trace.log("// RV_in_E = {}", rvInE);
			
			Trace.log("// N = {}", summationIndexN);
			Trace.log("N <- N intersection RV_in_E");
			summationIndexN = process.rewrite(R_intersection, LPIUtil.argForIntersectionRewriteCall(summationIndexN, rvInE));
			Trace.log("// N = {}", summationIndexN);
			
			if (IfThenElse.isIfThenElse(summationIndexN)) {
				
				// Externalizes conditionals on N
				// if N is 'if C' then N1 else N2'
				// return R_normalize(if C'
				// then R_sum(sum_{N1} E prod_{V in N'} m_F<-V, T, beingComputed)  under C'
				// else R_sum(sum_{N2} E prod_{V in N'} m_F<-V, T, beingComputed)) under not C'
				result = rewriteExternalizeN(summationIndexN, E, productOfIncomingMessages, currentExpression, T, beingComputed, process);
			} 
			else if (ExtensionalSet.isEmptySet(summationIndexN)) {
				Trace.log("if N is the empty set");
				Trace.log("    // then we know N' is also the empty set because N includes all elements in N'.");
				Trace.log("    return E // E = {}", E);

				if (Justification.isEnabled()) {
					Justification.beginEqualityStep("summation on no random variables evaluates to its summand; incoming messages, if any, can be discarded");
					Justification.endEqualityStep(E);
				}

				result = E;
			}
			else {
				
				Expression incomingMessagesSet = productOfIncomingMessages.get(0);
				List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) ((IntensionalSet) incomingMessagesSet).getIndexExpressions()).getList();
				Expression indexExpression     = indexExpressionsList.get(0);
				Expression NPrime              = IndexExpressions.getType(indexExpression);
				
				Trace.log("// N' = {}", NPrime);
				Trace.log("// RV_in_E = {}", rvInE);
				Trace.log("N' <- N' intersection RV_in_E");
				NPrime = process.rewrite(R_intersection, LPIUtil.argForIntersectionRewriteCall(NPrime, rvInE));
				Trace.log("// N' = {}", NPrime);

				if (IfThenElse.isIfThenElse(NPrime)) {
					// Externalizes conditionals on N'
					// if N' is 'if C' then N1' else N2''
					// return R_normalize(if C'
					// then R_sum(sum_{N} E prod_{V in N1'} m_F<-V, T, beingComputed)  under C'
					// else R_sum(sum_{N} E prod_{V in N2'} m_F<-V, T, beingComputed)) under not C'
					result = rewriteExternalizeNPrime(summationIndexN, E, productOfIncomingMessages, NPrime, currentExpression, T, beingComputed, process);
				}
				else {			
					// else
					// we have sum_N E * prod_{V in N'} m_F<-V, N, N' and E unconditional
					// ...
					result = rewriteUnconditional(summationIndexN, E, productOfIncomingMessages, NPrime, currentExpression, T, beingComputed, process);
				}
			}
		}

		return result;
	}

	//
	// PRIVATE METHODS
	//
	private static boolean externalizeConditionalsOnE(Expression E, RewritingProcess process) {
		boolean result = LPIUtil.isConditionalNotContainingRandomVariableValueExpressionsInCondition(E, process);
		return result;
	}

	private Expression rewriteExternalizeE(Expression summationIndexN,
			Expression E, 
			Expression productOfIncomingMessages,
			Expression currentExpression, 
			Expression T,
			Expression beingComputed,
			RewritingProcess process) {
		Trace.log("// E = {}", E);
		Trace.log("if E is 'if C' then E1 else E2'");
		Trace.log("    return R_normalize(if C'");
		Trace.log("                   then R_sum(sum_N E1 prod_{V in N'} m_F<-V, T, beingComputed)  under C'");
		Trace.log("                   else R_sum(sum_N E2 prod_{V in N'} m_F<-V, T, beingComputed)) under not C'");

		Expression condition = IfThenElse.getCondition(E);
		Expression e1        = IfThenElse.getThenBranch(E);
		Expression e2        = IfThenElse.getElseBranch(E);
		
		if (Justification.isEnabled()) {
			Justification.beginEqualityStep("externalizing conditional on summand");
			Expression subSummation1 = replaceHead(currentExpression, e1);
			Expression subSummation2 = replaceHead(currentExpression, e2);
			Expression ifThenElse    = IfThenElse.make(condition, subSummation1, subSummation2);
			Justification.endEqualityStep(ifThenElse);
		}
		
		Justification.beginEqualityStep("solving summations at then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newCallSumRewrite(), new Expression[] { summationIndexN, e1, productOfIncomingMessages, T, beingComputed },
				newCallSumRewrite(), new Expression[] { summationIndexN, e2, productOfIncomingMessages, T, beingComputed },
				R_check_branch_reachable, 
				R_normalize, process);
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteExternalizeN(Expression summationIndexN,
			Expression E, 
			Expression productOfIncomingMessages,
			Expression currentExpression, 
			Expression T,
			Expression beingComputed,
			RewritingProcess process) {
		Trace.log("// N = {}", summationIndexN);
		Trace.log("if N is of the form if C' then N1 else N2");
		Trace.log("    return R_normalize(if C'");
		Trace.log("           then R_sum(sum_{N1} E prod_{V in N'} m_F<-V, T, beingComputed)  under C'");
		Trace.log("           else R_sum(sum_{N2} E prod_{V in N'} m_F<-V, T, beingComputed)) under not C'");

		LPIUtil.assertProductOk(productOfIncomingMessages);

		Expression condition = IfThenElse.getCondition(summationIndexN);
		Expression n1        = IfThenElse.getThenBranch(summationIndexN);
		Expression n2        = IfThenElse.getElseBranch(summationIndexN);

		if (Justification.isEnabled()) {
			Justification.beginEqualityStep("externalizing conditional on neighbors");
			List<Expression> ln1 = Util.list(n1);
			List<Expression> ln2 = Util.list(n2);
 			Expression subSummation1 = Expressions.apply(FunctorConstants.SUM, ((IntensionalSet) currentExpression.get(0)).setIndexExpressions(new ExtensionalIndexExpressionsSet(ln1)));
			Expression subSummation2 = Expressions.apply(FunctorConstants.SUM, ((IntensionalSet) currentExpression.get(0)).setIndexExpressions(new ExtensionalIndexExpressionsSet(ln2)));
			Expression ifThenElse = IfThenElse.make(condition, subSummation1, subSummation2);
			Justification.endEqualityStep(ifThenElse);
		}
		
		Justification.beginEqualityStep("solving summations at then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newCallSumRewrite(), new Expression[] { n1, E, productOfIncomingMessages, T, beingComputed },
				newCallSumRewrite(), new Expression[] { n2, E, productOfIncomingMessages, T, beingComputed },
				R_check_branch_reachable,
				R_normalize, process);
		Justification.endEqualityStep(result);

		return result;
	}
	
	private Expression rewriteExternalizeNPrime(Expression summationIndexN,
			Expression E, 
			Expression productOfIncomingMessages,
			Expression NPrime,
			Expression currentExpression, 
			Expression T,
			Expression beingComputed,
			RewritingProcess process) {
		Trace.log("// N' = {}", NPrime);
		Trace.log("if N' is of the form if C' then N1' else N2'");
		Trace.log("    return R_normalize(if C'");
		Trace.log("           then R_sum(sum_{N} E prod_{V in N1'} m_F<-V, T, beingComputed)  under C'");
		Trace.log("           else R_sum(sum_{N} E prod_{V in N2'} m_F<-V, T, beingComputed)) under not C'");

		LPIUtil.assertProductOk(productOfIncomingMessages);

		Expression condition = IfThenElse.getCondition(NPrime);
		Expression n1        = IfThenElse.getThenBranch(NPrime);
		Expression n2        = IfThenElse.getElseBranch(NPrime);

		Expression prodIntensionalSet         = productOfIncomingMessages.get(0);
		List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) ((IntensionalSet) prodIntensionalSet).getIndexExpressions()).getList();
		Expression indexExpression            = indexExpressionsList.get(0);
		Expression index                = IndexExpressions.getIndex(indexExpression);
		Expression msgToF_V             = ((IntensionalSet) prodIntensionalSet).getHead();
		Expression prodScopingCondition = ((IntensionalSet) prodIntensionalSet).getCondition();

		Expression productOfIncomingMessagesN1 = LPIUtil.makeProductOfMessages(index, n1, msgToF_V, prodScopingCondition);
		Expression productOfIncomingMessagesN2 = LPIUtil.makeProductOfMessages(index, n2, msgToF_V, prodScopingCondition);
		
		Justification.beginEqualityStep("solving summations at then and else branches");
		Expression result = GrinderUtil.branchAndMergeOnACondition(
				condition,
				newCallSumRewrite(), new Expression[] { summationIndexN, E, productOfIncomingMessagesN1, T, beingComputed },
				newCallSumRewrite(), new Expression[] { summationIndexN, E, productOfIncomingMessagesN2, T, beingComputed },
				R_check_branch_reachable,
				R_normalize, process);
		Justification.endEqualityStep(result);

		return result;
	}

	private Expression rewriteUnconditional(Expression summationIndexN,
			Expression E, 
			Expression productOfIncomingMessages,
			Expression NPrime,
			Expression currentExpression, 
			Expression T,
			Expression beingComputed,
			RewritingProcess process) {

		Trace.log("We now have sum_N E * prod_{V in N'} m_F<-V,   E, N, and N' unconditional");

		Trace.log("// N  = {}", summationIndexN);
		Trace.log("// E  = {}", E);
		Trace.log("// N' = {}", NPrime);
		Trace.log("// T  = {}", T);

		LPIUtil.assertProductOk(productOfIncomingMessages);

		Expression incomingMessagesSet          = productOfIncomingMessages.get(0);
		IndexExpressionsSet indexExpressions    = ((IntensionalSet) incomingMessagesSet).getIndexExpressions();
		List<Expression> indexExpressionsList   = ((ExtensionalIndexExpressionsSet) indexExpressions).getList();
		Expression indexExpression              = indexExpressionsList.get(0);
		Expression indexOfOfIncomingMessagesSet = IndexExpressions.getIndex(indexExpression);
		Expression msgToF_V                     = ((IntensionalSet) incomingMessagesSet).getHead();
		Expression F                            = msgToF_V.get(0);
		Expression incomingMessagesSetCondition = ((IntensionalSet) incomingMessagesSet).getCondition();

		Expression toBeSummedOut = null;
		if (!ExtensionalSet.isEmptySet(NPrime)) {
			Trace.log("if N' is not the empty set");
			Trace.log("    toBeSummedOut <- N'");
			toBeSummedOut = NPrime;
		}
		else {
			Trace.log("if N' is the empty set");
			Trace.log("    toBeSummedOut <- N");
			toBeSummedOut = summationIndexN;
		}

		Trace.log("pick V' in toBeSummedOut (V' has the form [v'])");
		Expression VPrime      = ExtensionalSet.getElements(toBeSummedOut).get(0);
		Expression vPrimeValue = LPIUtil.getRandomVariableValueExpression(VPrime, process);
		Trace.log("// V' = {}", VPrime);
		Trace.log("// v' = {}", vPrimeValue);
		Trace.log("relevantRangeSoFar = {v in range(v') : R_basic(E[v'/v]) is not zero}");
		List<Expression> relevantRangeSoFar = getRelevantRange(vPrimeValue, E, process);
		Trace.log("// relevantRangeSoFar of {} is {}", vPrimeValue, relevantRangeSoFar);

		if (relevantRangeSoFar.size() == 0) {
			Trace.log("if relevantRangeSoFar is {}");
			Trace.log("    throw exception: model does not validate any values for " + VPrime);
			throw new IllegalStateException("Model does not validate any values for " + VPrime);
		}

		Expression M                            = null;
		Expression newProductOfIncomingMessages = null;
		if (!ExtensionalSet.isEmptySet(NPrime)) {
			Trace.log("if N' is not the empty set");

			Expression messageToFFromVPrime = Expressions.apply(LPIUtil.FUNCTOR_MSG_TO_FROM, F, VPrime);

			Trace.log("    products <- prod_{V in R_set_diff(N'\\{V'})} m_f<-V");
			if (Justification.isEnabled()) {
				Justification.beginEqualityStep("going to sum " + VPrime + " out, so we separate its message first");
			}
			
			Expression NPrimeMinusVPrime = LPIUtil.callSetDiff(NPrime, VPrime, process);
			newProductOfIncomingMessages = LPIUtil.makeProductOfMessages(indexOfOfIncomingMessagesSet, NPrimeMinusVPrime, msgToF_V, incomingMessagesSetCondition);
			Trace.log("    // products = {}", newProductOfIncomingMessages);
			
			if (Justification.isEnabled()) {
				Expression newHead = Expressions.apply(FunctorConstants.TIMES, E, messageToFFromVPrime, newProductOfIncomingMessages);
				currentExpression = replaceHead(currentExpression, newHead);
				Justification.endEqualityStep(currentExpression);
			}

			if (configuration.isSumRewriterUsingSingletonRelevantRangeHeuristic() && relevantRangeSoFar.size() == 1) {
				Trace.log("    if relevantRangeSoFar is singleton { v }");
				Trace.log("        M <- 1");
				if (Justification.isEnabled()) {
					Justification.beginEqualityStep("only possible value for " + vPrimeValue + " is " + relevantRangeSoFar.get(0) + "; we assume the incoming message will be consistent with that and just drop it");
				}
				
				M = Expressions.ONE;
				
				if (Justification.isEnabled()) {
					Expression newHead = Expressions.apply(FunctorConstants.TIMES, E, newProductOfIncomingMessages);
					currentExpression = replaceHead(currentExpression, newHead);
					Justification.endEqualityStep(currentExpression);
				}
			} 
			else {
				Trace.log("    if relevantRangeSoFar is not a singleton or heuristic is turned off");
				Trace.log("        M <- R_m_to_f_from_v(m_F<-V', beingComputed)");
				if (Justification.isEnabled()) {
					Justification.beginEqualityStep("computing incoming message from " + vPrimeValue);
				}
				
				M = process.rewrite(R_m_to_f_from_v, LPIUtil.argForMessageToFactorFromVariableRewriteCall(messageToFFromVPrime, beingComputed));
				Trace.log("        // M = {}", M);
				
				// For testing purposes.
				int testMessageCounter = configuration.getSumRewriterTestMessageCounter() + 1;
				configuration.setSumRewriterTestMessageCounter(testMessageCounter);
				
				if (LPIUtil.containsPreviousMessageExpressions(M)) {
					Trace.log("        if M is previous message to F from V'");
					Trace.log("            M <- (lambda v' : previous message to F from V')(v')");
					M = Expressions.apply(new DefaultLambdaExpression(arrayList(vPrimeValue), M), vPrimeValue);
				} 
				else {
					Trace.log("        if M is *not* previous message to F from V'");
					Trace.log("            relevantRangeSoFar = {v in relevantRangeSoFar : R_basic(M[v'/v]) is not zero }");
					relevantRangeSoFar = getRelevantRange(vPrimeValue, M, process);
					Trace.log("            // relevantRangeSoFar = {}", relevantRangeSoFar);
					
					if (relevantRangeSoFar.isEmpty()) {
						Trace.log("            if relevantRangeSoFar is {}");
						Trace.log("                throw exception: model does not validate any values for " + VPrime);
						throw new IllegalStateException("Model does not validate any values for " + VPrime);
					}
				}
				
				if (Justification.isEnabled()) {
					Expression newHead = Expressions.apply(FunctorConstants.TIMES, E, M, newProductOfIncomingMessages);
					currentExpression = replaceHead(currentExpression, newHead);
					Justification.endEqualityStep(currentExpression);
				}
			}
		} 
		else {
			Trace.log("else // N' is the empty set");
			Trace.log("    products <- 1");
			Trace.log("    M <- 1");
			Justification.beginEqualityStep("no incoming messages");
			newProductOfIncomingMessages = Expressions.ONE;
			M                            = Expressions.ONE;
			if (Justification.isEnabled()) {
				currentExpression = replaceHead(currentExpression, E);
				Justification.endEqualityStep(currentExpression);
			}
		}
		
		if (Justification.isEnabled()) {
			Justification.beginEqualityStep("summing " + VPrime + " out");	
		}

		Expression sumIndexWithoutVPrime = LPIUtil.callSetDiff(summationIndexN, VPrime, process);
		Expression EByM                  = Times.make(Arrays.asList(new Expression[] { E, M }));
		Trace.log("    // relevantRangeSoFar = {}", relevantRangeSoFar);
		Trace.log("    // M = {}", M);
		Trace.log("    // products = {}", newProductOfIncomingMessages);
		Trace.log("    // E*M = {}", EByM);
		Trace.log("return R_sum(sum_{R_set_diff(N\\{V'})} R_basic((E*M)[v'/v1] + ... + (E*M)[v'/vn]) * products, T, beingComputed)");
		Trace.log("       for relevantRangeSoFar in the form {v1,...,vn}");
		
		Expression[] substitutions = new Expression[relevantRangeSoFar.size()];
		for (int i = 0; i < substitutions.length; i++) {
			Expression v     = relevantRangeSoFar.get(i);
			substitutions[i] = SemanticSubstitute.replace(EByM, vPrimeValue, v, process);
		}
		Expression sumOfSubstitutions = Plus.make(Arrays.asList(substitutions));
		Trace.log("       // new expression to go through summation will be simpification of {}", sumOfSubstitutions);

		if (Justification.isEnabled()) {
			for (int i = 0; i < substitutions.length; i++) {
				currentExpression = replaceN(currentExpression, sumIndexWithoutVPrime);
				currentExpression =
					replaceHead(
							currentExpression,
							Expressions.apply(FunctorConstants.TIMES, sumOfSubstitutions, newProductOfIncomingMessages));
			}
			Justification.endEqualityStep(currentExpression);
		}
		
		Justification.beginEqualityStep("simplifying");
		Expression sumOfSubstitutionsValue = process.rewrite(R_basic, sumOfSubstitutions);
		if (Justification.isEnabled()) {
			for (int i = 0; i < substitutions.length; i++) {
				currentExpression = replaceHead(currentExpression, sumOfSubstitutionsValue);
			}
			Justification.endEqualityStep(currentExpression);
		}

		Trace.log("       // This simpification is {}", sumOfSubstitutionsValue);
		Justification.beginEqualityStep("recursively summing out the remaining random variables");
		Expression result = process.rewrite(R_sum,
								LPIUtil.argForSumRewriteCall(sumIndexWithoutVPrime, sumOfSubstitutionsValue, newProductOfIncomingMessages, T, beingComputed));
		Justification.endEqualityStep(result);

		return result;
	}

	public static Expression replaceN(Expression currentExpression, Expression newN) {
		Expression result = 
			Expressions.apply(
					FunctorConstants.SUM,
					((IntensionalSet) currentExpression.get(0)).setIndexExpressions(new ExtensionalIndexExpressionsSet(ExtensionalSet.getElements(newN))));
		return result;
	}

	public static Expression replaceHead(Expression currentExpression, Expression newHead) {
		Expression newSet = null;
		if (Sets.isIntensionalSet(currentExpression.get(0))) {
			newSet = ((IntensionalSet) currentExpression.get(0)).setHead(newHead);
		} 
		else {
			// must be a singleton
			newSet = ExtensionalSet.makeSingletonOfSameTypeAs(currentExpression.get(0), newHead);
		}
		Expression result = Expressions.apply(FunctorConstants.SUM, newSet);
		return result;
	}

	private List<Expression> getRelevantRange(Expression vPrimeValue, Expression E, RewritingProcess process) {
		// relevantRangeSoFar = {v in range(v') : R_basic(E[v'/v]) is not zero}
		List<Expression> relevantRangeSoFar = new ArrayList<Expression>();
		for (Expression v : Model.range(vPrimeValue, process)) {
			Expression subE = SemanticSubstitute.replace(E, vPrimeValue, v, process);
			Expression basicE = process.rewrite(R_basic, subE);
			if (!basicE.equals(Expressions.ZERO)) {
				relevantRangeSoFar.add(v);
			}
		}
		return relevantRangeSoFar;
	}
	
	//
	private RewriteOnBranch newCallSumRewrite() {
		return new RewriteOnBranch() {
			@Override
			public Expression rewrite(Expression[] expressions, RewritingProcess process) {
				Expression result = process.rewrite(R_sum, LPIUtil.argForSumRewriteCall(expressions[0], expressions[1], expressions[2], expressions[3], expressions[4]));
				
				return result;
			}
		};
	}
}
