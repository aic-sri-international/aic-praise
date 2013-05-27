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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.BranchRewriteTask;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.SubExpressionSelection;
import com.sri.ai.grinder.library.Variables;
import com.sri.ai.grinder.library.equality.CheapDisequalityModule;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.IterateValuesUsingExpansions;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.lbp.MessageExpansions;
import com.sri.ai.praise.lbp.UseValuesForPreviousMessages;
import com.sri.ai.praise.model.Model;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

/**
 * Default implementation of {@link LBPRewriter#R_belief} and supporting utility routines.
 * 
 * @author oreilly
 * 
 */
@Beta
public class Belief extends AbstractLBPHierarchicalRewriter implements LBPRewriter, MessageExpansions, IterateValuesUsingExpansions, UseValuesForPreviousMessages {
	private static Expression _emptySet = ExtensionalSet.makeEmptySetExpression();
	//
	private LBPConfiguration configuration = null;
	private Rational decimalPrecision = null;
	private Rational nonZeroMin = null;

	public Belief(LBPConfiguration configuration) {
		this.configuration     = configuration;
		setLimitPrecisionToNumberOfSignificantDecimals(configuration.getLimitPrecisionToNumberOfSignificantDecimals());
	}
	
	@Override
	public String getName() {
		return R_belief;
	}
	
	/**
	 * 
	 * @see MessageExpansions#getMessageExpansions(Expression, RewritingProcess)
	 */
	public Expression getMessageExpansions(Expression msgSets, RewritingProcess process) {		
		Expression result = null;
	
		List<Expression> msgSetUnionArguments        = getEntriesFromUnion(msgSets);
		List<Expression> msgExpansionsUnionArguments = getMessageExpansions(msgSetUnionArguments, new PreviousMessageToMsgValueCache(), Variables.freeVariables(msgSets, process), process);
		
		result = createUnionFromArgs(msgExpansionsUnionArguments);
		
		return result;
	}
	
	/**
	 * 
	 * @see IterateValuesUsingExpansions#iterateValuesUsingExpansions(Expression, Expression, RewritingProcess)
	 */
	public Expression iterateValuesUsingExpansions(Expression msgValues,
			Expression msgExpansions, RewritingProcess process) {
		
		Expression result = null;
		
		List<Expression> msgValuesUnionArguments     = getEntriesFromUnion(msgValues);
		List<Expression> msgExpansionsUnionArguments = getEntriesFromUnion(msgExpansions);
		
		List<Expression> nextMsgValuesUnionArguments = iterateValuesUsingExpansions(msgValuesUnionArguments, 
															msgExpansionsUnionArguments, 
															new PreviousMessageToMsgValueCache(),
															Variables.freeVariables(msgExpansions, process),
															process);
		
		result = createUnionFromArgs(nextMsgValuesUnionArguments);
		
		return result;
	}
	
	/**
	 * 
	 * @see UseValuesForPreviousMessages#useValuesForPreviousMessages(Expression, Expression, RewritingProcess)
	 */
	public Expression useValuesForPreviousMessages(Expression expansion,
			Expression msgValues, RewritingProcess process) {
		Expression result = null;
		
		List<Expression> msgValuesUnionArguments = getEntriesFromUnion(msgValues);

		result = useValuesForPreviousMessages(
				expansion, msgValuesUnionArguments,
				new PreviousMessageToMsgValueCache(),
				process);

		return result;
	}

	/**
	 * @see LBPRewriter#R_belief
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression belief, RewritingProcess process) {
		
		// Assert input arguments
		LPIUtil.assertBeliefOk(belief, process);
		
		// Ensure set to the latest configuration settings.
		setLimitPrecisionToNumberOfSignificantDecimals(configuration.getLimitPrecisionToNumberOfSignificantDecimals());
		
		Expression randomVariable = belief.get(0);
		
		Set<Expression> freeVariablesFromBeliefQuery = Variables.freeVariables(randomVariable, process);
		
		Trace.log("beingComputed    <- empty set");
		Expression beingComputed = LPIUtil.createNewBeingComputedExpression();
		Trace.log("belief_expansion <- R_prod_factor(prod_F in R_neigh_v(Neigh(V)) m_V<-F, beingComputed)");

		// prod_F in R_neigh_v(Neigh(V)) m_V<-F
		Expression factorIndex = Expressions.makeUniqueVariable("F", randomVariable, process);

		if (Justification.isEnabled()) {
			Justification.log(Expressions.apply(LPIUtil.FUNCTOR_BELIEF, randomVariable, Model.getModelDefinition(process)));

			Justification.beginEqualityStep("lifted belief propagation");

			Justification.log(belief); // detailed justification

			Justification.beginEqualityStep("definition of belief");

			Expression expandedBelief =
					Expressions.apply(
							LPIUtil.FUNCTOR_NORMALIZE,
							LPIUtil.makeProductOfMessages(
									factorIndex,
									Expressions.apply(LPIUtil.FUNCTOR_NEIGHBOR, randomVariable),
									Expressions.apply(LPIUtil.FUNCTOR_MSG_TO_FROM, randomVariable, factorIndex),
									Expressions.TRUE));

			Justification.endEqualityStep(expandedBelief);
		}

		// R_neigh_v(Neigh(V))
		Justification.beginEqualityStep("neighbors of random variable");

		Expression neighV          = Expressions.make(LPIUtil.FUNCTOR_NEIGHBOR, randomVariable);
		Expression rewriteOfNeighV = process.rewrite(R_neigh_v, neighV);

		Expression msgToV_F = Expressions.make(LPIUtil.FUNCTOR_MSG_TO_FROM, randomVariable, factorIndex);
		Expression product = LPIUtil.makeProductOfMessages(factorIndex, rewriteOfNeighV, msgToV_F, Expressions.TRUE);

		if (Justification.isEnabled()) {
			Justification.endEqualityStep(Expressions.apply(LPIUtil.FUNCTOR_NORMALIZE, product));
		}

		// R_prod_factor(prod_F in R_neigh_v(Neigh(V)) m_V<-F)
		Justification.beginEqualityStep("product of messages over factors");

		Expression beliefExpansion = process.rewrite(R_prod_factor, LPIUtil.argForProductFactorRewriteCall(product, beingComputed));

		if (Justification.isEnabled()) {
			Justification.endEqualityStep(Expressions.apply(LPIUtil.FUNCTOR_NORMALIZE, beliefExpansion));
		}
		
		Justification.beginEqualityStep("complete simplification of unnormalized belief expansion");
		Justification.log(beliefExpansion);
		Trace.log("belief_expansion <- R_complete_simplify(belief_expansion)");
		beliefExpansion = process.rewrite(R_complete_simplify, beliefExpansion);
		if (Justification.isEnabled()) {
			Justification.endEqualityStep(Expressions.apply(LPIUtil.FUNCTOR_NORMALIZE, beliefExpansion));
		}
	
		Expression result = null;
		if (!LPIUtil.containsPreviousMessageExpressions(beliefExpansion)) {
			Justification.beginEqualityStep("normalize and return belief since it does not depend on previous iteration messages");
			Trace.log("if belief_expansion does not contain 'previous message' expressions");
			Trace.log("    return R_normalize(V, belief_expansion)");
			
			Justification.beginEqualityStep("normalization");
			Expression normalizedBeliefExpansion = process.rewrite(R_normalize, LPIUtil.argForNormalizeRewriteCall(randomVariable, beliefExpansion));
			Justification.endEqualityStep(normalizedBeliefExpansion);
			
			Justification.endEqualityStep(normalizedBeliefExpansion);
			result = normalizedBeliefExpansion;
		} 
		else {
			Justification.beginEqualityStep("since belief does depends on previous iteration messages, we expand them too and iterate over all of them");
			// At this point, belief_expansion is a basic expression containing 'previous message' expressions
			
			Trace.log("msg_sets <- R_extract_previous_msg_sets(belief_expansion)");
			// a union of sets of pairs (N1, N2), with each pair representing an occurrence of 
			// "previous message" on that pair occurring in belief_expansion.
			List<Expression> msgSets = getEntriesFromUnion(process.rewrite(R_extract_previous_msg_sets, beliefExpansion));		
		
			Trace.log("msg_expansions <- get_msg_expansions(msg_sets)");
			// a union of intensional sets containing tuples of the form:
			// (Destination, Origin, Expansion), where Expansion is a basic expression
			// possibly containing "previous message" expressions, representing their 
			// value in terms of messages from the previous loopy BP iteration.
			PreviousMessageToMsgValueCache previousMessageToMsgValueCache = new PreviousMessageToMsgValueCache(); 
			List<Expression> msgExpansions = getMessageExpansions(msgSets, previousMessageToMsgValueCache, freeVariablesFromBeliefQuery, process);
			
			Trace.log("msg_values <- copy of msg_expansions, with the values replaced by uniform messages");
			// a union of sets of tuples (N1, N2, value) where (N1,N2) represents a message and value
			// is a basic expression representing its value at the current loopy BP iteration
			// (this expression is therefore free of previous message expressions).
			// {{ (on I) (Destination, Origin, 1) | C }} union ...
			List<Expression> msgValues = new ArrayList<Expression>();
			for (Expression msgExpansion : msgExpansions) {
				// i.e. (Destination, Origin, Expansion)
				Expression tupleExpansion = IntensionalSet.getHead(msgExpansion);
				// (Destination, Origin, 1)
				Expression tuple          = Tuple.make(Tuple.get(tupleExpansion, 0),
													   Tuple.get(tupleExpansion, 1),
													   Expressions.ONE);
				// {{ (on I) (Destination, Origin, 1) | C }}
				Expression msgValue       = IntensionalSet.make(Sets.getLabel(msgExpansion), 
																IntensionalSet.getScopingExpression(msgExpansion),
																tuple, 
																IntensionalSet.getCondition(msgExpansion));
				msgValues.add(msgValue);
			}
			
			// We now use the expansions to get the values of message pairs in successive loopy BP iterations
			// until either convergence or maximum number of iterations.
			Trace.log("while belief_value <- use_values_for_previous_msgs(belief_expansion, msg_values) not final");
			Trace.log("    msg_values <- iterate_values_using_expansions(msg_values, msg_expansions)");
			
			Expression beliefValue          = beliefExpansion;
			Expression priorBeliefValue     = beliefExpansion;
			List<Expression> priorMsgValues = new ArrayList<Expression>();
			int iteration = 1;
			beliefValue = useValuesForPreviousMessages(beliefExpansion, msgValues, previousMessageToMsgValueCache, process);
			notifyCollector(randomVariable, beliefValue, 1, process);
			Justification.log("Initial belief value {}", beliefValue);
			while (notFinal(beliefValue, priorBeliefValue, msgValues, priorMsgValues, iteration)) {
				priorBeliefValue = beliefValue;
				priorMsgValues   = msgValues;
				Justification.beginEqualityStep("iteration");
				LiftProductOfFactorToVariable.setMustAlwaysLift(true, process);
				msgValues        = iterateValuesUsingExpansions(msgValues, msgExpansions, previousMessageToMsgValueCache, freeVariablesFromBeliefQuery, process);
				beliefValue      = useValuesForPreviousMessages(beliefExpansion, msgValues, previousMessageToMsgValueCache, process);
				LiftProductOfFactorToVariable.setMustAlwaysLift(false, process);
				iteration++;			
				notifyCollector(randomVariable, beliefValue, iteration, process);
				Justification.endEqualityStep(beliefValue);
			}
			
			Trace.log("return R_normalize(V, belief_value)");
			
			Justification.log("Normalization of final unnormalized belief value {}", beliefValue);
			Justification.beginEqualityStep("normalization");
			Expression normalizedBeliefValue = process.rewrite(R_normalize, LPIUtil.argForNormalizeRewriteCall(randomVariable, beliefValue));
			Justification.endEqualityStep(normalizedBeliefValue);
			
			Justification.endEqualityStep(normalizedBeliefValue); // this endStep closes entire set of iterations
			result = normalizedBeliefValue;
		}
		
		Justification.endEqualityStep(result);
		
		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	private void setLimitPrecisionToNumberOfSignificantDecimals(int precision) {
		this.decimalPrecision = new Rational(10).pow(precision);
		this.nonZeroMin       = new Rational(1).divide(this.decimalPrecision);
	}
	
	private void notifyCollector(Expression randomVariable, Expression beliefValue, int iteration, RewritingProcess process) {
		LBPConfiguration.BeliefValueAtIterationCollector beliefValueAtIterationCollector = configuration.getBeliefValueAtIterationCollector();
		if (beliefValueAtIterationCollector != null) {
			if (beliefValueAtIterationCollector.isThisIterationWanted(iteration)) {
				Expression normalizedBeliefValue = process.rewrite(R_normalize, LPIUtil.argForNormalizeRewriteCall(randomVariable, beliefValue));
				beliefValueAtIterationCollector.setBeliefValueAtIteration(iteration, normalizedBeliefValue);
			}
		}
	}
	
	private List<Expression> getMessageExpansions(List<Expression> msgSets, PreviousMessageToMsgValueCache previousMessageToMsgValueCache, Set<Expression> freeVariablesFromBeliefQuery, RewritingProcess process) {		
		Trace.in("+get_msg_expansions({})", Tuple.make(msgSets));
		
		Trace.log("msg_expansions         <- empty set");
		Trace.log("msgs_alreaded_expanded <- empty set");
		Trace.log("msgs_to_be_expanded    <- msg_set");
		List<Expression> msgExpansions                = new ArrayList<Expression>();
		List<Expression> msgsAlreadyExpandedUnionArgs = new ArrayList<Expression>();
		List<Expression> msgsToBeExpanded             = new ArrayList<Expression>(msgSets);
		
		Justification.begin("Going to expand messages in {}", msgSets);
		
		Trace.log("while msgs_to_be_expanded is not empty");
		while (msgsToBeExpanded.size() > 0) {
			Trace.log("    msg_set <- remove a union argument from msgs_to_be_expanded");
			Expression msgSet = msgsToBeExpanded.remove(0);
			Trace.log("    // msg_set={}", msgSet);
			// Ensure is a legal msg_set expression
			assertIsLegalMsgSet(msgSet);
	
			Justification.begin("Going to expand message set {}", msgSet);
			
			// Note - approach discussed with Rodrigo to work around R_set_diff 
			// not supporting both arguments being intensional multisets:
			// "I realize now that for dealing with this situation with (D,O) pairs we
			//  don't need to extend the routine. Because they are guaranteed to have 
			//  unique elements, we can simply convert them to uni-sets before doing the
			//  set difference."
			Justification.begin("Going to subtract message instantiations that were already expanded");
			
			// Note: Minor Optimization, up front limit msgs_already_expanded to only those 
			// that can possibly intersect.
			List<Expression> possibleIntersectionUnionArgs = new ArrayList<Expression>();
			Expression alpha = IntensionalSet.getHead(msgSet);
			for (Expression maeua : msgsAlreadyExpandedUnionArgs) {				
				Expression alphaPrime = IntensionalSet.getHead(maeua); 
				// Perform a cheap disequality first
				if (!CheapDisequalityModule.isACheapDisequality(alpha, alphaPrime, process)) {
					possibleIntersectionUnionArgs.add(maeua);
				}
			}
			Expression msgsAlreadyExpanded = createUnionFromArgs(possibleIntersectionUnionArgs);
			
			Trace.log("    msg_set <- R_set_diff(msg_set minus msgs_already_expanded)");
			Expression uniMsgSet = msgSet; //convertIntensionalMultiSetToUniSet(msgSet);
			msgSet = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(uniMsgSet, msgsAlreadyExpanded));
			Trace.log("    msg_set <- R_complete_simplify(msg_set)");
			msgSet = process.rewrite(R_complete_simplify, msgSet);
			Justification.end("Remaining, non-expanded messages are {}", msgSet);
			Trace.log("    // msg_set = {}", msgSet);
			
			if (!Sets.isEmptySet(msgSet)) {
				// convert msg set back to a multiset
				//msgSet = convertUniSetToIntensionalMultiSet(msgSet);
				
				Justification.begin("Going to re-arrange as message expression");
				Trace.log("    if msg_set is not empty");
				// represent msg_set as { (on I) (Destination, Origin) | C }
				Expression expressionI            = IntensionalSet.getScopingExpression(msgSet);
				Expression tupleDestinationOrigin = IntensionalSet.getHead(msgSet);
				Expression destination            = Tuple.get(tupleDestinationOrigin, 0);
				Expression origin                 = Tuple.get(tupleDestinationOrigin, 1);
				Expression conditionC             = IntensionalSet.getCondition(msgSet);
				
				Trace.log("        expansion <- compute message to Destination from Origin with");
				Trace.log("                     beingComputed = empty set and contextual constraint C");
				Expression       msgTo_From    = Expressions.make(LPIUtil.FUNCTOR_MSG_TO_FROM, destination, origin);
				// Set up the contextual constraint
				RewritingProcess cSubProcess   = GrinderUtil.extendContextualVariablesAndConstraint(expressionI, conditionC, process);
				Justification.end("Message expression is {}", msgTo_From);
				
				Justification.begin("Going to symbolically compute message expression");
				Expression expansion = null;
				Justification.begin("Going to symbolically compute unsimplified message expression");
				// Determine which 'message to . from .' rewriter to call
				if (BracketedExpressionSubExpressionsProvider.isRandomVariable(destination, process)) {
					expansion = cSubProcess.rewrite(R_m_to_v_from_f,
									LPIUtil.argForMessageToVariableFromFactorRewriteCall(msgTo_From, LPIUtil.createNewBeingComputedExpression()));
				} 
				else {
					expansion = cSubProcess.rewrite(R_m_to_f_from_v,
									LPIUtil.argForMessageToFactorFromVariableRewriteCall(msgTo_From, LPIUtil.createNewBeingComputedExpression()));
				}
				Justification.end("Obtained unsimplified message expansion {}", expansion);
				
				Justification.begin("Going to simplify {}", expansion);
				Trace.log("        expansion <- R_complete_simplify(expansion)");
				// Note: use cSubProcess for R_complete_simplify call as this is the context
				// under which the expansion was generated and we want to remove any
				// invalid branches based on this.
				expansion = cSubProcess.rewrite(R_complete_simplify, expansion);
				Trace.log("        // expansion = {}", expansion);
				Justification.end("Obtained simplified message expansion {}", expansion);
				Justification.end("Obtained message expansion {}", expansion);
				
				Trace.log("        index <- size of msg_expansions");
				Trace.log("        cache index as msg_value_index corresponding to 'previous message to Destination from Origin' under constraining condition C");
				previousMessageToMsgValueCache.addPreviousMessageToMsgValueIndex(
						conditionC,
						Expressions.make(LPIUtil.FUNCTOR_PREVIOUS_MSG_TO_FROM, destination, origin),
						msgExpansions.size());
				
				Trace.log("        msg_expansion <- {{ (on I) (Destination, Origin, expansion) | C }}");
				Expression msgExpansion = IntensionalSet.makeUniSet(expressionI, 
						Tuple.make(destination, origin, expansion), 
						conditionC);
				
				if (isFreeVariablesIntroduced(msgExpansion, freeVariablesFromBeliefQuery, process)) {
					System.err.println("IllegalStateException: introduced additional free variables into msg_expansion.");
					System.err.println("msg_expansion ="+msgExpansion);
					System.err.println("msg_set       ="+msgSet);
					System.err.println("msg_expansions=");
					for (Expression me : msgExpansions) {
						System.err.println(me);
					}
					throw new IllegalStateException("IllegalStateException: introduced additional free variables into msg_expansion: "+msgExpansion);
				}
				
				if (isAnswerDependentOnLogicalVariableMessageIsNot(msgExpansion, process)) {
					System.err.println("IllegalStateException: msg_expansion has answer dependent on logical variable that value is not.");
					System.err.println("msg_expansion ="+msgExpansion);
					System.err.println("msg_set       ="+msgSet);
					System.err.println("msg_expansions=");
					for (Expression me : msgExpansions) {
						System.err.println(me);
					}
					throw new IllegalStateException("IllegalStateException: msg_expansion has answer dependent on logical variable that value is not: "+msgExpansion);
				}
				
				Trace.log("        msg_expansions <- msg_expansions union msg_expansion");
				msgExpansions.add(msgExpansion);
				
				Trace.log("        msgs_already_expanded <- msgs_already_expanded union { (on I) (Destination, Origin) | C }");
				Expression alreadyExpanded = IntensionalSet.makeUniSet(expressionI, 
						                     	Tuple.make(destination, origin),
						                        conditionC);
				
				msgsAlreadyExpandedUnionArgs.add(alreadyExpanded);
				Trace.log("        // msgs_already_expanded = {}", Expressions.apply(FunctorConstants.UNION, msgsAlreadyExpandedUnionArgs));
				
				Trace.log("        msgs_to_be_expanded <- msgs_to_be_expanded union R_extract_previous_msg_sets(msg_expansion)");

				Justification.begin("Going to identify messages inside expansion and schedule them for their own expansion");
				Expression newMsgSetsToBeExpanded = process.rewrite(R_extract_previous_msg_sets, msgExpansion);
				msgsToBeExpanded.addAll(getEntriesFromUnion(newMsgSetsToBeExpanded));
				Justification.end("Messages inside expansion identified: {}", newMsgSetsToBeExpanded);
				Trace.log("        // msgs_to_be_expanded = {}", Tuple.make(msgsToBeExpanded));

				Justification.end("Message set expanded. See inside for details.");
			}
			else {
				Justification.end("All messages in this set have already been expanded, so there is nothing to do.");
			}
		}
		
		Trace.log("return msg_expansions");
		
		Trace.out("-get_msg_expansions={}", Expressions.apply("list", msgExpansions));
		
		Justification.end("Messages expansions are {}", msgExpansions);

		return msgExpansions;
	}
	
	private List<Expression> iterateValuesUsingExpansions(final List<Expression> msgValues,
			final List<Expression> msgExpansions, final PreviousMessageToMsgValueCache previousMessageToMsgValueCache, final Set<Expression> freeVariablesFromBeliefQuery,
			RewritingProcess process) {
		
		Trace.in("+iterate_values_using_expansions({}, {})", msgValues, msgExpansions);
		
		Trace.log("next_msg_values <- empty set ");
		List<Expression> nextMsgValuesUnionArguments = new ArrayList<Expression>();
		Trace.log("for each union argument {{ (on I) (Destination, Origin, Expansion) | C }} in msg_expansions");
		BranchRewriteTask[] iterateValueTasks = new BranchRewriteTask[msgExpansions.size()];
		for (int i = 0; i < msgExpansions.size(); i++) {
			Expression msgExpansion = msgExpansions.get(i);
			
			RewriteOnBranch iterateValueTask = new RewriteOnBranch() {
				@Override
				public Expression rewrite(Expression[] expressions, RewritingProcess process) {
					Expression msgExpansion      = expressions[0];
					Expression msgExpansionTuple = IntensionalSet.getHead(msgExpansion);
					Expression destination       = Tuple.get(msgExpansionTuple, 0);
					Expression origin            = Tuple.get(msgExpansionTuple, 1);
					Expression expansion         = Tuple.get(msgExpansionTuple, 2);
					Trace.log("    // msgExpansion={}", msgExpansion);
					Trace.log("    // expansion   ={}", expansion);
					
					RewritingProcess subProcess = GrinderUtil.extendContextualVariablesAndConstraintWithIntensionalSet(msgExpansion, process);
					
					Trace.log("    value <- use_values_for_previous_msgs(Expansion, msg_values) under contextual constraint expanded by C");
					Expression value = useValuesForPreviousMessages(expansion, msgValues, previousMessageToMsgValueCache, subProcess);
					Trace.log("    // value={}", value);
					
					if (LPIUtil.containsPreviousMessageExpressions(value) || LPIUtil.containsProductExpressions(value)) {	
						// Output some useful information before throwing the exception
						System.err.println("IllegalStateException: invalid message value.");
						System.err.println("sub.context      ="+subProcess.getContextualConstraint());
						System.err.println("sub.context vars ="+subProcess.getContextualVariables());
						System.err.println("msg_expansion    ="+msgExpansion);
						System.err.println("expansion        ="+expansion);
						System.err.println("value            ="+value);
						System.err.println("msg_expansions   =");
						for (Expression me : msgExpansions) {
							System.err.println(me);
						}
						System.err.println("msg_values       =");
						for (Expression m : msgValues) {
							System.err.println(""+m);
						}
						throw new IllegalStateException("new msg_value contains previous message to . from . or product(...):"+value);
					}
					
					Expression destinationOrOriginRandomVariable = LPIUtil.isRandomVariable(destination, subProcess)? destination : origin;
					Expression destinationOrOriginRandomVariableValue = BracketedExpressionSubExpressionsProvider.getRandomVariableValueExpression(destinationOrOriginRandomVariable);
					List<Pair<Expression, Expression>> otherRandomVariables;
					if ((otherRandomVariables = LPIUtil.findRandomVariableValueExpressionsThatAreNotNecessarilyTheSameAsAGivenOne(value, destinationOrOriginRandomVariableValue, subProcess)
							).size() != 0) {
						// Output some useful information before throwing the exception
						System.err.println("IllegalStateException: value depends on random variable value other than its own destination/origin random variable.");
						System.err.println("Destination: " + destination);
						System.err.println("Origin: "      + origin);
						System.err.println("Message value should be a function of: " + destinationOrOriginRandomVariableValue);
						System.err.println("value            ="+value);
						System.err.println("Other random variable values and contexts in which they appear:\n" + Util.join(otherRandomVariables, "\n"));
						System.err.println("sub.context      ="+subProcess.getContextualConstraint());
						System.err.println("sub.context vars ="+subProcess.getContextualVariables());
						System.err.println("msg_expansion    ="+msgExpansion);
						System.err.println("expansion        ="+expansion);
						System.err.println("msg_expansions   =");
						for (Expression me : msgExpansions) {
							System.err.println(me);
						}
						System.err.println("msg_values       =");
						for (Expression m : msgValues) {
							System.err.println(""+m);
						}
						throw new IllegalStateException("value depends on random variable value other than its own destination/origin random variable: " + value);
					}
					
					// Normalize and manage precision
					Expression normalizedValue = value;
					if (Model.isAllDomainSizesKnown(process)) {
						Expression randomVariable = null;
						if (BracketedExpressionSubExpressionsProvider.isRandomVariable(
								destination, process)) {
							randomVariable = destination;
						} 
						else {
							randomVariable = origin;
						}
						normalizedValue = process.rewrite(R_normalize, LPIUtil.argForNormalizeRewriteCall(randomVariable, value));
						normalizedValue = limitPrecisionToNumberOfSignificantDecimalPlaces(normalizedValue, process);
					}
					
					Trace.log("    next_msg_values <- next_msg_values union {{ (on I) (Destination, Origin, value) | C }}");
					Expression tupleTriple       = Tuple.make(destination, origin, normalizedValue);
					Expression newMsgValue = IntensionalSet
												.makeUniSetFromIndexExpressionsList(
														IntensionalSet.getIndexExpressions(msgExpansion), 
														tupleTriple, 
														IntensionalSet.getCondition(msgExpansion));
					
					if (isFreeVariablesIntroduced(newMsgValue, freeVariablesFromBeliefQuery, process)) {
						System.err.println("IllegalStateException: introduced additional free variables into new_msg_value.");
						System.err.println("sub.context      ="+subProcess.getContextualConstraint());
						System.err.println("sub.context vars ="+subProcess.getContextualVariables());
						System.err.println("msg_expansion    ="+msgExpansion);
						System.err.println("expansion        ="+expansion);
						System.err.println("value            ="+value);
						System.err.println("new_msg_value    ="+newMsgValue);
						System.err.println("msg_expansions   =");
						for (Expression me : msgExpansions) {
							System.err.println(me);
						}
						System.err.println("msg_values       =");
						for (Expression mv : msgValues) {
							System.err.println(mv);
						}
						throw new IllegalStateException("IllegalStateException: introduced additional free variables into new_msg_value: "+newMsgValue);
					}
					
					if (isAnswerDependentOnLogicalVariableMessageIsNot(newMsgValue, process)) {
						System.err.println("IllegalStateException: new_msg_value has answer dependent on logical variable that value is not.");
						System.err.println("sub.context      ="+subProcess.getContextualConstraint());
						System.err.println("sub.context vars ="+subProcess.getContextualVariables());
						System.err.println("msg_expansion    ="+msgExpansion);
						System.err.println("expansion        ="+expansion);
						System.err.println("value            ="+value);
						System.err.println("new_msg_value    ="+newMsgValue);
						System.err.println("msg_expansions   =");
						for (Expression me : msgExpansions) {
							System.err.println(me);
						}
						System.err.println("msg_values       =");
						for (Expression mv : msgValues) {
							System.err.println(mv);
						}
						throw new IllegalStateException("IllegalStateException: new_msg_value has answer dependent on logical variable that value is not: "+newMsgValue);
					}
					
					return newMsgValue;
				}
			};
			
			iterateValueTasks[i] = new BranchRewriteTask(iterateValueTask, new Expression[] {msgExpansion});
		}
		
		List<Expression> taskResults = GrinderUtil.branchAndMergeTasks(iterateValueTasks, process);
		
		nextMsgValuesUnionArguments.addAll(taskResults);
		
		Trace.log("return next_msg_values");
		Trace.out("-iterate_values_using_expansions={}", nextMsgValuesUnionArguments);
		
		return nextMsgValuesUnionArguments;
	}

	private Expression useValuesForPreviousMessages(Expression expansion,
			List<Expression> msgValues, PreviousMessageToMsgValueCache previousMessageToMsgValueCache, 
			RewritingProcess process) {
		Expression result = null;

		Trace.in("+use_values_for_previous_msgs({}, {})", expansion, Expressions.apply("list", msgValues)); // "list" used so msgValues is properly shown in trace tree.
		Trace.log("substituted <- expansion");
		Expression substituted = expansion;

		Trace.log("replace all expressions in substituted with the following replacement function:");
		substituted = substituted.replaceAllOccurrences(
										new PreviousMessageReplacementFunction(msgValues, previousMessageToMsgValueCache), 
										process);
			
		Trace.log("// substituted = {}", substituted);
		Trace.log("return R_complete_simplify(substituted)");
		result = process.rewrite(R_complete_simplify, substituted);
		
		Trace.out("-use_values_for_previous_msgs={}", result);
		
		return result;
	}
	
	/**
	 * <pre>
     * find_msg_value_matching_previous_message(prev_message, msg_values)
     * prev_message is of the form: previous message to Destination' from Origin'
     * msg_value is of the form: {{ (on I) (Destination, Origin, value) | C }}
     * 
     * Intersection <- null
     * msg_value    <- null
     * if previous_message msg_value_index cached
     *     index     <- from cache
     *     msg_value <- standardize msg_values[index] apart from prev_message
     *     if Intersection cached for previous_message
     *         Intersection <- cache
     *     else
     *         Intersection <- calculate_intersection(prev_message, msg_value)
     *         cache Intersection under prev_message’s contextual constraint
     * else
     *     for each i in 1..length(msg_values)
     *         msg_value    <- standardize msg_values[i] apart from prev_message
     *         Intersection <- calculate_intersection(prev_message, msg_value)
     *         if Intersection not empty
     *             // have found msg_value for prev_message
     *             index <- i
     *             cache index
     *             cache Intersection under prev_message’s contextual constraint
     *             exit for each loop
     *             
     * pickFrom = { (on I) value | Condition from Intersection }
     * v <- pick_single_element(pickFrom)
     * // pick_single_element returns a representation of the single
     * // element of this set in terms of the free variables in the
     * // previous message expression,
     * // rather than the indices of the intensional set.
     * // The set is a singleton because of two reasons: it is not empty,
     * // and it is the intersection of a singleton set with another set
     * // It is not empty because it is formed out of Intersection’s condition and Intersection is not empty;
     * // It is the intersection of a singleton set and another set because the second set in calculate_intersection
     * // is a singleton.
     * 
     * return v
	 * </pre>
	 */
	private Expression findMsgValueMatchingPreviousMessage(Expression prevMessage, List<Expression> msgValues, 
			PreviousMessageToMsgValueCache previousMessageToMsgValueCache, RewritingProcess process) {
		Expression result = null;
		Trace.in("+findMsgValueMatchingPreviousMessage({},{})", prevMessage, Expressions.apply("list", msgValues));
		//
		// Intersection <- null
		// msg_value    <- null
		Expression intersection = null;
		Expression msgValue     = null;
		CachedMsgValueIntersection cachedMsgValueIntersection    = null;
		boolean                    newCachedMsgValueIntersection = true;
		Integer cachedIndex = previousMessageToMsgValueCache.getCachedMessageValueIndex(process, prevMessage);
		// if previous_message msg_value_index cached
		if (cachedIndex != null) {
			// index <- index from cache
			int index = cachedIndex;
			Trace.log("Set of msg values set cached for {} under contextual constraint {}: {}", prevMessage, process.getContextualConstraint(), msgValues.get(index));
			// msg_value <- standardize msg_values[index] apart from prev_message
			msgValue = StandardizedApartFrom.standardizedApartFrom(msgValues.get(index), prevMessage, process);
			Trace.log("When standardized apart, it is {}", msgValue);
			cachedMsgValueIntersection = previousMessageToMsgValueCache.getCachedMsgValueIntersection(process, prevMessage);
			// if Intersection cached for previous_message
			if (cachedMsgValueIntersection != null) {
				newCachedMsgValueIntersection = false;
				// Intersection <- cache
				intersection = cachedMsgValueIntersection.intersection;
				Trace.log("Cached intersection for {}: {}", prevMessage, intersection);
			} 
			else {
				Trace.log("No intersection cached for {}", prevMessage);
				// Intersection <- calculate_intersection(prev_message, msg_value)
				intersection = calculateIntersection(prevMessage, msgValue, process);
				Trace.log("Intersection calculated as {}", intersection);
				// cache Intersection under prev_message’s contextual constraint
				cachedMsgValueIntersection = new CachedMsgValueIntersection(intersection,
						IntensionalSet.getScopingExpression(msgValue),
						IntensionalSet.getCondition(msgValue));
				previousMessageToMsgValueCache.addCachedMsgValueIntersection(process, prevMessage, cachedMsgValueIntersection);
				Trace.log("Adding cache item for {}: intersection {}, scoping expression of msg value {}, condition {}",
						prevMessage, intersection, IntensionalSet.getScopingExpression(msgValue), IntensionalSet.getCondition(msgValue));
			}
		} 
		else {
			Trace.log("No cached msg values set for {}", prevMessage);
			// for each i in 1..length(msg_values)
			for (int i = 0; i < msgValues.size(); i++) {
				// msg_value    <- standardize msg_values[i] apart from prev_message
				Trace.log("Examining candidate {}", msgValues.get(i));
				msgValue = StandardizedApartFrom.standardizedApartFrom(msgValues.get(i), prevMessage, process);
				Trace.log("When standardized apart, it is {}", msgValue);
				// Intersection <- calculate_intersection(prev_message, msg_value)
				intersection = calculateIntersection(prevMessage, msgValue, process);
				Trace.log("Intersection calculated as {}", intersection);
				// if Intersection not empty
				if (!Sets.isEmptySet(intersection)) {
					Trace.log("Intersection is not empty, so using msg values set {}", msgValue);
					// have found msg_value for prev_message
					// index <- i
					int index = i;
					// cache index
					previousMessageToMsgValueCache.addPreviousMessageToMsgValueIndex(process.getContextualConstraint(), prevMessage, index);
					// cache Intersection under prev_message’s contextual constraint
					cachedMsgValueIntersection = new CachedMsgValueIntersection(intersection,
							IntensionalSet.getScopingExpression(msgValue),
							IntensionalSet.getCondition(msgValue));
					
					previousMessageToMsgValueCache.addCachedMsgValueIntersection(process, prevMessage, cachedMsgValueIntersection);
					Trace.log("Adding cache item for {}: intersection {}, scoping expression of msg value {}, condition {}",
							prevMessage, intersection, IntensionalSet.getScopingExpression(msgValue), IntensionalSet.getCondition(msgValue));
					// exit "for each" loop
					break;
				}
			}
		}
		
		//
		// Implementation Sanity check logic to ensure things are being matched up as expected.
		Expression cachedScopingExpressionFromMsgValue = cachedMsgValueIntersection.scopingExpressionFromMsgValue;
		if (cachedScopingExpressionFromMsgValue == null) {
			cachedScopingExpressionFromMsgValue = IntensionalSet.EMPTY_SCOPING_EXPRESSION;
		}
		Expression currentScopingExpressionFromMsgValue = IntensionalSet.getScopingExpression(msgValue);
		if (currentScopingExpressionFromMsgValue == null) {
			currentScopingExpressionFromMsgValue = IntensionalSet.EMPTY_SCOPING_EXPRESSION;
		}
		if (!cachedScopingExpressionFromMsgValue.equals(currentScopingExpressionFromMsgValue)
			||
			!cachedMsgValueIntersection.conditionFromMsgValue.equals(IntensionalSet.getCondition(msgValue))) {
			// Output some useful information before throwing the exception
			System.err.println("IllegalStateException: cache and message value mismatch.");
			System.err.println("context       ="+process.getContextualConstraint());
			System.err.println("context vars  ="+process.getContextualVariables());
			System.err.println("prev_msg      ="+prevMessage);
			System.err.println("intersection  ="+intersection);
			System.err.println("cachedIndex   ="+cachedIndex);
			if (cachedIndex != null) {
				System.err.println("new cached    ="+newCachedMsgValueIntersection);
				System.err.println("msg_value(idx)="+msgValues.get(cachedIndex));
			}
			System.err.println("msg_value     ="+msgValue);
			System.err.println("msg_values    =");
			for (Expression m : msgValues) {
				System.err.println(""+m);
			}
			throw new IllegalStateException("Cached msgValue scoping expression ["
						+cachedMsgValueIntersection.scopingExpressionFromMsgValue
						+"] or cached condition [" 
						+cachedMsgValueIntersection.conditionFromMsgValue
						+"] don't match indexed msgValue="+msgValue);
		}	
		// End Implementation Sanity check logic
		//
		
		// pickFrom = { (on I) value | Condition from Intersection }
		Expression pickFrom = IntensionalSet.makeUniSet(
				IntensionalSet.getScopingExpression(msgValue),
				Tuple.get(IntensionalSet.getHead(msgValue), 2), 
				IntensionalSet.getCondition(intersection));

		Trace.log("We now pick the msg value for the current previous message by picking the single element in: {}", pickFrom);
		// v <- pick_single_element(pickFrom)
		Trace.log("v <- pick_single_element({})", pickFrom); 
		Expression v = LPIUtil.pickSingleElement(pickFrom, process);
		if (v == null) {
			// Output some useful information before throwing the exception
			System.err.println("IllegalStateException: failed to pick single element.");
			System.err.println("context     ="+process.getContextualConstraint());
			System.err.println("context vars="+process.getContextualVariables());
			System.err.println("prev_msg    ="+prevMessage);
			System.err.println("msg_value   ="+msgValue);
			System.err.println("intersection="+intersection);
			System.err.println("pickFrom    ="+pickFrom);
			System.err.println("msg_values  =");
			for (Expression m : msgValues) {
				System.err.println(""+m);
			}
			throw new IllegalStateException("Failed to pick single element:"+pickFrom);
		}
		Trace.out("-findMsgValueMatchingPreviousMessage={}", v);
		// return v
		result = v;
		
		return result;
	}
	
	/**
	 * <pre>
	 * calculate_intersection(prev_message, msg_value)
	 * prev_message is of the form: previous message to Destination' from Origin'
	 * msg_value is of the form: {{ (on I) (Destination, Origin, value) | C }}
	 * 
	 * // We want to find out if this previous message is covered by
	 * // {{ (on I) (Destination, Origin, value) | C }}
	 * // This can be done by computing the intersection below,
	 * // where we range over all possible values
	 * // for the previous message by excluding them:
	 * Intersection <- R_intersection(
	 *                 {{ (on I) (Destination, Origin) | C }}
	 *                 intersection
	 *                 {{ (Destination', Origin') }})
	 * // Above rewriter guarantees representations of set is {}
	 * // if it is empty, instead of {{ (on I) (Destination, Origin) | UnsatisfiableConstraint }}
	 * 
	 * return Intersection // {{ (on I) (Destination, Origin) | D }}
	 * <pre>
	 */
	private Expression calculateIntersection(Expression prevMessage, Expression msgValue, RewritingProcess process) {		
		// We want to find out if this previous message is covered by
		// {{ (on I) (Destination, Origin, value) | C }}
		// This can be done by computing the intersection below,
		// where we assume that we range over all possible values
		// for the previous message by excluding them:
		// Intersection <- R_intersection(
		//     {{ (on I) (Destination, Origin) | C }}
		Expression msgValueTuple   = IntensionalSet.getHead(msgValue);
		Expression msgValueNoValue = IntensionalSet.makeUniSet(
				IntensionalSet.getScopingExpression(msgValue),
				Tuple.make(
						Tuple.get(msgValueTuple, 0),
						Tuple.get(msgValueTuple, 1)), 
				IntensionalSet.getCondition(msgValue));
		//	   intersection
		//     {{ (Destination', Origin') }})
		Expression destinationPrime = prevMessage.get(0);
		Expression originPrime      = prevMessage.get(1);
		Expression previousMsgTuple = Tuple.make(destinationPrime, originPrime);			
		Expression previousMsgSet   = IntensionalSet.makeUniSetFromIndexExpressionsList(new ArrayList<Expression>(), previousMsgTuple, Expressions.TRUE);
		
		Trace.log("    Intersection <- R_intersection(");
		Trace.log("    {}", msgValueNoValue);
		Trace.log("    intersection");
		Trace.log("    {} }} )", previousMsgSet);
		
		Expression intersection = process.rewrite(R_intersection, LPIUtil.argForIntersectionRewriteCall(msgValueNoValue, previousMsgSet));
		// return Intersection // {{ (on I) (Destination, Origin) | D }}
		return intersection;
	}
	
	private boolean notFinal(Expression beliefValue, Expression priorBeliefValue, List<Expression> msgValues, List<Expression> priorMsgValues, int iteration) {
		boolean notFinal = true;
		
		Trace.log("Belief Value at iteration {} is = {}", iteration, beliefValue);
		
		if (LBPConfiguration.BeliefPropagationUpdateSchedule.isAsynchronous(configuration.getBeliefPropagationUpdateSchedule())) { 
			if (beliefValue.equals(priorBeliefValue)) {
				Trace.log("Final Belief Value: belief value has not changed between iterations, iteration = {}", iteration);
				notFinal = false;
			}
		} 
		else {
			if (msgValues.equals(priorMsgValues)) {
				Trace.log("Final Belief Value: messages value has not changed between iterations, iteration = {}", iteration);
				notFinal = false;
			}	
		}
		if (iteration >= configuration.getMaxNumberOfIterationsForConvergence()) {
			Trace.log("Final Belief Value: max number of iterations reached, iteration = {}", iteration);
			notFinal = false;
		}
		
		return notFinal;
	}
	
	private static boolean isUnion(Expression expression) {
		if (Expressions.hasFunctor(expression, FunctorConstants.UNION)) {
			return true;
		}

		return false;
	}
	
	private static List<Expression> getEntriesFromUnion(Expression union) {
		List<Expression> entries = new ArrayList<Expression>();
		
		if (isUnion(union)) {
			entries.addAll(union.getArguments());
		} 
		else {
			if (!Sets.isEmptySet(union)) {
				entries.add(union);
			}
		}
		
		return entries;
	}
	
	private static Expression createUnionFromArgs(List<Expression> args) {
		Expression result = null;
		
		// First remove all empty sets 
		List<Expression> unionArgs = new ArrayList<Expression>();
		for (Expression arg : args) {
			if (!Sets.isEmptySet(arg)) {
				unionArgs.add(arg);
			}
		}
		
		if (unionArgs.size() == 0) {
			result = _emptySet;
		} 
		else if (unionArgs.size() == 1) {
			result = unionArgs.get(0);
		} 
		else {
			result = Expressions.make(FunctorConstants.UNION, unionArgs);
		}
		
		return result;
	}
	
	private static void assertIsLegalMsgSet(Expression msgSet) {
		boolean    legal = true;
		Expression tuple = null;
		if (!Sets.isUniSet(msgSet)) {
			legal = false;
		} 
		else if (Sets.isExtensionalUniSet(msgSet)) {
			if (!Sets.isSingletonExtensionalSet(msgSet)) {
				legal = false;
			} 
			else {
				tuple = msgSet.get(0);
			}
		} 
		else if (Sets.isIntensionalUniSet(msgSet)) {
			tuple = IntensionalSet.getHead(msgSet);
		}
		
		if (legal) {
			// Ensure is a tuple with two arguments
			if (!Tuple.isTuple(tuple) || tuple.numberOfArguments() != 2) {
				legal = false;
			} 
			else {
				// Ensure these arguments are
				List<Expression> tupleElements = Tuple.getElements(tuple);
				if (!BracketedExpressionSubExpressionsProvider.isBracketedExpression(tupleElements.get(0)) ||
					!BracketedExpressionSubExpressionsProvider.isBracketedExpression(tupleElements.get(1))   ) {
					legal = false;
				}
			}
		}
		
		if (!legal) {
			throw new IllegalArgumentException("msg_set is not an expression of the form { (on I) (Destination, Origin) | C }:"+msgSet);
		}
	}
	
	private Expression limitPrecisionToNumberOfSignificantDecimalPlaces(Expression normalizedValue, RewritingProcess process) {
		Expression result = normalizedValue.replaceAllOccurrences(new ReplacementFunctionWithContextuallyUpdatedProcess() {
					@Override
					public Expression apply(Expression expression) {
						throw new UnsupportedOperationException("evaluate(Object expression) should not be called.");
					}
					
					@Override
					public Expression apply(Expression expressionE, RewritingProcess process) {
						Expression result = expressionE;
						if (expressionE instanceof Symbol) {
							Symbol symbolE = (Symbol) expressionE;
							if (symbolE.getValue() instanceof Number) {
								Rational n = symbolE.rationalValue();
								// Handle the precision of non zero values
								if (!n.isZero()) {
									n = n.multiply(decimalPrecision).round(Rational.ROUND_HALF_UP).divide(decimalPrecision);
									if (n.isZero()) {
										// ensure we don't underflow and introduce zeros
										n = nonZeroMin;
									}
									result = DefaultSymbol.createSymbol(n);
								}
							}
						}
						
						return result;
					}
				}, 
				process);
		
		return result;
	}
	
	private boolean isFreeVariablesIntroduced(Expression msgExpansionOrValue, Set<Expression> freeVariablesFromBeliefQuery, RewritingProcess process) {
		boolean result = false;
		
		Set<Expression> freeInMsg = Variables.freeVariables(msgExpansionOrValue, process);
		freeInMsg.removeAll(Model.getSortNames(process)); // Ensure sort names are not included in this list.
		
		if (!freeVariablesFromBeliefQuery.containsAll(freeInMsg)) {
			result = true;
		}
		
		return result;
	}
	
	private boolean isAnswerDependentOnLogicalVariableMessageIsNot(Expression msgExpansionOrValue, final RewritingProcess process) {
		boolean result = false;
		
		Expression head = IntensionalSet.getHead(msgExpansionOrValue);	
		Set<Expression> destinationVariables = Variables.get(Tuple.get(head, 0), process);
		Set<Expression> originVariables      = Variables.get(Tuple.get(head, 1), process);
		Set<Expression> destOriginSet        = new HashSet<Expression>();
		destOriginSet.addAll(destinationVariables);
		destOriginSet.addAll(originVariables);
		destOriginSet.retainAll(IntensionalSet.getIndices(msgExpansionOrValue));
		
		Set<Expression> rvValueVariables     = new HashSet<Expression>();
		Set<Expression> rvValues             = SubExpressionSelection.get(Tuple.get(head, 2), new Predicate<Expression>() {
			@Override
			public boolean apply(Expression arg) {
				boolean result = LPIUtil.isRandomVariableValueExpressionCandidate(arg, process);
				return result;
			}
		});
		for (Expression rvv : rvValues) {
			rvValueVariables.addAll(Variables.get(rvv, process));
		}
		// Only keep random variable value logical variables that are not internally
		// scoped (e.g. in a product message from an expansion).
		rvValueVariables.retainAll(Variables.freeVariables(Tuple.get(head, 2), process));
		
		Set<Expression> testSet = new HashSet<Expression>();
		testSet.addAll(Variables.freeVariables(Tuple.get(head, 2), process));
		testSet.retainAll(IntensionalSet.getIndices(msgExpansionOrValue));
		testSet.removeAll(destOriginSet);
		testSet.removeAll(rvValueVariables);
				
		if (testSet.size() > 0) {
			System.err.println("expansion or value  ="+msgExpansionOrValue);
			System.err.println("tesSet              ="+testSet);
			System.err.println("destOriginSet       ="+destOriginSet);
			System.err.println("destinationVariables="+destinationVariables);
			System.err.println("originVariables     ="+originVariables);
			System.err.println("rvValueVariables    ="+rvValueVariables);
			result = true;
		}
		
		return result;
	}
	
	private class PreviousMessageReplacementFunction extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		
		private List<Expression> msgValues;
		private PreviousMessageToMsgValueCache previousMessageToMsgValueCache;
		
		public PreviousMessageReplacementFunction(List<Expression> msgValues, PreviousMessageToMsgValueCache previousMessageToMsgValueCache) {
			this.msgValues                      = msgValues;
			this.previousMessageToMsgValueCache = previousMessageToMsgValueCache;
		}
		
		//
		// START-ReplacementFunctionWithContextuallyUpdatedProcess
		@Override
		public Expression apply(Expression expressionE, RewritingProcess process) {
			Expression result = null;
			
			if ( ! LPIUtil.isPreviousMessage(expressionE)) {
				// if E is not of the form "previous message to Destination' from Origin'
				//     return E
				result = expressionE;
			} 
			else {
				Trace.log("    value <- find_msg_value_matching_previous_message({}, msg_values)", expressionE);
				Expression value = findMsgValueMatchingPreviousMessage(expressionE, msgValues, previousMessageToMsgValueCache, process);
				
				Trace.log("    return value");
				Trace.log("    // (replacing {} by {})", expressionE, value);
				result = value;
			}
			
			return result;
		}
		// END-ReplacementFunctionWithContextuallyUpdatedProcess
		//
	}
	
	private class PreviousMessageToMsgValueCache {
		ConcurrentHashMap<Pair<Expression, Expression>, Integer>                    previousMessageToMsgValueIndex              = new ConcurrentHashMap<Pair<Expression, Expression>, Integer>();
		ConcurrentHashMap<Pair<Expression, Expression>, CachedMsgValueIntersection> previousMessageToCachedMsgValueIntersection = new ConcurrentHashMap<Pair<Expression, Expression>, CachedMsgValueIntersection>();
		boolean useCache = true;
		
		public PreviousMessageToMsgValueCache() {
			useCache = configuration.isBeliefUseCache();
		}
		
		public void addPreviousMessageToMsgValueIndex(Expression contextualConstraint, Expression previousMessage, int msgValueIndex) {
			if (useCache) {
				previousMessageToMsgValueIndex.put(new Pair<Expression, Expression>(contextualConstraint, previousMessage), msgValueIndex);
			}
		}
		
		public Integer getCachedMessageValueIndex(RewritingProcess process, Expression previousMessage) {
			return previousMessageToMsgValueIndex.get(new Pair<Expression, Expression>(process.getContextualConstraint(), previousMessage));
		}
		
		public void addCachedMsgValueIntersection(RewritingProcess process, Expression previousMessage, CachedMsgValueIntersection cachedMsgValueIntersection) {
			if (useCache) {
				previousMessageToCachedMsgValueIntersection.put(new Pair<Expression, Expression>(process.getContextualConstraint(), previousMessage), 
						cachedMsgValueIntersection);
			}
		}
		
		public CachedMsgValueIntersection getCachedMsgValueIntersection(RewritingProcess process, Expression previousMessage) {
			return previousMessageToCachedMsgValueIntersection.get(new Pair<Expression, Expression>(process.getContextualConstraint(), previousMessage));
		}
	}
	
	private class CachedMsgValueIntersection {
		public Expression intersection;
		public Expression scopingExpressionFromMsgValue;
		public Expression conditionFromMsgValue;
		
		public CachedMsgValueIntersection(Expression intersection,
				Expression scopingExpressionFromMsgValue, Expression conditionFromMsgValue) {
			this.intersection                      = intersection;
			this.scopingExpressionFromMsgValue     = scopingExpressionFromMsgValue;
			this.conditionFromMsgValue             = conditionFromMsgValue;
		}
	}
}
