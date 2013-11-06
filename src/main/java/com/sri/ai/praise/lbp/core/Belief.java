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
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
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
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.CheapDisequalityModule;
import com.sri.ai.grinder.library.lambda.Lambda;
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
		List<Expression> msgExpansionsUnionArguments = getMessageExpansions(msgSetUnionArguments, new PreviousMessageToMsgValueCache(), process);
		
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
		
		Set<Expression> freeVariablesFromBeliefQuery = Expressions.freeVariables(randomVariable, process);
		
		process = LPIUtil.extendContextualVariablesInferringDomainsFromUsageInRandomVariables(randomVariable, process);
		process = LPIUtil.extendContextualVariablesInferringDomainsFromUsageInRandomVariables(Tuple.make(Model.getRewritingProcessesModel(process).getParfactorsDeclaration().getParfactors()), process);
		
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
		Trace.log("// belief_expansion = {}", beliefExpansion);
		Trace.log("belief_expansion <- R_complete_normalize(belief_expansion)");
		beliefExpansion = process.rewrite(R_complete_normalize, beliefExpansion);
		Trace.log("// belief_expansion = {}", beliefExpansion);
		if (Justification.isEnabled()) {
			Justification.endEqualityStep(Expressions.apply(LPIUtil.FUNCTOR_NORMALIZE, beliefExpansion));
		}
	
		Expression result = null;
		if (!LPIUtil.containsPreviousMessageExpressions(beliefExpansion)) {
			Justification.beginEqualityStep("normalize and return belief since it does not depend on previous iteration messages");
			Trace.log("if belief_expansion does not contain 'previous message' expressions");
			Trace.log("    return R_normalize_message(V, belief_expansion)");
			
			Justification.beginEqualityStep("normalization");
			Expression normalizedBeliefExpansion = process.rewrite(R_normalize_message, LPIUtil.argForNormalizeRewriteCall(randomVariable, beliefExpansion));
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
//			List<Expression> msgSets = getEntriesFromUnion(process.rewrite(R_extract_previous_msg_sets, beliefExpansion));		
			List<Expression> msgSets = getEntriesFromUnion(process.rewrite(R_extract_previous_msg_sets, Lambda.make(new LinkedList<Expression>(freeVariablesFromBeliefQuery), beliefExpansion)));		
			Trace.log("// msg_sets = {}", Expressions.make("list", msgSets));
		
			Trace.log("msg_expansions <- get_msg_expansions(msg_sets)");
			// a union of intensional sets containing tuples of the form:
			// (Destination, Origin, Expansion), where Expansion is a basic expression
			// possibly containing "previous message" expressions, representing their 
			// value in terms of messages from the previous loopy BP iteration.
			PreviousMessageToMsgValueCache previousMessageToMsgValueCache = new PreviousMessageToMsgValueCache(); 
			List<Expression> msgExpansions = getMessageExpansions(msgSets, previousMessageToMsgValueCache, process);
			Trace.log("// msg_expansions = {}", Expressions.make("list", msgExpansions));
			
			Trace.log("msg_values <- copy of msg_expansions, with the values replaced by uniform messages");
			// a union of sets of tuples (N1, N2, value) where (N1,N2) represents a message and value
			// is a basic expression representing its value at the current loopy BP iteration
			// (this expression is therefore free of previous message expressions).
			// { (on I) (Destination, Origin, 1) | C } union ...
			List<Expression> msgValues = new ArrayList<Expression>();
			for (Expression msgExpansion : msgExpansions) {
				// i.e. (Destination, Origin, Expansion)
				Expression tupleExpansion = IntensionalSet.getHead(msgExpansion);
				// (Destination, Origin, 1)
				Expression tuple          = Tuple.make(Tuple.get(tupleExpansion, 0),
													   Tuple.get(tupleExpansion, 1),
													   Expressions.ONE);
				// { (on I) (Destination, Origin, 1) | C }
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
			Trace.log("// initial belief_value = {}", beliefValue);
			while (notFinal(beliefValue, priorBeliefValue, msgValues, priorMsgValues, iteration)) {
				priorBeliefValue = beliefValue;
				priorMsgValues   = msgValues;
				Justification.beginEqualityStep("iteration");
				LiftProductOfFactorToVariable.setMustAlwaysLift(true, process);
				msgValues        = iterateValuesUsingExpansions(msgValues, msgExpansions, previousMessageToMsgValueCache, process);
				beliefValue      = useValuesForPreviousMessages(beliefExpansion, msgValues, previousMessageToMsgValueCache, process);
				LiftProductOfFactorToVariable.setMustAlwaysLift(false, process);
				iteration++;			
				notifyCollector(randomVariable, beliefValue, iteration, process);
				Justification.endEqualityStep(beliefValue);
				Trace.log("    // msg_values = {}", Expressions.make("list", msgValues));
				Trace.log("    // belief_value = {}", beliefValue);
			}
			
			Trace.log("return R_normalize_message(V, belief_value)");
			
			Justification.log("Normalization of final unnormalized belief value {}", beliefValue);
			Justification.beginEqualityStep("normalization");
			Expression normalizedBeliefValue = process.rewrite(R_normalize_message, LPIUtil.argForNormalizeRewriteCall(randomVariable, beliefValue));
			Trace.log("// belief_value = {}", beliefValue);
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
				Expression normalizedBeliefValue = process.rewrite(R_normalize_message, LPIUtil.argForNormalizeRewriteCall(randomVariable, beliefValue));
				beliefValueAtIterationCollector.setBeliefValueAtIteration(iteration, normalizedBeliefValue);
			}
		}
	}
	
	private List<Expression> getMessageExpansions(List<Expression> messageSets, PreviousMessageToMsgValueCache previousMessageToMsgValueCache, RewritingProcess process) {		
		Trace.in("+get_msg_expansions({})", Tuple.make(messageSets));
		
		Trace.log("msg_expansions         <- { }");
		Trace.log("msgs_alreaded_expanded <- { }");
		Trace.log("msgs_to_be_expanded    <- message_sets");
		List<Expression> messageExpansions                     = new ArrayList<Expression>();
		List<Expression> messagesAlreadyExpandedUnionArguments = new ArrayList<Expression>();
		List<Expression> messagesToBeExpanded                  = new ArrayList<Expression>(messageSets);
		
		Justification.begin("Going to expand messages in {}", messageSets);
		
		Trace.log("while msgs_to_be_expanded is not empty");
		while (messagesToBeExpanded.size() > 0) {
			Trace.log("    message_set <- remove a union argument from msgs_to_be_expanded");
			Expression messageSet = messagesToBeExpanded.remove(0);
			Trace.log("    // message_set = {}", messageSet);
			// Ensure is a legal message_set expression
			assertIsLegalMsgSet(messageSet);
	
			Justification.begin("Going to expand message set {}", messageSet);
			Trace.in("Going to expand message set {}", messageSet);

			messageSet = removeAlreadyExpandedMessages(messageSet, messagesAlreadyExpandedUnionArguments, process);
			
			if (!Sets.isEmptySet(messageSet)) {
				Trace.log("    if message_set is not empty");
				expand(messageSet, messagesAlreadyExpandedUnionArguments, messagesToBeExpanded, messageExpansions, previousMessageToMsgValueCache, process);
				Justification.end("Message set expanded. See inside for details.");
				Trace.out("Message set expanded. See inside for details.");
			}
			else {
				Justification.end("All messages in this set have already been expanded, so there is nothing to do.");
				Trace.out("All messages in this set have already been expanded, so there is nothing to do.");
			}
		}
		
		Trace.log("return msg_expansions");
		
		Trace.out("-get_msg_expansions={}", Expressions.apply("list", messageExpansions));
		
		Justification.end("Messages expansions are {}", messageExpansions);

		return messageExpansions;
	}

	private Expression removeAlreadyExpandedMessages(Expression messageSet, List<Expression> messagesAlreadyExpandedUnionArguments, RewritingProcess process) {
		Trace.in("Going to remove messages from message_set that were already expanded");
		Justification.begin("Going to subtract message instantiations that were already expanded");
		
		// Note: Minor Optimization, up front limit msgs_already_expanded to only those 
		// that can possibly intersect.
		List<Expression> possibleIntersectionUnionArgs = new ArrayList<Expression>();
		Expression alpha = IntensionalSet.getHead(messageSet);
		for (Expression messagesAlreadyExpandedUnionArgument : messagesAlreadyExpandedUnionArguments) {				
			Expression alphaPrime = IntensionalSet.getHead(messagesAlreadyExpandedUnionArgument); 
			// Perform a cheap disequality first
			if (!CheapDisequalityModule.isACheapDisequality(alpha, alphaPrime, process)) {
				possibleIntersectionUnionArgs.add(messagesAlreadyExpandedUnionArgument);
			}
		}
		Expression msgsAlreadyExpanded = createUnionFromArgs(possibleIntersectionUnionArgs);
		
		Trace.log("    message_set <- R_set_diff(message_set minus msgs_already_expanded)");
		messageSet = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(messageSet, msgsAlreadyExpanded));
		Trace.log("    message_set <- R_complete_normalize(message_set)");
		messageSet = process.rewrite(R_complete_normalize, messageSet);
		Justification.end("Remaining, non-expanded messages are {}", messageSet);
		Trace.out("Removed them, and remaining message_set is {}", messageSet);
		return messageSet;
	}

	private void expand(Expression messageSet, List<Expression> messagesAlreadyExpandedUnionArguments, List<Expression> messagesToBeExpanded, List<Expression> messageExpansions, PreviousMessageToMsgValueCache previousMessageToMsgValueCache, RewritingProcess process) {
		Expression expressionI            = IntensionalSet.getScopingExpression(messageSet);
		Expression destination            = Tuple.get(IntensionalSet.getHead(messageSet), 0);
		Expression origin                 = Tuple.get(IntensionalSet.getHead(messageSet), 1);
		Expression conditionC             = IntensionalSet.getCondition(messageSet);
		
		Expression expansion = computeExpansionSymbolically(messageSet, expressionI, destination, origin, conditionC, process);
		
		Trace.log("        msg_expansion <- { (on I) (Destination, Origin, expansion) | C }");
		Expression msgExpansion = IntensionalSet.makeUniSet(expressionI, Tuple.make(destination, origin, expansion), conditionC);
		
		sanityChecksOnLogicalVariables(messageSet, messageExpansions, process, msgExpansion);
		
		storeMessageExpansion(msgExpansion, destination, origin, conditionC, messageExpansions, previousMessageToMsgValueCache);
		
		updateSetOfMessagesAlreadyExpanded(messagesAlreadyExpandedUnionArguments, expressionI, destination, origin, conditionC);
		
		identifyMessagesUsedByMessageExpansionAndScheduleThemForExpansionThemselves(msgExpansion, messagesToBeExpanded, process);
	}

	private Expression computeExpansionSymbolically(Expression messageSet, Expression expressionI, Expression destination, Expression origin, Expression conditionC, RewritingProcess process) {
		Justification.begin("Going to re-arrange as message expression");
		// represent message_set as { (on I) (Destination, Origin) | C }

		Trace.log("        expansion <- compute message to Destination from Origin with");
		Trace.log("                     beingComputed = empty set and contextual constraint C");
		Expression       msgTo_From    = Expressions.make(LPIUtil.FUNCTOR_MSG_TO_FROM, destination, origin);
		Justification.end("Message expression is {}", msgTo_From);

		
		Justification.begin("Going to symbolically compute message expression");
		Expression expansion = null;
		Justification.begin("Going to symbolically compute unsimplified message expression");
		// Set up the contextual constraint
		RewritingProcess processExtendedByC = LPIUtil.extendContextualVariablesAndConstraintWithIntensionalSetInferringDomainsFromUsageInRandomVariables(messageSet, process);
		// Determine which 'message to . from .' rewriter to call
		if (BracketedExpressionSubExpressionsProvider.isRandomVariable(destination, process)) {
			expansion = processExtendedByC.rewrite(R_m_to_v_from_f,
							LPIUtil.argForMessageToVariableFromFactorRewriteCall(msgTo_From, LPIUtil.createNewBeingComputedExpression()));
		} 
		else {
			expansion = processExtendedByC.rewrite(R_m_to_f_from_v,
							LPIUtil.argForMessageToFactorFromVariableRewriteCall(msgTo_From, LPIUtil.createNewBeingComputedExpression()));
		}
		Justification.end("Obtained unsimplified message expansion {}", expansion);
		
		Justification.begin("Going to simplify {}", expansion);
		Trace.log("        expansion <- R_complete_normalize(expansion)");
		// Note: use cSubProcess for R_complete_normalize call as this is the context
		// under which the expansion was generated and we want to remove any
		// invalid branches based on this.
		expansion = processExtendedByC.rewrite(R_complete_normalize, expansion);
		Trace.log("        // expansion = {}", expansion);
		Justification.end("Obtained simplified message expansion {}", expansion);

		Justification.end("Obtained message expansion {}", expansion);
		return expansion;
	}

	private void sanityChecksOnLogicalVariables(Expression messageSet, List<Expression> messageExpansions, RewritingProcess process, Expression msgExpansion) {
		Collection<Expression> introducedFreeVariables = getIntroducedFreeVariables(msgExpansion, process);
		if ( ! introducedFreeVariables.isEmpty()) {
			System.err.println("IllegalStateException: introduced additional free variables into msg_expansion.");
			System.err.println("introduced     = " + Util.join(introducedFreeVariables));
			System.err.println("msg_expansion  = " + msgExpansion);
			System.err.println("message_set    = " + messageSet);
			System.err.println("msg_expansions =");
			for (Expression me : messageExpansions) {
				System.err.println(me);
			}
			throw new IllegalStateException("IllegalStateException: introduced additional free variables " + Util.join(introducedFreeVariables) + " into msg_expansion: "+msgExpansion);
		}
		if (expansionDependensOnLogicalVariableButMessageDoesNot(msgExpansion, process)) {
			System.err.println("IllegalStateException: expansion depends on logical variable but message does not.");
			System.err.println("msg_expansion  = " + msgExpansion);
			System.err.println("message_set    = " + messageSet);
			System.err.println("msg_expansions = ");
			for (Expression me : messageExpansions) {
				System.err.println(me);
			}
			throw new IllegalStateException("IllegalStateException: msg_expansion has answer dependent on logical variable that value is not: "+msgExpansion);
		}
	}

	private void storeMessageExpansion(Expression msgExpansion, Expression destination, Expression origin, Expression conditionC, List<Expression> messageExpansions, PreviousMessageToMsgValueCache previousMessageToMsgValueCache) {
		Trace.log("        index <- size of msg_expansions");
		Trace.log("        cache index as msg_value_index corresponding to 'previous message to Destination from Origin' under constraining condition C");
		previousMessageToMsgValueCache.addPreviousMessageToMsgValueIndex(
				conditionC,
				Expressions.make(LPIUtil.FUNCTOR_PREVIOUS_MSG_TO_FROM, destination, origin),
				messageExpansions.size());
		Trace.log("        msg_expansions <- msg_expansions union msg_expansion");
		messageExpansions.add(msgExpansion);
	}

	private void updateSetOfMessagesAlreadyExpanded(List<Expression> messagesAlreadyExpandedUnionArguments, Expression expressionI, Expression destination, Expression origin, Expression conditionC) {
		Trace.log("        msgs_already_expanded <- msgs_already_expanded union { (on I) (Destination, Origin) | C }");
		Expression alreadyExpanded = IntensionalSet.makeUniSet(expressionI, Tuple.make(destination, origin), conditionC);
		messagesAlreadyExpandedUnionArguments.add(alreadyExpanded);
		Trace.log("        // msgs_already_expanded = {}", Expressions.apply(FunctorConstants.UNION, messagesAlreadyExpandedUnionArguments));
	}

	private void identifyMessagesUsedByMessageExpansionAndScheduleThemForExpansionThemselves(Expression msgExpansion, List<Expression> messagesToBeExpanded, RewritingProcess process) {
		Trace.log("        msgs_to_be_expanded <- msgs_to_be_expanded union R_extract_previous_msg_sets(msg_expansion)");
		Justification.begin("Going to identify messages inside expansion and schedule them for their own expansion");
		Expression newMsgSetsToBeExpanded = process.rewrite(R_extract_previous_msg_sets, msgExpansion);
		messagesToBeExpanded.addAll(getEntriesFromUnion(newMsgSetsToBeExpanded));
		Justification.end("Messages inside expansion identified: {}", newMsgSetsToBeExpanded);
		Trace.log("        // msgs_to_be_expanded = {}", Tuple.make(messagesToBeExpanded));
	}

	private List<Expression> iterateValuesUsingExpansions(final List<Expression> msgValues,
			final List<Expression> msgExpansions, final PreviousMessageToMsgValueCache previousMessageToMsgValueCache, RewritingProcess process) {
		
		Trace.in("+iterate_values_using_expansions({}, {})", msgValues, msgExpansions);
		
		Trace.log("next_msg_values <- empty set ");
		List<Expression> nextMsgValuesUnionArguments = new ArrayList<Expression>();
		Trace.log("for each union argument { (on I) (Destination, Origin, Expansion) | C } in msg_expansions");
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
					
					RewritingProcess subProcess = LPIUtil.extendContextualVariablesAndConstraintWithIntensionalSetInferringDomainsFromUsageInRandomVariables(msgExpansion, process);
					
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
					if (Model.isAllDomainSizesKnown(subProcess)) {
						Expression randomVariable = null;
						if (BracketedExpressionSubExpressionsProvider.isRandomVariable(destination, subProcess)) {
							randomVariable = destination;
						} 
						else {
							randomVariable = origin;
						}
						normalizedValue = subProcess.rewrite(R_normalize_message, LPIUtil.argForNormalizeRewriteCall(randomVariable, value));
						normalizedValue = limitPrecisionToNumberOfSignificantDecimalPlaces(normalizedValue, subProcess);
					}
					
					Trace.log("    next_msg_values <- next_msg_values union { (on I) (Destination, Origin, value) | C }");
					Expression tupleTriple = Tuple.make(destination, origin, normalizedValue);
					Expression newMsgValue = IntensionalSet
												.makeUniSetFromIndexExpressionsList(
														IntensionalSet.getIndexExpressions(msgExpansion), 
														tupleTriple, 
														IntensionalSet.getCondition(msgExpansion));
					
					Collection<Expression> introducedFreeVariables = getIntroducedFreeVariables(newMsgValue, process);
					if ( ! introducedFreeVariables.isEmpty()) {
						System.err.println("IllegalStateException: introduced additional free variables into new_msg_value.");
						System.err.println("sub.context      ="+subProcess.getContextualConstraint());
						System.err.println("sub.context vars ="+subProcess.getContextualVariables());
						System.err.println("introduced       ="+Util.join(introducedFreeVariables));
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
						throw new IllegalStateException("IllegalStateException: introduced additional free variables " + Util.join(introducedFreeVariables) + " into new_msg_value: "+newMsgValue);
					}
					
					if (expansionDependensOnLogicalVariableButMessageDoesNot(newMsgValue, process)) {
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

		Trace.log("exhaustively replace all expressions in substituted with the following replacement function:");

		// Ensure we exhaustively replace
		Expression toSubstitute;
		do {
			toSubstitute = substituted;	
			substituted  = toSubstitute.replaceAllOccurrences(
										new PreviousMessageReplacementFunction(msgValues, previousMessageToMsgValueCache), 
										process);
		} while (substituted != toSubstitute);
			
		Trace.log("// substituted = {}, constrained by {}", substituted, process.getContextualConstraint());
		Trace.log("return R_complete_normalize(substituted)");
		result = process.rewrite(R_complete_normalize, substituted);
		
		Trace.out("-use_values_for_previous_msgs={}", result);
		
		return result;
	}
	
	/**
	 * <pre>
     * find_msg_value_matching_previous_message(prev_message, msg_values)
     *  inputs: prev_message is of the form: 
     *         previous message to Destination' from Origin'
     *         msg_values, with entris of the form: 
     *         { (on I) (Destination, Origin, value) | C }
     * output: intuitively, we interpret each entry in msg_values as storing the
     *         parameterized value of a message from Origin to Destination, valid for
     *         instantiations satisfying C, and return the value of prev_message
     *         according to these values (provided by the entries unifying with it; there
     *         may be several).
     *         Formally, if L are the free logical variables in prev_message,
     *         then the function returns a (conditional) value
     *             if <condition on L>
     *                then <value defined by msg_values under the condition on L>
     *                else <find_msg_value_matching_previous_message(
     *                           prev_message, msg_values)
     *                           under the complement of the condition>
     * 
     * Intersection <- null
     * msg_value    <- null
     * if previous_message index in msg_values is cached
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
     * // msg_value is represented as { (on I) (Destination, Origin, value) | C }
     * // Intersection is represented as { (on I) (Destination', Origin') | C' }
     * pickFrom = { (on I) value | C' }
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
     * // Partition the value based on its intersection:
     * // Intersection is of the form { (on I) (Destination, Origin) | C' }
     * D <- there exists I : C'
     * return R_normalize(if D then v else prev_message)
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
		Integer    cachedIndex  = previousMessageToMsgValueCache.getCachedMessageValueIndex(process, prevMessage);
		// if previous_message msg_value_index cached
		if (cachedIndex != null) {
			// index <- index from cache
			int index = cachedIndex;
			Trace.log("Set of msg values set cached for {} under contextual constraint {}: {}", prevMessage, process.getContextualConstraint(), msgValues.get(index));
			// msg_value <- standardize msg_values[index] apart from prev_message
			msgValue = StandardizedApartFrom.standardizedApartFrom(msgValues.get(index), prevMessage, process);
			Trace.log("When standardized apart, it is {}", msgValue);
			// Note: msg_value is passed through to the cache so that we can guarantee that the current msg_value
			// and the cached intersection are scoped by the same indices.
			intersection = previousMessageToMsgValueCache.getCachedMsgValueIntersection(process, prevMessage, msgValue);
			// if Intersection cached for previous_message
			if (intersection != null) {
				// Intersection <- cache
				Trace.log("Cached intersection for {}: {}", prevMessage, intersection);
			} 
			else {
				Trace.log("No intersection cached for {} and {}", prevMessage, msgValue);
				// Intersection <- calculate_intersection(prev_message, msg_value)
				intersection = calculateIntersection(prevMessage, msgValue, process);
				Trace.log("Intersection calculated as {}", intersection);
				// cache Intersection under prev_message’s contextual constraint
				previousMessageToMsgValueCache.addCachedMsgValueIntersection(process, prevMessage, intersection);
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
					previousMessageToMsgValueCache.addCachedMsgValueIntersection(process, prevMessage, intersection);
					Trace.log("Adding cache item for {}: intersection {}, scoping expression of msg value {}, condition {}",
							prevMessage, intersection, IntensionalSet.getScopingExpression(msgValue), IntensionalSet.getCondition(msgValue));
					// exit "for each" loop
					break;
				}
			}
		}
		
		if (intersection == null) {
			System.err.println("IllegalStateException: unable to find intersection.");
			System.err.println("context      ="+process.getContextualConstraint());
			System.err.println("prev_message ="+prevMessage);
			System.err.println("msg_values   =");
			for (Expression m : msgValues) {
				System.err.println(""+m);
			}
			throw new IllegalStateException("Unable to find intersection: "+prevMessage+" under: "+process.getContextualConstraint());
		}
		
		// msg_value is represented as { (on I) (Destination, Origin, value) | C }
		// Intersection is represented as { (on I) (Destination', Origin') | C' }
		// pickFrom = { (on I) value | C' }
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
			throw new IllegalStateException("Failed to pick single element: "+pickFrom + " under: "+process.getContextualConstraint());
		}
		
		// Partition the value based on its intersection:
		// Intersection is of the form { (on I) (Destination, Origin) | C' }
		Trace.log("D <- there exists I : C'");
		Expression expressionD = ThereExists.make(new ArrayList<Expression>(IntensionalSet.getIndices(intersection)), 
												  IntensionalSet.getCondition(intersection));
		Trace.log("return R_normalize(if D then v else prev_message)"); 
		Expression conditionalD = IfThenElse.make(expressionD, v, prevMessage);
		result = process.rewrite(R_normalize, conditionalD);

		Trace.out("-findMsgValueMatchingPreviousMessage={}", result);
		
		return result;
	}
	
	/**
	 * <pre>
	 * calculate_intersection(prev_message, msg_value)
 	 * inputs: prev_message, which is of the form: 
 	 *         previous message to Destination' from Origin'
 	 *         msg_value is of the form: 
 	 *         { (on I) (Destination, Origin, value) | C }
  	 * output: the intersection of { (Destination', Origin') } and { (on I) (Destination, Origin) | C },
 	 *         which is going to be either an intensional set expression involving the variables
 	 *         in Destination' and Origin' as free variables, or the empty set {}.
 	 *         The output is guaranteed to be {} if the set is indeed empty,
 	 *         that is, it will not be an intensional set expression with an unsatisfiable condition.
	 * 
	 * // We want to find out if this previous message is covered by
	 * // { (on I) (Destination, Origin, value) | C }
	 * // This can be done by computing the intersection below,
	 * // where we range over all possible values
	 * // for the previous message by excluding them:
	 * Intersection <- R_intersection(
	 *                 { (on I) (Destination, Origin) | C }
	 *                 intersection
	 *                 { (Destination', Origin') })
	 * // Above rewriter guarantees representations of set is {}
	 * // if it is empty, instead of { (on I) (Destination, Origin) | UnsatisfiableConstraint }
	 * 
	 * return Intersection
	 * <pre>
	 */
	private Expression calculateIntersection(Expression prevMessage, Expression msgValue, RewritingProcess process) {		
		// We want to find out if this previous message is covered by
		// { (on I) (Destination, Origin, value) | C }
		// This can be done by computing the intersection below,
		// where we assume that we range over all possible values
		// for the previous message by excluding them:
		// Intersection <- R_intersection(
		//     { (on I) (Destination, Origin) | C }
		Expression msgValueTuple   = IntensionalSet.getHead(msgValue);
		Expression msgValueNoValue = IntensionalSet.makeUniSet(
				IntensionalSet.getScopingExpression(msgValue),
				Tuple.make(
						Tuple.get(msgValueTuple, 0),
						Tuple.get(msgValueTuple, 1)), 
				IntensionalSet.getCondition(msgValue));
		//	   intersection
		//     { (Destination', Origin') })
		Expression destinationPrime = prevMessage.get(0);
		Expression originPrime      = prevMessage.get(1);
		Expression previousMsgTuple = Tuple.make(destinationPrime, originPrime);			
		Expression previousMsgSet   = IntensionalSet.makeUniSetFromIndexExpressionsList(new ArrayList<Expression>(), previousMsgTuple, Expressions.TRUE);
		
		Trace.log("    Intersection <- R_intersection(");
		Trace.log("    {}", msgValueNoValue);
		Trace.log("    intersection");
		Trace.log("    {} }} )", previousMsgSet);
		
		Expression intersection = process.rewrite(R_intersection, LPIUtil.argForIntersectionRewriteCall(msgValueNoValue, previousMsgSet));
		// return Intersection
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
			throw new IllegalArgumentException("message_set is not an expression of the form { (on I) (Destination, Origin) | C }:"+msgSet);
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
	
	private Collection<Expression> getIntroducedFreeVariables(Expression msgExpansionOrValue, RewritingProcess process) {
		Set<Expression> freeInMsg = Expressions.freeVariables(msgExpansionOrValue, process);
		freeInMsg.removeAll(Model.getSortNames(process)); // Ensure sort names are not included in this list.
		
		Collection<Expression> introducedFreeVariables = Util.setDifference(freeInMsg, process.getContextualVariables());
		
		return introducedFreeVariables;
	}
	
	private boolean expansionDependensOnLogicalVariableButMessageDoesNot(Expression msgExpansionOrValue, final RewritingProcess process) {
		boolean result = false;
		
		Expression head = IntensionalSet.getHead(msgExpansionOrValue);	
		Set<Expression> destinationVariables = Expressions.getVariables(Tuple.get(head, 0), process);
		Set<Expression> originVariables      = Expressions.getVariables(Tuple.get(head, 1), process);
		Set<Expression> destOriginSet        = new HashSet<Expression>();
		destOriginSet.addAll(destinationVariables);
		destOriginSet.addAll(originVariables);
		destOriginSet.retainAll(IntensionalSet.getIndices(msgExpansionOrValue));
		
		Set<Expression> rvValueVariables     = new HashSet<Expression>();
		Set<Expression> rvValues             = SubExpressionSelection.getVariables(Tuple.get(head, 2), new Predicate<Expression>() {
			@Override
			public boolean apply(Expression arg) {
				boolean result = LPIUtil.isRandomVariableValueExpression(arg, process);
				return result;
			}
		});
		for (Expression rvv : rvValues) {
			rvValueVariables.addAll(Expressions.getVariables(rvv, process));
		}
		// Only keep random variable value logical variables that are not internally
		// scoped (e.g. in a product message from an expansion).
		rvValueVariables.retainAll(Expressions.freeVariables(Tuple.get(head, 2), process));
		
		Set<Expression> testSet = new HashSet<Expression>();
		testSet.addAll(Expressions.freeVariables(Tuple.get(head, 2), process));
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
		// key= (context, prev_message), value = msg_value_index to use.
		ConcurrentHashMap<Expression, Integer>    previousMessageToMsgValueIndex              = new ConcurrentHashMap<Expression, Integer>();
		// key= (context, prev_message, scoping expression for intersection), value=intersection
		ConcurrentHashMap<Expression, Expression> previousMessageToCachedMsgValueIntersection = new ConcurrentHashMap<Expression, Expression>();
		boolean useCache = true;
		
		public PreviousMessageToMsgValueCache() {
			useCache = configuration.isBeliefUseCache();
		}
		
		/**
		 * Add a prev_message -> msg_value_index to the cache.
		 * 
		 * @param contextualConstraint
		 *            the context prev_message is under.
		 * @param previousMessage
		 *            the prev_message to be mapped to a msg_value index.
		 * @param msgValueIndex
		 *            the index of the msg_value the prev_message under the
		 *            given context is represented by.
		 */
		public void addPreviousMessageToMsgValueIndex(Expression contextualConstraint, Expression previousMessage, int msgValueIndex) {
			if (useCache) {
				previousMessageToMsgValueIndex.put(Tuple.make(contextualConstraint, previousMessage), msgValueIndex);
			}
		}
		
		/**
		 * Get the msg_value_index associated with the given prev_message under
		 * the current context.
		 * 
		 * @param process
		 *            the rewriting process containing the current context for
		 *            prev_message.
		 * @param previousMessage
		 *            the prev_messages whose msg_value_index is to be looked up
		 *            in the cache.
		 * @return the msg_value_index associated with the prev_message under
		 *         the given context if in the cache, null otherwise.
		 */
		public Integer getCachedMessageValueIndex(RewritingProcess process, Expression previousMessage) {
			return previousMessageToMsgValueIndex.get(Tuple.make(process.getContextualConstraint(), previousMessage));
		}
		
		/**
		 * Will store an entry in the cache where intersection is represented
		 * as: { (on I) (Destination', Origin') | C' }<br>
		 * 
		 * <pre>
		 * key   = (context, prev_message, (on I)), 
		 * value = intersection
		 * </pre>
		 * 
		 * @param process
		 *            the rewriting process containing the current context for
		 *            prev_message.
		 * @param previousMessage
		 *            the prev_messages whose intersection with a msg_value is
		 *            to be added to the cache.
		 * @param intersection
		 *            the intersection to be added.
		 */
		public void addCachedMsgValueIntersection(RewritingProcess process, Expression previousMessage, Expression intersection) {
			if (useCache) {
				previousMessageToCachedMsgValueIntersection.put(Tuple.make(process.getContextualConstraint(), previousMessage, IntensionalSet.getScopingExpression(intersection)), 
						intersection);
			}
		}
		
		/**
		 * Get the cached intersection for a prev_message and msg_value under
		 * the current context for prev_message.
		 * 
		 * @param process
		 *            the rewriting process containing the current context for
		 *            prev_message.
		 * @param previousMessage
		 *            the prev_messages whose intersection with a msg_value is
		 *            to be looked up in the cache.
		 * @param msgValue
		 *            the msg_value that prev_message is meant to intersect
		 *            with. Will use the scoping expression from the msg_value
		 *            to ensure the msg_value and cached intersection are always
		 *            scoped by the same indices.
		 * @return the cached intersection if one exists, false otherwise.
		 */
		public Expression getCachedMsgValueIntersection(RewritingProcess process, Expression previousMessage, Expression msgValue) {
			return previousMessageToCachedMsgValueIntersection.get(Tuple.make(process.getContextualConstraint(), previousMessage, IntensionalSet.getScopingExpression(msgValue)));
		}
	}
}
