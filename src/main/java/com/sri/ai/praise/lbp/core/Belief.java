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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.core.DefaultIntensionalUniSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp;
import com.sri.ai.grinder.helper.SymbolicInjectiveLookUp;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.BranchRewriteTask;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.CheapDisequalityModule;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
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
	private static final String SYMBOLIC_MAP_FOR_MESSAGE_VALUES_SETS_GLOBAL_OBJECTS_KEY = "Symbolic map for message values sets";
	//
	private LBPConfiguration configuration = null;
	private Rational decimalPrecision = null;
	private Rational nonZeroMin = null;
	
	public Belief() {
		this(new DefaultLBPConfiguration());
	}

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
	@Override
	public Expression getMessageExpansions(Expression msgSets, RewritingProcess process) {		
		Expression result = null;
	
		List<Expression> msgSetUnionArguments        = Union.getEntriesFromUnionOrSet(msgSets);
		List<Expression> msgExpansionsUnionArguments = getMessageExpansions(msgSetUnionArguments, process);
		
		result = Union.make(msgExpansionsUnionArguments);
		
		return result;
	}
	
	/**
	 * 
	 * @see IterateValuesUsingExpansions#iterateValuesUsingExpansions(Expression, Expression, RewritingProcess)
	 */
	@Override
	public Expression iterateValuesUsingExpansions(Expression msgValues, Expression msgExpansions, RewritingProcess process) {
		
		Expression result = null;
		
		List<Expression> msgValuesUnionArguments     = Union.getEntriesFromUnionOrSet(msgValues);
		List<Expression> msgExpansionsUnionArguments = Union.getEntriesFromUnionOrSet(msgExpansions);
		
		ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp messageValueSets = new ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp(msgValuesUnionArguments, makeSymbolicLookUpForPreviousMessages(), configuration.isBeliefUseCache());
		messageValueSets = iterateValuesUsingExpansions(messageValueSets, msgExpansionsUnionArguments, process);
		List<Expression> nextMsgValuesUnionArguments = messageValueSets.getIntensionalSets();
		
		result = Union.make(nextMsgValuesUnionArguments);
		
		return result;
	}
	
	/**
	 * 
	 * @see UseValuesForPreviousMessages#useValuesForPreviousMessages(Expression, Expression, RewritingProcess)
	 */
	@Override
	public Expression useValuesForPreviousMessages(Expression expansion, Expression msgValues, RewritingProcess process) {
		Expression result = null;
		
		List<Expression> msgValuesUnionArguments = Union.getEntriesFromUnionOrSet(msgValues);

		ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValuesMap = new ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp(msgValuesUnionArguments, makeSymbolicLookUpForPreviousMessages(), configuration.isBeliefUseCache());
		result = useValuesForPreviousMessages(expansion, msgValuesMap, process);

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
		
		Expression previousRandomVariableBeingNormalized = LPIUtil.setRandomVariableBeingNormalizedAndReturnPreviousOne(randomVariable, process);
		
		process = LPIUtil.extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(randomVariable, process);
		process = LPIUtil.extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(Tuple.make(Model.getRewritingProcessesModel(process).getParfactorsDeclaration().getParfactors()), process);
		
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

		Expression neighV          = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_NEIGHBOR, randomVariable);
		Expression rewriteOfNeighV = process.rewrite(R_neigh_v, neighV);

		Expression msgToV_F = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_MSG_TO_FROM, randomVariable, factorIndex);
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
			List<Expression> msgSets = Union.getEntriesFromUnionOrSet(process.rewrite(R_extract_previous_msg_sets, beliefExpansion));		
			Trace.log("// msg_sets = {}", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", msgSets));
		
			Trace.log("msg_expansions <- get_msg_expansions(msg_sets)");
			// a union of intensional sets containing tuples of the form:
			// (Destination, Origin, Expansion), where Expansion is a basic expression
			// possibly containing "previous message" expressions, representing their 
			// value in terms of messages from the previous loopy BP iteration.
			List<Expression> msgExpansions = getMessageExpansions(msgSets, process);
			Trace.log("// msg_expansions = {}", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", msgExpansions));
			
			Trace.log("msg_values <- copy of msg_expansions, with the values replaced by uniform messages");
			// a union of sets of tuples (N1, N2, value) where (N1,N2) represents a message and value
			// is a basic expression representing its value at the current loopy BP iteration
			// (this expression is therefore free of previous message expressions).
			// { (on I) (Destination, Origin, 1) | C } union ...
			ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp messageValueSets = new ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp(new ArrayList<Expression>(), makeSymbolicLookUpForPreviousMessages(), configuration.isBeliefUseCache());
			for (Expression msgExpansion : msgExpansions) {
				// i.e. (Destination, Origin, Expansion)
				Expression tupleExpansion = ((IntensionalSet) msgExpansion).getHead();
				// (Destination, Origin, 1)
				Expression destination = getDestinationFromMessageValueTuple(tupleExpansion);
				Expression origin      = getOriginFromMessageValueTuple(tupleExpansion);
				Expression value       = Expressions.ONE;
				Expression tuple       = makeMessageValueTuple(destination, origin, value);
				// { (on I) (Destination, Origin, 1) | C }
				Expression msgValue       = IntensionalSet.make(Sets.getLabel(msgExpansion), 
																((IntensionalSet) msgExpansion).getIndexExpressions(),
																tuple, 
																((IntensionalSet) msgExpansion).getCondition());
				messageValueSets.putIntensionalSetWithKeyKnownToBeDisjointFromOthers(msgValue, process);
			}
			
			// We now use the expansions to get the values of message pairs in successive loopy BP iterations
			// until either convergence or maximum number of iterations.
			Trace.log("while belief_value <- use_values_for_previous_msgs(belief_expansion, msg_values) not final");
			Trace.log("    msg_values <- iterate_values_using_expansions(msg_values, msg_expansions)");
			
			Expression beliefValue          = beliefExpansion;
			Expression priorBeliefValue     = beliefExpansion;
			ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp priorMsgValues = null;
			int iteration = 1;
			beliefValue = useValuesForPreviousMessages(beliefExpansion, messageValueSets, process);
			notifyCollector(randomVariable, beliefValue, 1, process);
			Justification.log("Initial belief value {}", beliefValue);
			Trace.log("// initial belief_value = {}", beliefValue);
			while (notFinal(beliefValue, priorBeliefValue, messageValueSets, priorMsgValues, iteration)) {
				priorBeliefValue = beliefValue;
				priorMsgValues = messageValueSets;
				Justification.beginEqualityStep("iteration");
				LiftProductOfFactorToVariable.setMustAlwaysLift(true, process);
				messageValueSets = iterateValuesUsingExpansions(messageValueSets, msgExpansions, process);
				beliefValue      = useValuesForPreviousMessages(beliefExpansion, messageValueSets, process);
				LiftProductOfFactorToVariable.setMustAlwaysLift(false, process);
				iteration++;			
				notifyCollector(randomVariable, beliefValue, iteration, process);
				Justification.endEqualityStep(beliefValue);
				Trace.log("    // msg_values = {}", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", messageValueSets));
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
			process.removeGlobalObject(SYMBOLIC_MAP_FOR_MESSAGE_VALUES_SETS_GLOBAL_OBJECTS_KEY);
		}
		
		LPIUtil.restorePreviousRandomVariableBeingNormalized(previousRandomVariableBeingNormalized, process);
		
		Justification.endEqualityStep(result);
		
		return result;
	}

	//
	// PRIVATE METHODS
	//

	// The following methods determine how (Destination, Origin, Value) are stored in intensional sets.
	
	private static Expression makeMessageValueTuple(Expression destination, Expression origin, Expression value) {
		Expression result = Tuple.make(destination, origin, value);
		return result;
	}
	
	private static Expression getDestinationOriginTuple(Expression messageValueTuple) {
		return Tuple.make(messageValueTuple.get(0), messageValueTuple.get(1));
	}

	private static Expression getDestinationFromMessageValueTuple(Expression tuple) {
		return Tuple.get(tuple, 0);
	}

	private static Expression getOriginFromMessageValueTuple(Expression tuple) {
		return Tuple.get(tuple, 1);
	}

	private static Expression getValueFromMessageValueTuple(Expression tuple) {
		return Tuple.get(tuple, 2);
	}

	private static Function<Expression, Expression> getDestinationAndOriginFromPreviousMessage = new Function<Expression, Expression> () {
		@Override
		public Expression apply(Expression previousMessageExpression) {
			return Tuple.make(previousMessageExpression.get(0), previousMessageExpression.get(1));
		}
	};
	private static Function<Expression, Expression> getDestinationAndOriginFromMessageValueTuple = new Function<Expression, Expression> () {
		@Override
		public Expression apply(Expression messageValueTuple) {
			return getDestinationOriginTuple(messageValueTuple);
		}
	};
	private static Function<Expression, Expression> getValueFromMessageValueTuple = new Function<Expression, Expression> () {
		@Override
		public Expression apply(Expression messageValueTuple) {
			return getValueFromMessageValueTuple(messageValueTuple);
		}
	};

	private static SymbolicInjectiveLookUp makeSymbolicLookUpForPreviousMessages() {
		SymbolicInjectiveLookUp result = new SymbolicInjectiveLookUp(getDestinationAndOriginFromPreviousMessage, getDestinationAndOriginFromMessageValueTuple, getValueFromMessageValueTuple, LBPRewriter.R_complete_normalize, true);
		return result;
	}

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
	
	private List<Expression> getMessageExpansions(List<Expression> messageSets, RewritingProcess process) {		
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
			checkForLegalMsgSet(messageSet);
	
			Justification.begin("Going to expand message set {}", messageSet);
			Trace.in("Going to expand message set {}", messageSet);

			messageSet = removeAlreadyExpandedMessages(messageSet, messagesAlreadyExpandedUnionArguments, process);
			
			if (!Sets.isEmptySet(messageSet)) {
				Trace.log("    if message_set is not empty");
				expand(messageSet, messagesAlreadyExpandedUnionArguments, messagesToBeExpanded, messageExpansions, process);
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
		Expression alpha = ((IntensionalSet) messageSet).getHead();
		for (Expression messagesAlreadyExpandedUnionArgument : messagesAlreadyExpandedUnionArguments) {				
			Expression alphaPrime = ((IntensionalSet) messagesAlreadyExpandedUnionArgument).getHead(); 
			// Perform a cheap disequality first
			if (!CheapDisequalityModule.isACheapDisequality(alpha, alphaPrime, process)) {
				possibleIntersectionUnionArgs.add(messagesAlreadyExpandedUnionArgument);
			}
		}
		Expression msgsAlreadyExpanded = Union.make(possibleIntersectionUnionArgs);
		
		Trace.log("    message_set <- R_set_diff(message_set minus msgs_already_expanded)");
		messageSet = process.rewrite(R_set_diff, LPIUtil.argForSetDifferenceRewriteCall(messageSet, msgsAlreadyExpanded));
		Trace.log("    message_set <- R_complete_normalize(message_set)");
		messageSet = process.rewrite(R_complete_normalize, messageSet);
		Justification.end("Remaining, non-expanded messages are {}", messageSet);
		Trace.out("Removed them, and remaining message_set is {}", messageSet);
		return messageSet;
	}

	private void expand(Expression messageSet, List<Expression> messagesAlreadyExpandedUnionArguments, List<Expression> messagesToBeExpanded, List<Expression> messageExpansions, RewritingProcess process) {
		IndexExpressionsSet indexExpressions = ((IntensionalSet) messageSet).getIndexExpressions();
		List<Expression> expressionI = ((ExtensionalIndexExpressionsSet) indexExpressions).getList();
		Expression destinationOriginTuple = ((IntensionalSet) messageSet).getHead();
		Expression destination            = Tuple.get(destinationOriginTuple, 0);
		Expression origin                 = Tuple.get(destinationOriginTuple, 1);
		Expression conditionC             = ((IntensionalSet) messageSet).getCondition();
		
		Expression expansion = computeExpansionSymbolically(messageSet, destination, origin, conditionC, process);
		
		Trace.log("        msg_expansion <- { (on I) (Destination, Origin, expansion) | C }");
		Expression msgExpansion = new DefaultIntensionalUniSet(expressionI, makeMessageValueTuple(destination, origin, expansion), conditionC);
		
		sanityChecksOnLogicalVariables(messageSet, messageExpansions, process, msgExpansion);
		
		Trace.log("        index <- size of msg_expansions");
		Trace.log("        cache index as msg_value_index corresponding to 'previous message to Destination from Origin' under constraining condition C");
		Trace.log("        msg_expansions <- msg_expansions union msg_expansion");
		messageExpansions.add(msgExpansion);
		
		updateSetOfMessagesAlreadyExpanded(messagesAlreadyExpandedUnionArguments, expressionI, destination, origin, conditionC);
		
		identifyMessagesUsedByMessageExpansionAndScheduleThemForExpansionThemselves(msgExpansion, messagesToBeExpanded, process);
	}

	private Expression computeExpansionSymbolically(Expression messageSet, Expression destination, Expression origin, Expression conditionC, RewritingProcess process) {
		Justification.begin("Going to re-arrange as message expression");
		// represent message_set as { (on I) (Destination, Origin) | C }

		Trace.log("        expansion <- compute message to Destination from Origin with");
		Trace.log("                     beingComputed = empty set and contextual constraint C");
		Expression       msgTo_From    = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_MSG_TO_FROM, destination, origin);
		Justification.end("Message expression is {}", msgTo_From);

		
		Justification.begin("Going to symbolically compute message expression");
		Expression expansion = null;
		Justification.begin("Going to symbolically compute unsimplified message expression");
		// Set up the contextual constraint
		RewritingProcess processExtendedByC = GrinderUtil.extendContextualSymbolsAndConstraintWithIntensionalSet(messageSet, process);
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
		if (checkIfExpansionDependsOnLogicalVariableButMessageDoesNot(msgExpansion, process)) {
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

	private void updateSetOfMessagesAlreadyExpanded(List<Expression> messagesAlreadyExpandedUnionArguments, List<Expression> expressionI, Expression destination, Expression origin, Expression conditionC) {
		Trace.log("        msgs_already_expanded <- msgs_already_expanded union { (on I) (Destination, Origin) | C }");
		Expression alreadyExpanded = new DefaultIntensionalUniSet(expressionI, Tuple.make(destination, origin), conditionC);
		messagesAlreadyExpandedUnionArguments.add(alreadyExpanded);
		Trace.log("        // msgs_already_expanded = {}", Expressions.apply(FunctorConstants.UNION, messagesAlreadyExpandedUnionArguments));
	}

	private void identifyMessagesUsedByMessageExpansionAndScheduleThemForExpansionThemselves(Expression msgExpansion, List<Expression> messagesToBeExpanded, RewritingProcess process) {
		Trace.log("        msgs_to_be_expanded <- msgs_to_be_expanded union R_extract_previous_msg_sets(msg_expansion)");
		Justification.begin("Going to identify messages inside expansion and schedule them for their own expansion");
		Expression newMsgSetsToBeExpanded = process.rewrite(R_extract_previous_msg_sets, msgExpansion);
		messagesToBeExpanded.addAll(Union.getEntriesFromUnionOrSet(newMsgSetsToBeExpanded));
		Justification.end("Messages inside expansion identified: {}", newMsgSetsToBeExpanded);
		Trace.log("        // msgs_to_be_expanded = {}", Tuple.make(messagesToBeExpanded));
	}

	private ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp iterateValuesUsingExpansions(
			final ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValues,
			final List<Expression> msgExpansions,
			RewritingProcess process) {
		
		Trace.in("+iterate_values_using_expansions({}, {})", msgValues.getIntensionalSets(), msgExpansions);
		
		Trace.log("next_msg_values <- empty set ");
		List<Expression> nextMsgValues = new ArrayList<Expression>();
		Trace.log("for each union argument { (on I) (Destination, Origin, Expansion) | C } in msg_expansions");
		BranchRewriteTask[] iterateValueTasks = new BranchRewriteTask[msgExpansions.size()];
		for (int i = 0; i < msgExpansions.size(); i++) {
			Expression msgExpansion = msgExpansions.get(i);
			
			RewriteOnBranch iterateValueTask = new RewriteOnBranch() {
				@Override
				public Expression rewrite(Expression[] expressions, RewritingProcess process) {
					Expression msgExpansion      = expressions[0];
					Expression msgExpansionTuple = ((IntensionalSet) msgExpansion).getHead();
					Expression destination       = getDestinationFromMessageValueTuple(msgExpansionTuple);
					Expression origin            = getOriginFromMessageValueTuple(msgExpansionTuple);
					Expression expansion         = getValueFromMessageValueTuple(msgExpansionTuple);
					Trace.log("    // msgExpansion={}", msgExpansion);
					Trace.log("    // expansion   ={}", expansion);
					
					RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsAndConstraintWithIntensionalSet(msgExpansion, process);
					
					Trace.log("    value <- use_values_for_previous_msgs(Expansion, msg_values) under contextual constraint expanded by C");
					Expression value = useValuesForPreviousMessages(expansion, msgValues, subProcess);
					Trace.log("    // value={}", value);
					
					checkForPreviousMessageSubExpressionsOrProductExpressions(value, msgExpansion, expansion, msgValues, msgExpansions, subProcess);
					
					Expression destinationOrOriginRandomVariable = LPIUtil.isRandomVariable(destination, subProcess)? destination : origin;
					Expression destinationOrOriginRandomVariableValue = ((BracketedExpression) destinationOrOriginRandomVariable).getInnerExpression();
					
					checkForOtherRandomVariableValueExpressions(value, msgValues, msgExpansions, msgExpansion, destination, origin, expansion, destinationOrOriginRandomVariableValue, subProcess);
					
					// Normalize and manage precision
					Expression normalizedValue = value;
					if (Model.isAllTypeSizesKnown(subProcess)) {
						Expression randomVariable = null;
						if (BracketedExpressionSubExpressionsProvider.isRandomVariable(destination, subProcess)) {
							randomVariable = destination;
						} 
						else {
							randomVariable = origin;
						}
						
						Expression previousRandomVariableBeingNormalized = LPIUtil.setRandomVariableBeingNormalizedAndReturnPreviousOne(randomVariable, process);

						normalizedValue = subProcess.rewrite(R_normalize_message, LPIUtil.argForNormalizeRewriteCall(randomVariable, value));
						normalizedValue = limitPrecisionToNumberOfSignificantDecimalPlaces(normalizedValue, subProcess);
						
						LPIUtil.restorePreviousRandomVariableBeingNormalized(previousRandomVariableBeingNormalized, process);
					}
					
					Trace.log("    next_msg_values <- next_msg_values union { (on I) (Destination, Origin, value) | C }");
					Expression messageValueTuple = makeMessageValueTuple(destination, origin, normalizedValue);
					Expression newMsgValue = new DefaultIntensionalUniSet(((IntensionalSet) msgExpansion).getIndexExpressions(), messageValueTuple, ((IntensionalSet) msgExpansion).getCondition());
					
					checkForIntroducedVariables(value, msgValues, msgExpansions, process, msgExpansion, expansion, newMsgValue, subProcess);
					
					checkIfExpansionDependsOnLogicalVariableButMessageDoesNot(value, msgValues, msgExpansions, process, msgExpansion, expansion, newMsgValue, subProcess);
					
					return newMsgValue;
				}
			};
			
			iterateValueTasks[i] = new BranchRewriteTask(iterateValueTask, new Expression[] {msgExpansion});
		}
		
		List<Expression> taskResults = GrinderUtil.branchAndMergeTasks(iterateValueTasks, process);
		
		nextMsgValues.addAll(taskResults);
		
		Trace.log("return next_msg_values");
		Trace.out("-iterate_values_using_expansions={}", nextMsgValues);
		
		ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp result = msgValues.replaceIntensionalSets(nextMsgValues);
		
		return result;
	}

	private Expression useValuesForPreviousMessages(Expression expansion, ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValues, RewritingProcess process) {
		
		Expression result = null;

		Trace.in("+use_values_for_previous_msgs({}, {})", expansion, Expressions.apply("list", msgValues.getIntensionalSets())); // "list" used so msgValues is properly shown in trace tree.
		Trace.log("substituted <- expansion");
		Expression substituted = expansion;

		Trace.log("exhaustively replace all expressions in substituted with the following replacement function:");

		// Ensure we exhaustively replace
		Expression toSubstitute;
		do {
			toSubstitute = substituted;	
			substituted  = toSubstitute.replaceAllOccurrences(
										new PreviousMessageReplacementFunction(msgValues), 
										process);
		} while (substituted != toSubstitute);
			
		Trace.log("// substituted = {}, constrained by {}", substituted, process.getContextualConstraint());
		Trace.log("return R_complete_normalize(substituted)");
		result = process.rewrite(R_complete_normalize, substituted);
		
		Trace.out("-use_values_for_previous_msgs={}", result);
		
		return result;
	}
	
	private boolean notFinal(Expression beliefValue, Expression priorBeliefValue, ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValues, ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp priorMsgValues, int iteration) {
		boolean notFinal = true;
		
		Trace.log("Belief Value at iteration {} is = {}", iteration, beliefValue);
		
		if (LBPConfiguration.BeliefPropagationUpdateSchedule.isAsynchronous(configuration.getBeliefPropagationUpdateSchedule())) { 
			if (beliefValue.equals(priorBeliefValue)) {
				Trace.log("Final Belief Value: belief value has not changed between iterations, iteration = {}", iteration);
				notFinal = false;
			}
		} 
		else {
			if (priorMsgValues != null && msgValues.equals(priorMsgValues)) {
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
	
	private Expression limitPrecisionToNumberOfSignificantDecimalPlaces(Expression normalizedValue, RewritingProcess process) {
		Expression result = normalizedValue.replaceAllOccurrences(new AbstractReplacementFunctionWithContextuallyUpdatedProcess() {
					@Override
					public Expression apply(Expression expressionE, RewritingProcess process) {
						Expression result = expressionE;
						if (expressionE.getSyntacticFormType().equals("Symbol")) {
							if (expressionE.getValue() instanceof Number) {
								Rational n = expressionE.rationalValue();
								// Handle the precision of non zero values
								if (!n.isZero()) {
									n = n.multiply(decimalPrecision).round(Rational.ROUND_HALF_UP).divide(decimalPrecision);
									if (n.isZero()) {
										// ensure we don't underflow and introduce zeros
										n = nonZeroMin;
									}
									result = Expressions.makeSymbol(n);
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
		
		Collection<Expression> introducedFreeVariables = Util.setDifference(freeInMsg, process.getContextualSymbols());
		
		return introducedFreeVariables;
	}
	
	private class PreviousMessageReplacementFunction extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		
		private ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValues;
		
		public PreviousMessageReplacementFunction(ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValues) {
			this.msgValues = msgValues;
		}
		
		//
		// START-ReplacementFunctionWithContextuallyUpdatedProcess
		@Override
		public Expression apply(Expression expressionE, RewritingProcess process) {
			Expression result = null;
			
			if ( ! LPIUtil.isPreviousMessageDefinition(expressionE)) {
				// if E is not of the form "previous message to Destination' from Origin'
				//     return E
				result = expressionE;
			} 
			else {
				Trace.log("    value <- look up value for {} in msg_values", expressionE);
				Expression value = msgValues.lookUp(expressionE, process);
				
				Trace.log("    return value");
				Trace.log("    // (replacing {} by {})", expressionE, value);
				result = value;
			}
			
			return result;
		}
		// END-ReplacementFunctionWithContextuallyUpdatedProcess
		//
	}
	
	private boolean checkIfExpansionDependsOnLogicalVariableButMessageDoesNot(Expression msgExpansionOrValue, final RewritingProcess process) {
		boolean result = false;
		
		List<Expression> indices = IndexExpressions.getIndices(((IntensionalSet) msgExpansionOrValue).getIndexExpressions());
		Expression messageValueTuple = ((IntensionalSet) msgExpansionOrValue).getHead();	
		Expression destination = getDestinationFromMessageValueTuple(messageValueTuple);
		Expression origin      = getOriginFromMessageValueTuple(messageValueTuple);
		Set<Expression> destinationVariables          = Expressions.getVariables(destination, process);
		Set<Expression> originVariables               = Expressions.getVariables(origin, process);
		Set<Expression> originAndDestinationVariables = new LinkedHashSet<Expression>();
		originAndDestinationVariables.addAll(destinationVariables);
		originAndDestinationVariables.addAll(originVariables);
		originAndDestinationVariables.retainAll(indices);
		
		Set<Expression> randomVariableValueVariables = new LinkedHashSet<Expression>();
		Set<Expression> randomVariableValues         = LPIUtil.getRandomVariableValueExpressions(getValueFromMessageValueTuple(messageValueTuple), process);
		for (Expression randomVariableValue : randomVariableValues) {
			randomVariableValueVariables.addAll(Expressions.getVariables(randomVariableValue, process));
		}
		// Only keep random variable value logical variables that are not internally
		// scoped (e.g. in a product message from an expansion).
		Expression value = getValueFromMessageValueTuple(messageValueTuple);
		randomVariableValueVariables.retainAll(Expressions.freeVariables(value, process));
		
		Set<Expression> testSet = new LinkedHashSet<Expression>();
		testSet.addAll(Expressions.freeVariables(value, process));
		testSet.retainAll(indices);
		testSet.removeAll(originAndDestinationVariables);
		testSet.removeAll(randomVariableValueVariables);
				
		if (testSet.size() > 0) {
			System.err.println("expansion or value  ="+msgExpansionOrValue);
			System.err.println("tesSet              ="+testSet);
			System.err.println("destOriginSet       ="+originAndDestinationVariables);
			System.err.println("destinationVariables="+destinationVariables);
			System.err.println("originVariables     ="+originVariables);
			System.err.println("rvValueVariables    ="+randomVariableValueVariables);
			result = true;
		}
		
		return result;
	}

	private static void checkForLegalMsgSet(Expression msgSet) {
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
				tuple = ExtensionalSet.getElements(msgSet).get(0);
			}
		} 
		else if (Sets.isIntensionalUniSet(msgSet)) {
			tuple = ((IntensionalSet) msgSet).getHead();
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

	private void checkIfExpansionDependsOnLogicalVariableButMessageDoesNot(Expression value, final ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValues, final List<Expression> msgExpansions, RewritingProcess process, Expression msgExpansion, Expression expansion, Expression newMsgValue, RewritingProcess subProcess) {
		if (checkIfExpansionDependsOnLogicalVariableButMessageDoesNot(newMsgValue, process)) {
			System.err.println("IllegalStateException: new_msg_value has answer dependent on logical variable that value is not.");
			System.err.println("sub.context      ="+subProcess.getContextualConstraint());
			System.err.println("sub.context vars ="+subProcess.getContextualSymbols());
			System.err.println("msg_expansion    ="+msgExpansion);
			System.err.println("expansion        ="+expansion);
			System.err.println("value            ="+value);
			System.err.println("new_msg_value    ="+newMsgValue);
			System.err.println("msg_expansions   =");
			for (Expression me : msgExpansions) {
				System.err.println(me);
			}
			System.err.println("msg_values       =");
			for (Expression mv : msgValues.getIntensionalSets()) {
				System.err.println(mv);
			}
			throw new IllegalStateException("IllegalStateException: new_msg_value has answer dependent on logical variable that value is not: "+newMsgValue);
		}
	}

	private void checkForIntroducedVariables(Expression value, final ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValues, final List<Expression> msgExpansions, RewritingProcess process, Expression msgExpansion, Expression expansion, Expression newMsgValue, RewritingProcess subProcess) {
		Collection<Expression> introducedFreeVariables = getIntroducedFreeVariables(newMsgValue, process);
		if ( ! introducedFreeVariables.isEmpty()) {
			System.err.println("IllegalStateException: introduced additional free variables into new_msg_value.");
			System.err.println("sub.context      ="+subProcess.getContextualConstraint());
			System.err.println("sub.context vars ="+subProcess.getContextualSymbols());
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
			for (Expression mv : msgValues.getIntensionalSets()) {
				System.err.println(mv);
			}
			throw new IllegalStateException("IllegalStateException: introduced additional free variables " + Util.join(introducedFreeVariables) + " into new_msg_value: "+newMsgValue);
		}
	}

	private void checkForOtherRandomVariableValueExpressions(Expression value, final ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValues, final List<Expression> msgExpansions, Expression msgExpansion, Expression destination, Expression origin, Expression expansion, Expression destinationOrOriginRandomVariableValue, RewritingProcess subProcess) {
		List<Pair<Expression, Expression>> otherRandomVariables;
		if ((otherRandomVariables = LPIUtil.findRandomVariableValueExpressionsThatAreNotNecessarilyTheSameAsAGivenOne(value, destinationOrOriginRandomVariableValue, subProcess)
				).size() != 0) {
			// Output some useful information before throwing the exception
			System.err.println("IllegalStateException: value depends on random variable value other than its own destination/origin random variable.");
			System.err.println("Destination: " + destination);
			System.err.println("Origin: "      + origin);
			System.err.println("Message value should be a function of: " + destinationOrOriginRandomVariableValue);
			System.err.println("value            = " + value);
			System.err.println("Other random variable values appearing in message value that may not unify with " + destinationOrOriginRandomVariableValue +
					           ", paired with the contexts in which they appear:\n" + Util.join(otherRandomVariables, "\n"));
			System.err.println("sub.context      = " + subProcess.getContextualConstraint());
			System.err.println("sub.context vars = " + subProcess.getContextualSymbols());
			System.err.println("msg_expansion    = " + msgExpansion);
			System.err.println("expansion        = " + expansion);
			System.err.println("msg_expansions   = ");
			for (Expression me : msgExpansions) {
				System.err.println(me);
			}
			System.err.println("msg_values       = ");
			for (Expression m : msgValues.getIntensionalSets()) {
				System.err.println(m);
			}
			throw new IllegalStateException("value depends on random variable value other than its own destination/origin random variable: " + value);
		}
	}

	private void checkForPreviousMessageSubExpressionsOrProductExpressions(Expression value, Expression msgExpansion, Expression expansion, final ListOfDisjointIntensionalSetsForSymbolicInjectiveLookUp msgValues, final List<Expression> msgExpansions, RewritingProcess subProcess) {
		if (LPIUtil.containsPreviousMessageExpressions(value) || LPIUtil.containsProductExpressions(value)) {	
			// Output some useful information before throwing the exception
			System.err.println("IllegalStateException: invalid message value.");
			System.err.println("sub.context      ="+subProcess.getContextualConstraint());
			System.err.println("sub.context vars ="+subProcess.getContextualSymbols());
			System.err.println("msg_expansion    ="+msgExpansion);
			System.err.println("expansion        ="+expansion);
			System.err.println("value            ="+value);
			System.err.println("msg_expansions   =");
			for (Expression me : msgExpansions) {
				System.err.println(me);
			}
			System.err.println("msg_values       =");
			for (Expression m : msgValues.getIntensionalSets()) {
				System.err.println(""+m);
			}
			throw new IllegalStateException("new msg_value contains previous message to . from . or product(...):"+value);
		}
	}
}
