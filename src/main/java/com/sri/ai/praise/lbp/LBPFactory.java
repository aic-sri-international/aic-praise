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
package com.sri.ai.praise.lbp;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewriterLookup;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.rewriterrefiner.RewriterRefiner;
import com.sri.ai.praise.PRAiSEConfiguration;
import com.sri.ai.praise.lbp.core.AnyimeRandomVariableFromMessageRewriterCall;
import com.sri.ai.praise.lbp.core.AnytimeRefiner;
import com.sri.ai.praise.lbp.core.Basic;
import com.sri.ai.praise.lbp.core.Belief;
import com.sri.ai.praise.lbp.core.BoundBelief;
import com.sri.ai.praise.lbp.core.CompleteNormalize;
import com.sri.ai.praise.lbp.core.ConvexRewriterOnMessageBounds;
import com.sri.ai.praise.lbp.core.DefaultLBPConfiguration;
import com.sri.ai.praise.lbp.core.DifferenceOfExtensionalAndExtensionalSet;
import com.sri.ai.praise.lbp.core.DifferenceOfExtensionalAndIntensionalSet;
import com.sri.ai.praise.lbp.core.ExtractPreviousMessageSets;
import com.sri.ai.praise.lbp.core.FormulaSimplification;
import com.sri.ai.praise.lbp.core.In;
import com.sri.ai.praise.lbp.core.IntensionalSimplification;
import com.sri.ai.praise.lbp.core.Intersection;
import com.sri.ai.praise.lbp.core.LBPRandomVariableFromMessageRewriterCall;
import com.sri.ai.praise.lbp.core.MessageToFactorFromVariable;
import com.sri.ai.praise.lbp.core.MessageToVariableFromFactor;
import com.sri.ai.praise.lbp.core.NeighborsFactor;
import com.sri.ai.praise.lbp.core.NeighborsOfRandomVariableInParfactor;
import com.sri.ai.praise.lbp.core.NeighborsRandomVariable;
import com.sri.ai.praise.lbp.core.NormalizeMessage;
import com.sri.ai.praise.lbp.core.NormalizeRandomVariableCondition;
import com.sri.ai.praise.lbp.core.ProductFactor;
import com.sri.ai.praise.lbp.core.ProductMessageAndProductFactor;
import com.sri.ai.praise.lbp.core.SetDifference;
import com.sri.ai.praise.lbp.core.Normalize;
import com.sri.ai.praise.lbp.core.Simplify;
import com.sri.ai.praise.lbp.core.Sum;
import com.sri.ai.praise.lbp.core.Union;
import com.sri.ai.praise.model.Model;
import com.sri.ai.util.functionalsequence.RefinerIterator;

/**
 * Factory class for instantiating Lifted Belief Propagation (LBP) related objects (i.e. query engines and rewriters).
 * 
 * @author oreilly
 * 
 */
@Beta
public class LBPFactory {
	
	public static LBPQueryEngine newLBPQueryEngine() {
		LBPQueryEngine result = PRAiSEConfiguration.newConfiguredInstance(PRAiSEConfiguration.getLBPQueryEngineClass());
		return result;
	}
	
	public static Rewriter getRootRewriter() {
		return (new Simplify()).getRootRewriter();
	}
	
	public static Rewriter newNormalize() {
		Normalize normalize = new Normalize();
		return normalize;
	}
	
	public static Rewriter newCompleteNormalize() {
		CompleteNormalize completeNormalize = new CompleteNormalize();
		return completeNormalize;
	}
	
	public static LBPConfiguration newLBPConfiguration() {
		return new DefaultLBPConfiguration();
	}
	
	public static RewritingProcess newLBPProcessWithHighLevelModel(String highLevelModel) {
		RewritingProcess process = newLBPProcess(null);
		Model model = Model.fromRules(highLevelModel);
		model.setRewritingProcessesModel(process);
		return process;
	}
	
	public static RewritingProcess newLBPProcess() {
		return newLBPProcess(null);
	}
	
	public static RewritingProcess newLBPProcess(Expression rootExpression) {
		return newLBPProcess(rootExpression, newLBPConfiguration());
	}
	
	public static RewritingProcess newLBPProcess(Expression rootExpression, LBPConfiguration configuration) {
		return newLBPProcess(rootExpression, configuration, null);
	}
	
	public static RewritingProcess newLBPProcess(Expression rootExpression, LBPConfiguration configuration, RewritingProcess parentProcess) {
		DefaultRewriterLookup lbpRewriterLookup = newLBPDefaultRewriterLookup(configuration);
		
		RewritingProcess lbpProcess = makeLBPProcess(rootExpression, lbpRewriterLookup, configuration, parentProcess);
													
		return lbpProcess;
	}
	
	public static MessageExpansions newMessageExpansions(RewritingProcess lbpProcess) {
		return getBelief(lbpProcess); 
	}
	
	public static IterateValuesUsingExpansions newIterateValuesUsingExpansions(RewritingProcess lbpProcess) {
		return getBelief(lbpProcess); 
	}
	
	public static UseValuesForPreviousMessages newUseValuesForPreviousMessages(RewritingProcess lbpProcess) {
		return getBelief(lbpProcess);
	}
	
	public static RewritingProcess newBoundLBPProcess(Expression rootExpression) {
		return newBoundLBPProcess(rootExpression, newLBPConfiguration());
	}
	
	public static RewritingProcess newBoundLBPProcess(Expression rootExpression, LBPConfiguration configuration) {
		return newBoundLBPProcess(rootExpression, configuration, null);
	}
	
	public static RewritingProcess newBoundLBPProcess(Expression rootExpression, LBPConfiguration configuration, RewritingProcess parentProcess) {
		DefaultRewriterLookup boundLbpRewriterLookup = newBoundLBPDefaultRewriterLookup(configuration);
		
		RewritingProcess boundLbpProcess = makeLBPProcess(rootExpression, boundLbpRewriterLookup, configuration, parentProcess);

		return boundLbpProcess;
	}
	
	public static Iterator<Expression> newAnytimeLBPRewriteRequest(String rewriterName, Expression expression, RewritingProcess parentProcess) {
		RewritingProcess boundLBPProcess = newBoundLBPProcess(expression, newLBPConfiguration(), parentProcess);
		
		RewriterRefiner refiner = new AnytimeRefiner(rewriterName, expression, boundLBPProcess,
																	new AnyimeRandomVariableFromMessageRewriterCall());
		
		Iterator<Expression> result = new RefinerIterator<Expression>(refiner);

		return result; 
	}
	
	//
	// PRIVATE 
	//
	private static Belief getBelief(RewritingProcess lbpProcess) {
		return (Belief) ((DefaultRewriterLookup)((DefaultRewritingProcess)lbpProcess).getRewriterLookup()).get(LBPRewriter.R_belief);
	}
	
	private static RewritingProcess makeLBPProcess(Expression rootExpression, DefaultRewriterLookup lbpRewriterLookup, LBPConfiguration configuration, RewritingProcess parentProcess) {
		Map<Expression, Expression> contextualVariablesDomains = null;
		Predicate<Expression> isConstantPredicate              = null;
		Map<Object, Object>   globalObjects                    = null;
		
		if (parentProcess != null) {
			contextualVariablesDomains = parentProcess.getContextualVariablesDomains();
			isConstantPredicate        = parentProcess.getIsConstantPredicate();
			globalObjects              = parentProcess.getGlobalObjects();
		}
		else {
			contextualVariablesDomains = new HashMap<Expression, Expression>();
			isConstantPredicate        = new PrologConstantPredicate();
			globalObjects              = new HashMap<Object, Object>();
		}
		
		DefaultRewritingProcess lbpProcess = new DefaultRewritingProcess(
				rootExpression,
				getRootRewriter(),
				lbpRewriterLookup,
				contextualVariablesDomains,
				isConstantPredicate,
				globalObjects);

		return lbpProcess;
	}
	
	private static DefaultRewriterLookup newBoundLBPDefaultRewriterLookup(LBPConfiguration configuration) {
		DefaultRewriterLookup boundLBPRewriterLookup = newLBPDefaultRewriterLookup(configuration);
		
		RandomVariableFromMessageRewriterCall randomVariableFromInnerRewriterCall = new LBPRandomVariableFromMessageRewriterCall();

		//
		// Add in the bound rewriters
		boundLBPRewriterLookup.put(LBPRewriter.R_bound_belief, new BoundBelief());
		//
		Map<String, String> messageToFactorFromVariableChildRedirectMap = new HashMap<String, String>();
		messageToFactorFromVariableChildRedirectMap.put(LBPRewriter.R_prod_factor,   LBPRewriter.R_bound_prod_factor);
		ConvexRewriterOnMessageBounds rBoundMessageToFactorFromVariable = new ConvexRewriterOnMessageBounds(LBPRewriter.R_bound_m_to_f_from_v,
				LBPRewriter.R_m_to_f_from_v, randomVariableFromInnerRewriterCall, messageToFactorFromVariableChildRedirectMap);
		boundLBPRewriterLookup.put(LBPRewriter.R_bound_m_to_f_from_v, rBoundMessageToFactorFromVariable);
		//
		Map<String, String> messageToVariableFromFactorChildRedirectMap = new HashMap<String, String>();
		messageToVariableFromFactorChildRedirectMap.put(LBPRewriter.R_sum,           LBPRewriter.R_bound_sum);
		ConvexRewriterOnMessageBounds rBoundMessageToVariableFromFactor = new ConvexRewriterOnMessageBounds(LBPRewriter.R_bound_m_to_v_from_f,
				LBPRewriter.R_m_to_v_from_f, randomVariableFromInnerRewriterCall, messageToVariableFromFactorChildRedirectMap);
		boundLBPRewriterLookup.put(LBPRewriter.R_bound_m_to_v_from_f, rBoundMessageToVariableFromFactor);
		//
		Map<String, String> productFactorChildRedirectMap = new HashMap<String, String>();
		productFactorChildRedirectMap.put(LBPRewriter.R_m_to_v_from_f,          LBPRewriter.R_bound_m_to_v_from_f);
		productFactorChildRedirectMap.put(LBPRewriter.R_prod_m_and_prod_factor, LBPRewriter.R_bound_prod_m_and_prod_factor);
		ConvexRewriterOnMessageBounds rBoundProductFactor = new ConvexRewriterOnMessageBounds(LBPRewriter.R_bound_prod_factor,
				LBPRewriter.R_prod_factor, randomVariableFromInnerRewriterCall, productFactorChildRedirectMap);
		boundLBPRewriterLookup.put(LBPRewriter.R_bound_prod_factor, rBoundProductFactor);
		//
		Map<String, String> productMessageAndProductFactorChildRedirectMap = new HashMap<String, String>();
		productMessageAndProductFactorChildRedirectMap.put(LBPRewriter.R_prod_factor, LBPRewriter.R_bound_prod_factor);
		ConvexRewriterOnMessageBounds rBoundProductMessageAndProductFactor = new ConvexRewriterOnMessageBounds(LBPRewriter.R_bound_prod_m_and_prod_factor,
				LBPRewriter.R_prod_m_and_prod_factor, randomVariableFromInnerRewriterCall, productMessageAndProductFactorChildRedirectMap);
		boundLBPRewriterLookup.put(LBPRewriter.R_bound_prod_m_and_prod_factor, rBoundProductMessageAndProductFactor);
		//
		Map<String, String> sumChildRedirectMap = new HashMap<String, String>();
		sumChildRedirectMap.put(LBPRewriter.R_m_to_f_from_v, LBPRewriter.R_bound_m_to_f_from_v);
		ConvexRewriterOnMessageBounds rBoundSum = new ConvexRewriterOnMessageBounds(LBPRewriter.R_bound_sum,
				LBPRewriter.R_sum, randomVariableFromInnerRewriterCall, sumChildRedirectMap);
		boundLBPRewriterLookup.put(LBPRewriter.R_bound_sum, rBoundSum);
		
		return boundLBPRewriterLookup;
	}
	
	private static DefaultRewriterLookup newLBPDefaultRewriterLookup(LBPConfiguration configuration) {
		DefaultRewriterLookup lbpRewriterLookup = new DefaultRewriterLookup(DirectCardinalityComputationFactory.getCardinalityRewritersMap());

		//
		lbpRewriterLookup.put(LBPRewriter.R_basic,  new Basic());
		lbpRewriterLookup.put(LBPRewriter.R_belief, new Belief(configuration));
		lbpRewriterLookup.put(LBPRewriter.R_normalize, new Normalize());
		lbpRewriterLookup.put(LBPRewriter.R_complete_normalize, new CompleteNormalize());
		lbpRewriterLookup.put(LBPRewriter.R_DifferenceOfExtensionalAndExtensionalSet, new DifferenceOfExtensionalAndExtensionalSet());
		lbpRewriterLookup.put(LBPRewriter.R_DifferenceOfExtensionalAndIntensionalSet, new DifferenceOfExtensionalAndIntensionalSet());
		lbpRewriterLookup.put(LBPRewriter.R_extract_previous_msg_sets, new ExtractPreviousMessageSets());
		lbpRewriterLookup.put(LBPRewriter.R_formula_simplification, new FormulaSimplification());
		lbpRewriterLookup.put(LBPRewriter.R_in, new In());
		lbpRewriterLookup.put(LBPRewriter.R_intensional_simplification, new IntensionalSimplification());
		lbpRewriterLookup.put(LBPRewriter.R_intersection, new Intersection());
		lbpRewriterLookup.put(LBPRewriter.R_m_to_f_from_v, new MessageToFactorFromVariable(configuration));
		lbpRewriterLookup.put(LBPRewriter.R_m_to_v_from_f, new MessageToVariableFromFactor(configuration));
		lbpRewriterLookup.put(LBPRewriter.R_neigh_f, new NeighborsFactor());
		lbpRewriterLookup.put(LBPRewriter.R_neigh_v, new NeighborsRandomVariable());
		lbpRewriterLookup.put(LBPRewriter.R_neigh_v_parf, new NeighborsOfRandomVariableInParfactor());
		lbpRewriterLookup.put(LBPRewriter.R_normalize_message, new NormalizeMessage());
		lbpRewriterLookup.put(LBPRewriter.R_normalize_random_variable_condition, new NormalizeRandomVariableCondition());
		lbpRewriterLookup.put(LBPRewriter.R_prod_factor, new ProductFactor());
		lbpRewriterLookup.put(LBPRewriter.R_prod_m_and_prod_factor, new ProductMessageAndProductFactor());
		lbpRewriterLookup.put(LBPRewriter.R_set_diff, new SetDifference());
		lbpRewriterLookup.put(LBPRewriter.R_sum, new Sum(configuration));
		lbpRewriterLookup.put(LBPRewriter.R_union, new Union());
		
		return lbpRewriterLookup;
	}
}
