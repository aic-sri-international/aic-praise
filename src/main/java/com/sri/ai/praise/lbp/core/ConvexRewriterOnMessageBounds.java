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
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.LambdaExpression;
import com.sri.ai.expresso.core.DefaultLambdaExpression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.IsApplicationOf;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.ChildRewriterCallIntercepter;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.SimplifyMessagesConvexHull;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.lbp.RandomVariableFromMessageRewriterCall;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Equals;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;

/**
 * <p>
 * A general purpose intercepter class to allow existing rewriters to be able to
 * take message bounds (convex hulls) and return convex hulls of possible
 * results. When a function is convex, this is cheap because we can use the
 * extrema only:
 * </p>
 * 
 * <pre>
 * f('convex hull'(S1), ..., 'convex hull'(Sn)) 
 * = 
 * 'convex hull'( { (on s1,...,sn) f(s1, ..., sn) }  )
 *                for s_i iterating over the extrema of Si.
 * </pre>
 * 
 * For example, in a 1-dimensional case (<b>Note:</b> '+' is convex):
 * 
 * <pre>
 * +('convex hull'( {1, 2} ), 'convex hull'( {3, 4} ) ) 
 * = 
 * 'convex hull'( { 1 + 3, 1 + 4, 2 + 3, 2 + 4 } ) 
 * =
 * 'convex hull'( { 4, 5, 5, 6 } ) 
 * =
 * 'convex hull'( { 4, 6 } )
 * </pre>
 * 
 * <p>
 * All LPI rewriters that take messages in and produce messages out are convex,
 * so we can apply the principle above to them all, implementing it only once
 * via this class ConvexRewriterOnMessageBounds. This intercepter wraps around
 * any of our regular message rewriters.
 * </p>
 * <p>
 * When it is time for ConvexRewriterOnMessageBounds to rewrite something, it
 * runs the inner rewriter with hooks on child-rewriter invocation. This
 * intercepter does the following: if the child-rewriter on an expression E
 * returns something that is not a bound, the listener simply returns the same
 * thing; if the child-rewriter does return a bound B (a 'convex hull' function
 * application on its extremum messages), the listener saves B and returns a
 * placeholder associated with B. At the end of this executing of the inner
 * rewriter, we have a basic expression that contains a bunch of
 * child-expressions that should have been rewritten but were not (i.e. are
 * placeholders), but for which we have bounds.
 * </p>
 * <p>
 * ConvexRewriterOnMessageBounds then iterates over all possible extrema
 * combinations from all those bounds, replacing the placeholder expressions by
 * those extrema, and obtaining a set of expressions, one for each combination.
 * We then construct a new 'convex hull' application on an extensional set of
 * them, and simplify. This will provide a simplified convex hull that we then
 * return.
 * </p>
 * For example: suppose in the original algorithm we have a rewriter that
 * computes something equivalent to:
 * 
 * <pre>
 * message to [p] from [ Factor1 ] ^ | { (on X) X : X != a } | * message to [p] from [ Factor2 ]
 * </pre>
 * 
 * Say we invoke the child-rewriters for message to [p] from [ Factor1 ], | {
 * (on X) X : X != a } |, and message to [p] from [ Factor2 ] and obtain:
 * 
 * <pre>
 * if p then 0.7 else 0.3 
 * 5
 * if p then 0.6 else 0.4
 * </pre>
 * 
 * respectively.<br>
 * <br>
 * In our new scenario, the child-rewriter call is intercepted. Say we still get
 * a non-bound value for the first child-rewriter, with the same value as
 * before:
 * 
 * <pre>
 * if p then 0.7 else 0.3
 * </pre>
 * 
 * Then the intercepter just returns that same thing, with the same happening
 * with the second rewriter:
 * 
 * <pre>
 * 5
 * </pre>
 * 
 * For the third child-rewriter, because now we get message bounds, we get, say:
 * 
 * <pre>
 * 'convex hull'( { [if p then 0.8 else 0.2], [if p then 0.4 else 0.6] } )
 * </pre>
 * 
 * We save this bound associated with a placeholder 'convex hull child
 * placeholder'(1) and return the placeholder. Now we have the basic expression:
 * 
 * <pre>
 * (if p then 0.7 else 0.3)^5 * ('convex hull child placeholder'(1))
 * </pre>
 * 
 * For each combination of extrema (not too interesting since there is only one
 * bound) and then applying 'convex hull' on their set, we get:
 * 
 * <pre>
 * 'convex hull'( { (if p then 0.7 else 0.3)^5 * (if p then 0.8 else 0.2),  
 *                  (if p then 0.7 else 0.3)^5 * (if p then 0.4 else 0.6)  } )
 * </pre>
 * 
 * which is:
 * 
 * <pre>
 * 'convex hull'( { [if p then 0.134456 else 0.000486],  
 *                  [if p then 0.067228 else 0.001458] } )
 * </pre>
 * 
 * and after normalization/simplification is:
 * 
 * <pre>
 * 'convex hull'( { [if p then 0.996398453 else 0.003601547],  
 *                  [if p then 0.978772967 else 0.021227033] } )
 * </pre>
 * 
 * <b>Note</b> that if another child-call had also resolved to a bound, we would
 * have four arguments in that convex hull. In that case, we would have a
 * simplification of 'convex hull' to something with two arguments only.<br>
 * <br>
 * <b>PSEUDO-CODE:<b><br>
 * 
 * <pre>
 * R_convex_rewriter_on_message_bounds(R, E, t)
 * // NOTE: This logic is as it is due to the fact message values and not messages
 * // are currently returned by message rewriters within the system.
 * R is a rewriter such that R(E) is a message M on a random variable [ V ].
 * Returns a conditional bound B on messages on [ V ] at time step t.
 * (each bound is represented by the convex hull of a set of messages on [ V ]).
 * The relationship between B and M is as follows.
 * M can be seen as a function M(M1,...,Mn) on input messages M1,...,Mn computed by R.
 * R_convex_rewriter_on_message_bounds intercepts the calculation of M1,...,Mn and obtains conditional bounds Bt1,...,Btn instead, where Btj contains Mj.
 * The resulting bound B is the convex hull of { (on E1,...,En) M(E1,...,En) }, where Ej ranges over the extrema of bound Btj.
 * 
 * C  <- contextual constraint
 * RV <- get_random_variable_for(R, E)
 * if there is no message_value_with_placeholders(E, C)
 *     message_value_with_placeholders(E,C) <- R(E) with following intercepter:
 *         intercept(child_rewriter, E', C')
 *             if child_rewriter is a message producer
 *                 placeholder <- placeholder(child_rewriter, E', C')
 *                 bound <- map_from_placeholder_to_bound(E,C).get(placeholder)
 *                          possibly creating and storing the value with
 *                             R_convex_rewriter_on_message_bounds(child_rewriter, E', t) under C'
 *                          // bound will be created pretty much every time;
 *                          // it will only be reused instead if R invoked (E', C') more than once  
 *                 [v'] <- get_random_variable_for(child_rewriter, E')                       
 *                 return (lambda v' : placeholder)(v')
 *             else
 *                 return rewrite(child_rewriter, E') under C'
 * else
 *     for each placeholder in keys of map_from_placeholder_to_bound(E,C)
 *         (child_rewriter, E', C') <- from placeholder
 *         bound <- R_convex_rewriter_on_message_bounds(child_rewriter, E', t) under C'
 *         map_from_placeholder_to_bound(E,C).put(placeholder, bound)
 * 
 * // at this point, map_from_placeholder_to_bound contains Bt1,...,Btn
 * message_values_without_placeholders 
 *     <- expand_message_values_with_placeholders(
 *           message_value_with_placeholders(E,C),
 *           map_from_placeholder_to_bound(E,C))
 *                           
 * return make_simplified_convex_hull(RV, message_values_without_placeholders)
 * 
 * -----
 * expand_message_values_with_placeholders(message_value_with_placeholders,
 *                                         map_from_placeholder_to_bound)
 * 
 * if message_value_with_placeholders is conditional if C then M1 else M2
 *     return if C
 *            then expand_message_value_with_placeholders(M1, map_from_placeholder_to_bound)
 *            else expand_message_value_with_placeholders(M2, map_from_placeholder_to_bound)
 * else
 *     iterate_extrema({ message_value_with_placeholders }, // singleton set
 *                       copy of map_from_placeholder_to_bound)
 * -----
 * iterate_extrema(message_values_set, map_from_placeholder_to_bound)
 * Receives an extensional set of message values with placeholders
 * and a map from placeholders to conditional bounds,
 * and replaces every message value in the set by all the possible message values
 * obtained by replacing each placeholder by one of its bound's extrema
 * 
 * if there is a P in message_values_set and (P, B) is in map_from_placeholder_to_bound 
 *     if B is conditional bound if C then B1 else B2 and P is not scoped by message_values_set
 *         // Externalize conditionals
 *         thenMap <- copy of map_from_placeholder_to_bound with P mapping to B1
 *         elseMap <- copy of map_from_placeholder_to_bound with P mapping to B2
 *         return 
 *             if C
 *             then iterate_extrema(message_values_set, thenMap)
 *             else iterate_extrema(message_values_set, elseMap)
 *     else // replace all message values by their possibilities according to unconditional B
 *         new_message_values_set <- empty list
 *         for each message_value in message_values_set
 *             for each extremum value M of B
 *                 new_message_value <- R_normalize(message_value[P/M])
 *                 add new_message_value to new_message_values_set
 *         return iterate_extrema(new_message_values_set, map_from_placeholder_to_bound)
 *         
 * return message_values_set   
 *      
 * -----
 * make_simplified_convex_hull([V], message_values_set)
 * [V] is the random variable for which messages are being bound.
 * message_values_set is a conditional extensional set of unconditional message values
 * 
 * // Externalizes conditionals, that is,
 * if message_values_set is if C then message_values_set1 else message_values_set2
 *     return 
 *         if C
 *         then make_simplified_convex_hull([V], message_values_set1)
 *         else make_simplified_convex_hull([V], message_values_set2)
 * else // message_values_set is an unconditional set of unconditional message values
 *     messages_set <- empty list
 *     for each message_value in message_values_set
 *         if message_value is a numeric constant
 *             message <- [if V then message_value else message_value]
 *         else
 *             message <- [message_value]
 *         add message to messages_set
 *     if messges_set still contains any placeholders
 *         return 'convex hull'(messages_set)
 *     else
 *         return R_simplify_messages_convex_hull('convex hull'(messages_set))
 *     
 * -----
 * get_random_variable_for(R, E)
 * // Extracts the Random Variable associated with rewriter R's argument expression E.
 * // This will be a utility routine that works for each of the rewriters that
 * // R_convex_rewriter_on_message_bounds is a wrapper for.
 * </pre>
 * 
 * @author oreilly
 * 
 */
@Beta
public class ConvexRewriterOnMessageBounds extends
		AbstractLBPHierarchicalRewriter {
	//
	private static final String FUNCTOR_CONVEX_HULL_CHILD_PLACEHOLDER = "convex hull child placeholder";
	// To ease tracing ensure all placeholders are given a globally unique id.
	private static AtomicLong _placeholderUniqueId = new AtomicLong(0L);
	//
	// Used to simplify message bounds before returning
	private Rewriter rSimplifyBounds = new SimplifyMessagesConvexHull();
	//
	private String                                innerRewriterNameR                  = null;
	private RandomVariableFromMessageRewriterCall randomVariableFromInnerRewriterCall = null;
	private Map<String, String>                   boundChildCallRedirectMap           = new LinkedHashMap<String, String>();
	// Maps where the key (E,C) is represented by a Pair. 
	private Map<Pair<Expression, Expression>, Map<Placeholder, Expression>> mapFromPlaceholderToBoundEC    = new ConcurrentHashMap<Pair<Expression,Expression>, Map<Placeholder, Expression>>();
	private Map<Pair<Expression, Expression>, Expression>                   messageValueWithPlaceholdersEC = new ConcurrentHashMap<Pair<Expression,Expression>, Expression>();
	/**
	 * Constructor.
	 * 
	 * @param name
	 *            the name to be associated with this instance of a bound
	 *            wrapper (e.g. R_bound_prod_factor).
	 * @param innerRewriterName
	 *            the name of the inner rewriter that this is a bound wrapper
	 *            for (e.g. R_prod_factor).
	 * @param randomVariableFromInnerRewriterCall
	 *            a utility routine that works for each of the rewriters that
	 *            an instance of this class is a rewriter for.
	 * @param boundChildCallRedirectMap
	 *            a map indicating which child rewriters that can be called by
	 *            the named inner rewriter should be mapped to bound wrappers,
	 *            e.g.:<br>
	 *            'R_m_to_v_from_f'          -> 'R_bound_m_to_v_from_f'
	 *            'R_prod_m_and_prod_factor' -> 'R_bound_prod_m_and_prod_factor'
	 */
	public ConvexRewriterOnMessageBounds(String name, String innerRewriterName, 
			RandomVariableFromMessageRewriterCall randomVariableFromInnerRewriterCall,
			Map<String, String> boundChildCallRedirectMap) {
		setName(name);
		this.innerRewriterNameR                  = innerRewriterName;
		this.randomVariableFromInnerRewriterCall = randomVariableFromInnerRewriterCall;
		this.boundChildCallRedirectMap.putAll(boundChildCallRedirectMap);
		// For convenience:
		// Ensure recursive calls from the inner rewriter to itself
		// are captured in this map as well.
		if (!this.boundChildCallRedirectMap.containsKey(innerRewriterName)) {
			this.boundChildCallRedirectMap.put(innerRewriterName, name);
		}
	}
	
	/**
	 * Determine whether or not a given expressions contains any bound
	 * placedholder expressions.
	 * 
	 * @param expression
	 *            the expression to be tested.
	 * @return true if the given expressions contains one or more placeholder
	 *         expressionds, false otherwise.
	 */
	public static boolean containsPlaceholderExpression(Expression expression) {
		boolean result = false;
		if (Expressions.hasFunctor(expression, FUNCTOR_CONVEX_HULL_CHILD_PLACEHOLDER)) {
			result = true;
		} 
		else {
			Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(
					expression);
			result = Util.thereExists(subExpressionsIterator, new IsApplicationOf(FUNCTOR_CONVEX_HULL_CHILD_PLACEHOLDER));
		}
		
		return result;
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression,
			RewritingProcess process) {
		// R_convex_rewriter_on_message_bounds(R, E, t)
		// Extract E and t
		if (!(Tuple.isTuple(expression) && Tuple.size(expression) == 2)) {
			throw new IllegalArgumentException("Input expression should be a tuple of the form: (E, t) was "+expression);
		}
		Expression expressionE        = Tuple.get(expression, 0);
		Expression expressionTimestep = Tuple.get(expression, 1); 
		
		Expression result = expressionE;
		
		Trace.log("C <- contextual constraint");
		Expression expressionC  = process.getContextualConstraint();
		Trace.log("// C ={}", expressionC);
		Trace.log("RV <- get_random_variable_for(R,E)");
		Expression expressionRV = randomVariableFromInnerRewriterCall.getRandomVariableFor(innerRewriterNameR, expressionE);
		Trace.log("// RV={}", expressionRV);
		
		// Determine: if there is no message_value_with_placeholders(E, C)
		boolean firstTime = false;
		Pair<Expression, Expression> keyEC                     = new Pair<Expression, Expression>(expressionE, expressionC);
		Map<Placeholder, Expression> mapFromPlaceholderToBound = null;
		// Synchronize access to the main map
		// we try to identify the correct mapFromPlaceholderToBound for this request
		synchronized (mapFromPlaceholderToBoundEC) {
			mapFromPlaceholderToBound = mapFromPlaceholderToBoundEC.get(keyEC);
			// If this is null so will message_value_with_placeholders for this key, however,
			// we synchronize on mapFromPlaceholderToBound as while its contents will change,
			// the object itself will remain fixed so easier to synchronize on.
			if (mapFromPlaceholderToBound == null) {
				mapFromPlaceholderToBound = new LinkedHashMap<Placeholder, Expression>();
				mapFromPlaceholderToBoundEC.put(keyEC, mapFromPlaceholderToBound);
				firstTime = true;
			}
		}
		
		// At this point only need to synchronize the mapFromPlaceholderToBound 
		// specific to this invocation.
		Expression messageValuesWithoutPlaceholders = null;
		synchronized (mapFromPlaceholderToBound) {
			if (firstTime) {
				Trace.log("if there is no message_value_with_placeholders(E={}, C={})", expressionE, expressionC);
				Trace.log("    message_value_with_placeholders(E,C) <- R(E) with following intercepter:");
				InnerChildRewriteCallIntercepter intercepter = new InnerChildRewriteCallIntercepter(expressionTimestep);
				Expression expressionRofE                    = process.rewrite(innerRewriterNameR, expressionE, intercepter);
				mapFromPlaceholderToBound.putAll(intercepter.getMapFromPlaceholderToBound());
				messageValueWithPlaceholdersEC.put(keyEC, expressionRofE);
			}
			else {
				Trace.log("else // there is a message_value_with_placeholders(E={}, C={})", expressionE, expressionC);
				Trace.log("    for each placeholder in keys of map_from_placeholder_to_bound(E,C)");
				for (Placeholder placeholder : mapFromPlaceholderToBound.keySet()) {
					Trace.log("        (child_rewriter, E', C') <- from placeholder");
					Trace.log("        bound <- R_convex_rewriter_on_message_bounds(child_rewriter, E', t) under C'");
					Expression bound = placeholder.callMessageBoundsOnChildRewriter(expressionTimestep);
					Trace.log("        map_from_placeholder_to_bound(E,C).put(placeholder, bound)");
					mapFromPlaceholderToBound.put(placeholder, bound);
				}
			}
			
			Trace.log("// at this point, map_from_placeholder_to_bound contains Bt1,...,Btn");
			Trace.log("message_values_without_placeholders <- expand_message_values_with_placeholders(message_value_with_placeholders(E,C), map_from_placeholder_to_bound(E,C))");
 			messageValuesWithoutPlaceholders = expandMessageValuesWithPlaceholders(messageValueWithPlaceholdersEC.get(keyEC),
 																					mapFromPlaceholderToBound,
 																					process);
		}
		
		Trace.log("return make_simplified_convex_hull(RV, message_values_without_placeholders)");
		result = makeSimplifiedConvexHull(expressionRV, messageValuesWithoutPlaceholders, process);
		
		return result; 
	}

	//
	// PRIVATE
	//
	private class InnerChildRewriteCallIntercepter implements ChildRewriterCallIntercepter {	
		//
		private Map<Placeholder, Expression>                             mapFromPlaceholderToBound = new LinkedHashMap<Placeholder, Expression>();
		private Expression                                               expressionTimestep        = null;
		private Map<Triple<String, Expression, Expression>, Placeholder> sharedPlaceholders        = new LinkedHashMap<Triple<String,Expression,Expression>, Placeholder>(); 
		
		public InnerChildRewriteCallIntercepter(Expression expressionTimestep) {
			this.expressionTimestep = expressionTimestep;
		}
		
		public Map<Placeholder, Expression> getMapFromPlaceholderToBound() {
			// Note: We keep and return a local copy so as not to worry about
			// synchronization issues from the caller (i.e. the intercepter) 
			// as could be called on multiple threads separate from that
			// of the thread that called the inner rewriter.
			return mapFromPlaceholderToBound;
		}

		//
		// START-ChildRewriterCallIntercepter
		@Override
		public Expression intercept(String rewriterName, Expression expression, RewritingProcess process) {
			Expression result = null;
			
			Trace.in("+intercept(child_rewriter={}, E'={}, C'={})", rewriterName, expression, process.getContextualConstraint());		
			String boundRewriterName = boundChildCallRedirectMap.get(rewriterName);
			if (boundRewriterName != null) {				
				Trace.log("if child_rewriter is a message producer");
				Trace.log("    placeholder <- placeholder(child_rewriter, E', C')");
				Trace.log("    bound <- map_from_placeholder_to_bound(E,C).get(placeholder)");
				Trace.log("             possibly creating and storing the value with R_convex_rewriter_on_message_bounds(child_rewriter, E', t) under C'");
				// Note: bound will be created pretty much every time;
				// it will only be reused instead if R invokes (E',C') more than once
				Placeholder                            placeholder    = null;
				Triple<String, Expression, Expression> placeholderKey = new Triple<String, Expression, Expression>(boundRewriterName, expression, process.getContextualConstraint());
				synchronized (sharedPlaceholders) {
					placeholder = sharedPlaceholders.get(placeholderKey);
					if (placeholder == null) {
						placeholder = new Placeholder(boundRewriterName, rewriterName, expression, expressionTimestep, process, _placeholderUniqueId.getAndAdd(1));
						placeholder.callMessageBoundsOnChildRewriter(expressionTimestep);
						sharedPlaceholders.put(placeholderKey, placeholder);
					}
				}
				mapFromPlaceholderToBound.put(placeholder, placeholder.getBound());
				Trace.log("    [v'] <- get_random_variable_for(child_rewriter, E')");
				Trace.log("    // v'={}", placeholder.getRandomVariableValueExpression());
				Trace.log("    return (lambda v' : placeholder)(v')");
				Expression lambdaPlaceholder = Expressions.apply(new DefaultLambdaExpression(arrayList(placeholder.getRandomVariableValueExpression()), placeholder.getPlaceholderExpression()), placeholder.getRandomVariableValueExpression());
				result = lambdaPlaceholder;
			}
			else {
				Trace.log("else // child_rewriter is not a message producer");
				Trace.log("    return rewrite(child_rewriter, E') under C'");
				result = process.rewrite(rewriterName, expression);
			}
			Trace.out("-intercept={}", result);
			
			return result;
		}
		// END-ChildRewriterCallIntercepter
		//
	}
	
	private Expression expandMessageValuesWithPlaceholders(Expression messageValueWithPlaceholders, final Map<Placeholder, Expression> mapFromPlaceholderToBound, RewritingProcess process) {
		Expression result = null;
		
		Trace.in("+expand_message_values_with_placeholders({})", messageValueWithPlaceholders);
		
		if (IfThenElse.isIfThenElse(messageValueWithPlaceholders) && !isMessageValueWithPossiblePlaceholders(messageValueWithPlaceholders, process)) {
			Trace.log("if message_value_with_placeholders is conditionaal if C then M1 else M2");
			Trace.log("    return if C then expand_message_value_with_placeholders(M1, map_from_placeholder_to_bound) else expand_message_value_with_placeholders(M2, map_from_placeholder_to_bound)");
			result = GrinderUtil.branchAndMergeOnACondition(IfThenElse.getCondition(messageValueWithPlaceholders), 
					new RewriteOnBranch() {	
						@Override
						public Expression rewrite(Expression[] expressions, RewritingProcess process) {
							Expression result = expandMessageValuesWithPlaceholders(expressions[0], mapFromPlaceholderToBound, process);
							return result;
						}
					}, 
					new Expression[] {IfThenElse.getThenBranch(messageValueWithPlaceholders)}, 
					new RewriteOnBranch() {
						@Override
						public Expression rewrite(Expression[] expressions, RewritingProcess process) {
							Expression result = expandMessageValuesWithPlaceholders(expressions[0], mapFromPlaceholderToBound, process);
							return result;
						}
					}, 
					new Expression[] {IfThenElse.getElseBranch(messageValueWithPlaceholders)}, 
					LBPRewriter.R_check_branch_reachable, 
					null, process);
		}
		else {
			Trace.log("else");
			Trace.log("    iterate_extrema({message_value_with_placeholders}, copy of map_from_placeholder_to_bound");
			result = iterateExtrema(ExtensionalSet.makeUniSet(messageValueWithPlaceholders), new LinkedHashMap<Placeholder, Expression>(mapFromPlaceholderToBound), process);
		}

		Trace.out("-expand_message_valus_with_placeholders={}", result);
		
		return result;
	}

	
	/**
	 * iterate_extrema(message_values_set, map_from_placeholder_to_bound)
	 * Receives an extensional set of message values with placeholders and a map
	 * from placeholders to conditional bounds, and replaces every message value
	 * in the set by all the possible message values obtained by replacing each
	 * placeholder by one of its bound's extrema
	 * 
	 * @param messageValuesSet
	 *            a set of message values with placeholders.
	 * @param mapFromPlaceholderToBound
	 *            a map from placeholders to conditional bounds
	 * @param process
	 *            the process under which the rewriting is occurring.
	 * @return an extensional set with every message value in the set replaced
	 *         by all the possible message values obtained by replacing each
	 *         placeholder by one of its bound's extrema.
	 */
	private Expression iterateExtrema(Expression messageValuesSet, Map<Placeholder, Expression> mapFromPlaceholderToBound, RewritingProcess process) {
		Expression result = messageValuesSet;
		
		Trace.in("+iterate_extrema({})", messageValuesSet);

		Trace.log("if there is a P in message_values_set and (P, B) is in map_from_placeholder_to_bound");
		Pair<Placeholder, Expression> pairPB = null;	
		if ((pairPB = retrievePB(messageValuesSet, mapFromPlaceholderToBound, process)) != null ) {
			Placeholder placeholderP = pairPB.first;
			Expression  expressionB  = pairPB.second;

			Trace.log("// P={}", placeholderP.getPlaceholderExpression());
			Trace.log("// B={}", expressionB);
			
			if (IfThenElse.isIfThenElse(expressionB) && !isMessageValueWithPossiblePlaceholders(expressionB, process) && !isPlaceholderScopedBy(placeholderP.getPlaceholderExpression(), messageValuesSet, process)) {
				Trace.log("    if B is conditional bound if C then B1 else B2 and P is not scoped by message_values_set");
				Trace.log("        // Externalize Conditionals");
				Trace.log("        thenMap <- copy of map_from_placeholder_to_bound with P mapping to B1");
				final Map<Placeholder, Expression> thenMap = new LinkedHashMap<Placeholder, Expression>(mapFromPlaceholderToBound);
				thenMap.put(placeholderP, IfThenElse.getThenBranch(expressionB));
				Trace.log("        elseMap <- copy of map_from_placeholder_to_bound with P mapping to B2");
				final Map<Placeholder, Expression> elseMap = new LinkedHashMap<Placeholder, Expression>(mapFromPlaceholderToBound);
				elseMap.put(placeholderP, IfThenElse.getElseBranch(expressionB));
				Trace.log("        return if C then iterate_extrema(message_values_set, thenMap) else iterate_extrema(message_values_set, elseMap)");				
				result = GrinderUtil.branchAndMergeOnACondition(IfThenElse.getCondition(expressionB), 
						new RewriteOnBranch() {	
							@Override
							public Expression rewrite(Expression[] expressions, RewritingProcess process) {
								Expression result = iterateExtrema(expressions[0], thenMap, process);
								return result;
							}
						}, 
						new Expression[] { messageValuesSet }, 
						new RewriteOnBranch() {
							@Override
							public Expression rewrite(Expression[] expressions, RewritingProcess process) {
								Expression result = iterateExtrema(expressions[0], elseMap, process);
								return result;
							}
						}, 
						new Expression[] { messageValuesSet }, 
						LBPRewriter.R_check_branch_reachable, 
						null, process);
			}
			else {
				Trace.log("    else // replace all message values by their possibilities according to unconditional B");
				Trace.log("        new_message_values_set <- empty list");
				List<Expression> newMessageValuesSet = new ArrayList<Expression>();
				Trace.log("        for each message_value in message_values_set");
				for (Expression messageValue : ExtensionalSet.getElements(messageValuesSet)) {
					Trace.log("        for each extremum value M of B");
					for (Expression extrenumValueM : getExtremumValues(expressionB, process)) {
						Trace.log("        // message_value={}", messageValue);
						Trace.log("        // M            ={}", extrenumValueM);
						Trace.log("        new_message_value <- R_normalize(message_value[P/M])");
						Expression messageValuePReplacedByM = messageValue.replaceAllOccurrences(placeholderP.getPlaceholderExpression(), extrenumValueM, process);
						Trace.log("        // message_value[P/M]={}", messageValuePReplacedByM);
						Expression newMessageValue          = process.rewrite(LBPRewriter.R_normalize, messageValuePReplacedByM);
						Trace.log("        // new_message_value={}", newMessageValue);
						Trace.log("        add new_message_value to new_message_values_set");
						newMessageValuesSet.add(newMessageValue);
					}
				}
				Trace.log("        return iterate_extrema(new_message_value_set, map_from_placeholder_to_bound)");
				result = iterateExtrema(ExtensionalSet.makeUniSetExpression(newMessageValuesSet), mapFromPlaceholderToBound, process);
			}
		}
		
		// i.e. no relevant placeholders remaining.
		if (pairPB == null) {
			Trace.log("return message_values_set");
		}
		
		Trace.out("-iterate_extrema={}", result);
		
		return result;
	}
	
	private Expression makeSimplifiedConvexHull(final Expression expressionRV, Expression messageValuesSet, RewritingProcess process) {
		Expression result = null;
		
		Trace.in("+make_simplified_convex_hull({}, {})", expressionRV, messageValuesSet);
		
		// Externalize conditionals, that is
		if (IfThenElse.isIfThenElse(messageValuesSet)) {
			Trace.log("if message_values_set is if C then message_values_set1 else message_values_set2");
			Trace.log("    return if C then make_simpflified_convex_hull([V], message_values_set1) else make_simplified_convex_hull([V], message_values_set2)");
			result = GrinderUtil.branchAndMergeOnACondition(IfThenElse.getCondition(messageValuesSet), 
					new RewriteOnBranch() {	
						@Override
						public Expression rewrite(Expression[] expressions, RewritingProcess process) {
							Expression result = makeSimplifiedConvexHull(expressionRV, expressions[0], process);
							return result;
						}
					}, 
					new Expression[] { IfThenElse.getThenBranch(messageValuesSet) }, 
					new RewriteOnBranch() {
						@Override
						public Expression rewrite(Expression[] expressions, RewritingProcess process) {
							Expression result = makeSimplifiedConvexHull(expressionRV, expressions[0], process);
							return result;
						}
					}, 
					new Expression[] { IfThenElse.getElseBranch(messageValuesSet) }, 
					LBPRewriter.R_check_branch_reachable, 
					LBPRewriter.R_normalize, 
					process);
		}
		else {
			Trace.log("else // message_values_set is an unconditional set of unconditional message values");
			Trace.log("    message_set <- empty_list");
			List<Expression> messagesSet = new ArrayList<Expression>();
			Expression expressionV = LPIUtil.getRandomVariableValueExpression(expressionRV, process);
			Trace.log("    for each message_value in message_values_set");
			boolean stillContainsChildPlaceholders = false;
			for (Expression messageValue : ExtensionalSet.getElements(messageValuesSet)) {
				Expression message = null;
				if (messageValue.getSyntacticFormType().equals("Symbol")) {
					Trace.log("        if message_value_is a numeric constant");
					Trace.log("            message <- [if V then message_value else message_value]");
					message = BracketedExpressionSubExpressionsProvider.make(IfThenElse.make(expressionV, messageValue, messageValue));
				}
				else {
					Trace.log("        else // message_value_is conditioned on random variable's value");
					Trace.log("            message <- [message_value]");
					message = BracketedExpressionSubExpressionsProvider.make(messageValue);
					if (!stillContainsChildPlaceholders) {
						stillContainsChildPlaceholders = containsPlaceholderExpression(messageValue);
					}
				}
				Trace.log("        add message to messages_set");
				messagesSet.add(message);
			}
			Expression convexHullOfMessages = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(LPIUtil.FUNCTOR_CONVEX_HULL, ExtensionalSet.makeUniSet(messagesSet));
			
			if (stillContainsChildPlaceholders) {
				Trace.log("    if messages_set still contains any placeholders");
				Trace.log("        return 'convex hull'(messages_set)");
				// Don't simplify as there are child placeholders in the expression that where not inserted
				// by this call (i.e. was passed thru as an argument higher up).
				result = convexHullOfMessages;
			}
			else {
				Trace.log("    else // messages_set contains no placeholders");
				Trace.log("        return R_simplify_messages_convex_hull('convex hull'(messages_set))");
				result = rSimplifyBounds.rewrite(convexHullOfMessages, process);
			}
		}
	
		Trace.out("-make_simplified_convex_hull="+result);
		
		return result;
	}	
	
	private Pair<Placeholder, Expression> retrievePB(Expression messageValuesSet, Map<Placeholder, Expression> mapFromPlaceholderToBound, RewritingProcess process) {
		Pair<Placeholder, Expression> result = null;
	
		// Logic for: if there is a P in message_values_set and (P, B) is in map_from_placeholder_to_bound 
		for (Map.Entry<Placeholder, Expression> entryPB : mapFromPlaceholderToBound.entrySet()) {
			Placeholder placeholderP = entryPB.getKey();
			Expression  expressionB  = entryPB.getValue();
			
			if (Expressions.isSubExpressionOf(placeholderP.getPlaceholderExpression(), messageValuesSet)) {		
				result = new Pair<Placeholder, Expression>(placeholderP, expressionB);
				break;
			}
		}
		
		return result;
	}
	
	private List<Expression> getExtremumValues(Expression bound, final RewritingProcess process) {
		List<Expression> result = new ArrayList<Expression>();
		
		if (Expressions.hasFunctor(bound, LPIUtil.FUNCTOR_CONVEX_HULL)) {
			result.addAll(LPIUtil.extractMessageValuesFromConvexHull(bound, process));
		}
		else {
			result.add(bound);
		}
		
		return result;
	}
	
	private boolean isMessageValueWithPossiblePlaceholders(Expression expression, RewritingProcess process) {
		boolean result = false;
		if (IfThenElse.isIfThenElse(expression)) {
			Expression condition = IfThenElse.getCondition(expression);
			if (LPIUtil.isRandomVariableValueExpression(condition, process)) {			
				result = true;
			}
		}
		else {
			result = LPIUtil.isMessageValue(expression, process);
		}
		return result;
	}
	
	private boolean isPlaceholderScopedBy(Expression placeholder, Expression expressionBy, RewritingProcess process) {
		boolean result = false;
		
		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expressionBy);
		while (subExpressionsIterator.hasNext()) {
			Expression subExpression = subExpressionsIterator.next();
			// Note: Skip lambda expression, as we put placeholders in lambda expressions.
			if (!(subExpression instanceof LambdaExpression) && subExpression.getScopedExpressions(process).size() > 0) {
				if (Util.thereExists(new SubExpressionsDepthFirstIterator(subExpression), new Equals<Expression>(placeholder))) {
					result = true;				
					break;
				}
			}
		}
		
		return result;
	}
	
	private class Placeholder {
		private String           boundRewriterName;
		private Expression       expression;
		private RewritingProcess process;
		private Expression       placeholderExpression;
		private Expression       bound;
		private Expression       randomVariableValueExpression;
		
		public Placeholder(String boundRewriterName, String innerRewriterName, Expression expression, Expression timestep, RewritingProcess process, long id) {
			this.boundRewriterName = boundRewriterName;
			this.expression        = expression;
			this.process           = process;
			placeholderExpression  = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONVEX_HULL_CHILD_PLACEHOLDER, id);
			
			Expression expressionRV       = randomVariableFromInnerRewriterCall.getRandomVariableFor(innerRewriterName, expression);
			randomVariableValueExpression = LPIUtil.getRandomVariableValueExpression(expressionRV, process);
		}
		
		public Expression getPlaceholderExpression() {
			return placeholderExpression;
		}
		
		public Expression getRandomVariableValueExpression() {
			return randomVariableValueExpression;
		}
		
		public Expression getBound() {
			return bound;
		}
		
		public Expression callMessageBoundsOnChildRewriter(Expression timestep) {
			Expression args = Tuple.make(expression, timestep);
			bound = process.rewrite(boundRewriterName, args);
			
			return bound;
		}
	}
}
