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
package com.sri.ai.test.praise.lbp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.DefaultRewriterLookup;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.praise.SimplifyMessagesConvexHull;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.lbp.RandomVariableFromMessageRewriterCall;
import com.sri.ai.praise.lbp.core.ConvexRewriterOnMessageBounds;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.example.TrivialEpidemicAndSickNotbob;
import com.sri.ai.praise.model.example.TrivialEpidemicSickEveryone;
import com.sri.ai.praise.model.example.TrivialEpidemicSickEveryoneNotbobAmaryAjohn;
import com.sri.ai.praise.model.example.TrivialEpidemicSickbob;
import com.sri.ai.praise.model.example.TrivialGaveTreasureToOwnsRich;
import com.sri.ai.praise.model.example.TrivialPQPeoplea1Anda2;
import com.sri.ai.praise.model.example.TrivialPQR;
import com.sri.ai.praise.model.example.TrivialPQWithPriors;
import com.sri.ai.praise.model.example.TrivialPeopleAmericanTallIntelligentUnintelligent;
import com.sri.ai.praise.model.example.TrivialSickSmokerbob;
import com.sri.ai.praise.model.example.TrivialSickbob;
import com.sri.ai.praise.model.example.TrivialSunnyAvailableCanPlayWith;
import com.sri.ai.praise.model.example.WeightedPQWithPriors;
import com.sri.ai.test.praise.AbstractLPITest;
import com.sri.ai.util.Util;

public class BoundsTest extends AbstractLPITest {
	
	private static final int _resultPrecision = ExpressoConfiguration.getDisplayNumericPrecisionForSymbols();;
	
	@Override
	public RewritingProcess newRewritingProcess(Expression rootExpression) {
		return LBPFactory.newBoundLBPProcess(rootExpression);
	}
	
	@Test
	public void testSimplifyMessagesConvexHullUnconditional() {
		TestData[] tests = new TestData[] {
			//
			// Basic simplification 
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({[if p(b) then 0.8 else 0.2], [if p(b) then 600 else 400], [if p(b) then 300 else 700]})",
					false,
					"'convex hull'({[if p(b) then 0.8 else 0.2], [if p(b) then 0.3 else 0.7]})"),
			// Just normalization required
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({[if p(b) then 0.8 else 0.2], [if p(b) then 300 else 700]})",
					false,
					"'convex hull'({[if p(b) then 0.8 else 0.2], [if p(b) then 0.3 else 0.7]})"),
			// Collapsed to a single message
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({[if p(b) then 0.8 else 0.2], [if p(b) then 0.8 else 0.2]})",
					false,
					"if p(b) then 0.8 else 0.2"),
			// Collapsed to a single message due to normalization
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({ [ if p(a) then 0.480000000 else 0 ], [ if p(a) then 0.180000000 else 0 ] })",
					false,
					"if p(a) then 1 else 0"),
			// Edge case that should not occur but will collapse as well to a single message
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({[if p(b) then 0.8 else 0.2]})",
					false,
					"if p(b) then 0.8 else 0.2"),
	        // No Simplification required
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({[if p(b) then 0.8 else 0.2], [if p(b) then 0.3 else 0.7]})",
					false,
					"'convex hull'({[if p(b) then 0.8 else 0.2], [if p(b) then 0.3 else 0.7]})"),
			//
			// Illegal Convex Hulls
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					// No messages.
					"'convex hull'({})",
					true,
					"N/A"),
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					// On random variables
					"'convex hull'({[if p(a) then 0.8 else 0.2], [p(a)]})",
					true,
					"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testSimplifyMessagesConvexHullConditional() {
		TestData[] tests = new TestData[] {
			//
			// Basic
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({if X = a then [if p(X) then 0.8 else 0.2] else [if p(X) then 600 else 400]})",
					false,
					// Collapses to messages
					"if X = a then (if p(a) then 0.8 else 0.2) else (if p(X) then 0.6 else 0.4)"),
			// 
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({if X = a then [if p(X) then 0.8 else 0.2] else [if p(X) then 0.8 else 0.2], " + 
			                       "if X = a then [if p(X) then 0.3 else 0.7] else [if p(X) then 600 else 400]})",
					false,
					"if X = a then 'convex hull'({[if p(a) then 0.8 else 0.2], [if p(a) then 0.3 else 0.7]}) else 'convex hull'({[if p(X) then 0.8 else 0.2], [if p(X) then 0.6 else 0.4]})"),
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({if X = a then [if p(X) then 0.8 else 0.2] else [if p(X) then 0.8 else 0.2], " +
			                       "if X = a then [if p(X) then 0.4 else 0.6] else [if p(X) then 0.7 else 0.3], " +
			                       "if X = a then [if p(X) then 0.3 else 0.7] else [if p(X) then 600 else 400]})",
					false,
					"if X = a then 'convex hull'({[if p(a) then 0.8 else 0.2], [if p(a) then 0.3 else 0.7]}) else 'convex hull'({[if p(X) then 0.8 else 0.2], [if p(X) then 0.6 else 0.4]})"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testSimplifyMessagesConvexHullNumberConstantMessages() {
		TestData[] tests = new TestData[] {
			//
			// Basic
			new SimplifyMessagesConvexHullTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"'convex hull'({[if p(X) then 0.8 else 0.2], [0.5]})",
					false,
					// No change expected
					"'convex hull'({[if p(X) then 0.8 else 0.2], [0.5]})"),
		};
		
		perform(tests);
	}
	
	@Test
	@Ignore
	public void testConvexRewriterOnMessageBoundsBasicInnerUnconditional() {
	
		TestData[] tests = new TestData[] {
			//
			// Basic: No bounds but simplifies down product expression.
			new ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(a)] from [ALL], 1)",
					"[p(a)]",
					new String[][] {
						new String[] {"message to [p(a)] from [ Factor1 ]", "if p(a) then 0.5 else 0.5"},
						new String[] {"message to [p(a)] from [ Factor2 ]", "if p(a) then 0.1 else 0.9"},
						new String[] {"message to [p(a)] from [ Factor3 ]", "if p(a) then 0.6 else 0.4"}
					},
					false,
					"if p(a) then 0.142857143 else 0.857142857"),
			//
			// Basic - simple bounds returned from one of the sub-calls.
			new ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(a)] from [ALL], 1)",
					"[p(a)]",
					new String[][] {
						new String[] {"message to [p(a)] from [ Factor1 ]", "if p(a) then 0.5 else 0.5"},
						new String[] {"message to [p(a)] from [ Factor2 ]", "'convex hull'( { [if p(a) then 0.8 else 0.2], [if p(a) then 0.3 else 0.7] } )"},
						new String[] {"message to [p(a)] from [ Factor3 ]", "if p(a) then 0.6 else 0.4"}
					},
					false,
					// Calculation:
					// #1:          if p(a) then 0.5 else 0.5 * if p(a) then 0.8 else 0.2 * if p(a) then 0.6 else 0.4
					//            = if p(a) then 0.5 * 0.8 * 0.6 else 0.5 * 0.2 * 0.4
					//            = if p(a) then 0.24 else 0.04
					// #2:          if p(a) then 0.5 else 0.5 * if p(a) then 0.3 else 0.7 * if p(a) then 0.6 else 0.4
					//            = if p(a) then 0.5 * 0.3 * 0.6 else 0.5 * 0.7 * 0.4 
					//            = if p(a) then 0.09 else 0.14
					// bound      =
					// 'convex hull'({ ([if p(a) then 0.24 else 0.04]),                 ([if p(a) then 0.09 else 0.14]) })
					// simplified = 
					"  'convex hull'({ ([ if p(a) then 0.857142857 else 0.142857143 ]), ([ if p(a) then 0.391304348 else 0.608695652 ]) })"),
			//
			// Basic - simple bounds returned from two of the sub-calls.
			new ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(a)] from [ALL], 1)",
					"[p(a)]",
					new String[][] {
						new String[] {"message to [p(a)] from [ Factor1 ]", "if p(a) then 0.5 else 0.5"},
						new String[] {"message to [p(a)] from [ Factor2 ]", "'convex hull'( { [if p(a) then 0.8 else 0.2], [if p(a) then 0.3 else 0.7] } )"},
						new String[] {"message to [p(a)] from [ Factor3 ]", "'convex hull'( { [if p(a) then 0.6 else 0.4], [if p(a) then 0.45 else 0.55] } )"}
					},
					false,
					// Calculation:
					// #1:        = if p(a) then 0.5 else 0.5 * if p(a) then 0.8 else 0.2 * if p(a) then 0.6 else 0.4
					//            = if p(a) then 0.5 * 0.8 * 0.6 else 0.5 * 0.2 * 0.4
					//            = if p(a) then 0.24 else 0.04
					// #2:        = if p(a) then 0.5 else 0.5 * if p(a) then 0.8 else 0.2 * if p(a) then 0.45 else 0.55
					//            = if p(a) then 0.5 * 0.8 * 0.45 else 0.5 * 0.2 * 0.55
					//            = if p(a) then 0.18 else 0.055
					// #3:        = if p(a) then 0.5 else 0.5 * if p(a) then 0.3 else 0.7 * if p(a) then 0.6 else 0.4
					//            = if p(a) then 0.5 * 0.3 * 0.6 else 0.5 * 0.7 * 0.4 
					//            = if p(a) then 0.09 else 0.14
					// #4:        = if p(a) then 0.5 else 0.5 * if p(a) then 0.3 else 0.7 * if p(a) then 0.45 else 0.55
					//            = if p(a) then 0.5 * 0.3 * 0.45 else 0.5 * 0.7 * 0.55 
					//            = if p(a) then 0.0675 else 0.1925
					// bound      =
					// 'convex hull'({ ([if p(a) then 0.24 else 0.04]), ([if p(a) then 0.18 else 0.055]), ([if p(a) then 0.09 else 0.14]), ([if p(a) then 0.0675 else 0.1925])})
					// normalized =
					// 'convex hull'({ ([if p(a) then 0.857142857 else 0.142857143]), ([if p(a) then 0.765957447 else 0.234042553]), ([if p(a) then 0.391304348 else 0.608695652]), ([if p(a) then 0.259615385 else 0.740384615])})
					// simplified =
					"  'convex hull'({ ([ if p(a) then 0.857142857 else 0.142857143 ]), ([ if p(a) then 0.259615385 else 0.740384615 ]) })"),
			//
			// Basic - simple conditional bounds returned from one of the sub-calls
			new ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(X)] from [ALL], 1)",
					"[p(X)]",
					new String[][] {
						new String[] {"message to [p(X)] from [ Factor1 ]", "if p(X) then 0.5 else 0.5"},
						new String[] {"message to [p(X)] from [ Factor2 ]", "if X = a then 'convex hull'( { [if p(X) then 0.8 else 0.2], [if p(X) then 0.3 else 0.7] } ) else 'convex hull'( { [if p(X) then 0.95 else 0.05], [if p(X) then 0.89 else 0.11] } )"},
						new String[] {"message to [p(X)] from [ Factor3 ]", "if p(X) then 0.6 else 0.4"}
					},
					false,
					// Calculation:
					// Condition: if X = a
					// #a.1:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.8 else 0.2 * if p(X) then 0.6 else 0.4
					//            = if p(X) then 0.5 * 0.8 * 0.6 else 0.5 * 0.2 * 0.4
					//            = if p(X) then 0.24 else 0.04
					// #a.2:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.3 else 0.7 * if p(X) then 0.6 else 0.4
					//            = if p(X) then 0.5 * 0.3 * 0.6 else 0.5 * 0.7 * 0.4 
					//            = if p(X) then 0.09 else 0.14
					// bound      =
					// 'convex hull'({ ([if p(a) then 0.24 else 0.04]),                 ([if p(a) then 0.09 else 0.14]) })
					// simplified =
					// 'convex hull'({ ([ if p(a) then 0.857142857 else 0.142857143 ]), ([ if p(a) then 0.391304348 else 0.608695652 ]) })
					//
					// Condition: else
					// #e.1:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.95 else 0.05 * if p(X) then 0.6 else 0.4
					//            = if p(X) then 0.5 * 0.95 * 0.6 else 0.5 * 0.05 * 0.4
					//            = if p(X) then 0.285 else 0.01 
					// #e.2:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.89 else 0.11 * if p(X) then 0.6 else 0.4
					//            = if p(X) then 0.5 * 0.89 * 0.6 else 0.5 * 0.11 *0.4
					//            = if p(X) then 0.267 else 0.022
					// bound      =
					// 'convex hull'({ ([if p(X) then 0.285 else 0.01]),                 ([if p(X) then 0.267 else 0.022]) })
					// simplified = 
					// 'convex hull'({ ([ if p(X) then 0.966101695 else 0.0338983051 ]), ([ if p(X) then 0.923875433 else 0.0761245675 ]) })
					//
					// conditioned=
					"if X = a then 'convex hull'({ ([ if p(a) then 0.857142857 else 0.142857143 ]), ([ if p(a) then 0.391304348 else 0.608695652 ]) }) else 'convex hull'({ ([ if p(X) then 0.966101695 else 0.0338983051 ]), ([ if p(X) then 0.923875433 else 0.0761245675 ]) })"),
			//
			// Basic - conditional bounds (with the same condition) returned from two of the sub-calls
			new ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(X)] from [ALL], 1)",
					"[p(X)]",
					new String[][] {
						new String[] {"message to [p(X)] from [ Factor1 ]", "if p(X) then 0.5 else 0.5"},
						new String[] {"message to [p(X)] from [ Factor2 ]", "if X = a then 'convex hull'( { [if p(X) then 0.8 else 0.2], [if p(X) then 0.3 else 0.7] } ) else 'convex hull'( { [if p(X) then 0.95 else 0.05], [if p(X) then 0.89 else 0.11] } )"},
						new String[] {"message to [p(X)] from [ Factor3 ]", "if X = a then 'convex hull'( { [if p(X) then 0.6 else 0.4], [if p(X) then 0.45 else 0.55] } ) else 'convex hull'( { [if p(X) then 0.65 else 0.35], [if p(X) then 0.78 else 0.22] } )"}
					},
					false,
					// Calculation:
					// Condition: if X = a
					// #a.1:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.8 else 0.2 * if p(X) then 0.6 else 0.4
					//            = if p(X) then 0.5 * 0.8 * 0.6 else 0.5 * 0.2 * 0.4
					//            = if p(X) then 0.24 else 0.04
					// #a.2:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.8 else 0.2 * if p(X) then 0.45 else 0.55
					//            = if p(X) then 0.5 * 0.8 * 0.45 else 0.5 * 0.2 * 0.55
					//            = if p(X) then 0.18 else 0.055
					// #a.3:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.3 else 0.7 * if p(X) then 0.6 else 0.4
					//            = if p(X) then 0.5 * 0.3 * 0.6 else 0.5 * 0.7 * 0.4 
					//            = if p(X) then 0.09 else 0.14
					// #a.4:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.3 else 0.7 * if p(X) then 0.45 else 0.55
					//            = if p(X) then 0.5 * 0.3 * 0.45 else 0.5 * 0.7 * 0.55
					//            = if p(X) then 0.0675 else 0.1925
					// bound      =
					// 'convex hull'({ ([if p(X) then 0.24 else 0.04]), ([if p(X) then 0.18 else 0.055]), ([if p(X) then 0.09 else 0.14]), ([if p(X) then 0.0675 else 0.1925]) })
					// normalized =
					// 'convex hull'({ ([if p(X) then 0.857142857 else 0.142857143]), ([if p(X) then 0.765957447 else 0.234042553]), ([if p(X) then 0.391304348 else 0.608695652]), ([if p(X) then 0.259615385 else 0.740384615]) })
					// simplified =
					// 'convex hull'({ ([ if p(a) then 0.857142857 else 0.142857143 ]), ([ if p(a) then 0.259615385 else 0.740384615 ]) })
					//
					// Condition: else
					// #e.1:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.95 else 0.05 * if p(X) then 0.65 else 0.35
					//            = if p(X) then 0.5 * 0.95 * 0.65 else 0.5 * 0.05 * 0.35
					//            = if p(X) then 0.30875 else 0.00875
					// #e.2:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.95 else 0.05 * if p(X) then 0.78 else 0.22
					//            = if p(X) then 0.5 * 0.95 * 0.78 else 0.5 * 0.05 * 0.22
					//            = if p(X) then 0.3705 else 0.0055
					// #e.3:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.89 else 0.11 * if p(X) then 0.65 else 0.35
					//            = if p(X) then 0.5 * 0.89 * 0.65 else 0.5 * 0.11 * 0.35
					//            = if p(X) then 0.28925 else 0.01925
					// #e.4:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.89 else 0.11 * if p(X) then 0.78 else 0.22
					//            = if p(X) then 0.5 * 0.89 * 0.78 else 0.5 * 0.11 * 0.22
					//            = if p(X) then 0.3471 else 0.0121
					// bound      =
					// 'convex hull'({ ([if p(X) then 0.30875 else 0.00875]), ([if p(X) then 0.3705 else 0.0055]), ([if p(X) then 0.28925 else 0.01925]), ([if p(X) then 0.3471 else 0.0121]) })
					// normalized =
					// 'convex hull'({ ([if p(X) then 0.972440945 else 0.027559055]), ([if p(X) then 0.985372340 else 0.0146276596]), ([if p(X) then 0.937601297 else 0.0623987034]), ([if p(X) then 0.966314031 else 0.033685969]) })
					// simplified =
					// 'convex hull'({ ([ if p(X) then 0.985372340 else 0.0146276596 ]), ([ if p(X) then 0.937601297 else 0.0623987034 ]) })
					//
					// conditioned=
					"if X = a then 'convex hull'({ ([ if p(a) then 0.857142857 else 0.142857143 ]), ([ if p(a) then 0.259615385 else 0.740384615 ]) }) else 'convex hull'({ ([ if p(X) then 0.985372340 else 0.0146276596 ]), ([ if p(X) then 0.937601297 else 0.0623987034 ]) })"),
			//
			// Basic - conditional bounds (with different conditions) returned from two of the sub-calls
			new ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(X)] from [ALL], 1)",
					"[p(X)]",
					new String[][] {
						new String[] {"message to [p(X)] from [ Factor1 ]", "if p(X) then 0.5 else 0.5"},
						new String[] {"message to [p(X)] from [ Factor2 ]", "if X = a then 'convex hull'( { [if p(X) then 0.8 else 0.2], [if p(X) then 0.3 else 0.7] } ) else 'convex hull'( { [if p(X) then 0.95 else 0.05], [if p(X) then 0.89 else 0.11] } )"},
						new String[] {"message to [p(X)] from [ Factor3 ]", "if X = b then 'convex hull'( { [if p(X) then 0.6 else 0.4], [if p(X) then 0.45 else 0.55] } ) else 'convex hull'( { [if p(X) then 0.65 else 0.35], [if p(X) then 0.78 else 0.22] } )"}
					},
					false,
					// Calculation:
					// Condition: if X = a
					// #a.1:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.8 else 0.2 * if p(X) then 0.65 else 0.35
					//            = if p(X) then 0.5 * 0.8 * 0.65 else 0.5 * 0.2 * 0.35
					//            = if p(X) then 0.26 else 0.035
					// #a.2:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.8 else 0.2 * if p(X) then 0.78 else 0.22
					//            = if p(X) then 0.5 * 0.8 * 0.78 else 0.5 * 0.2 * 0.22
					//            = if p(X) then 0.312 else 0.022
					// #a.3:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.3 else 0.7 * if p(X) then 0.65 else 0.35
					//            = if p(X) then 0.5 * 0.3 * 0.65 else 0.5 * 0.7 * 0.35
					//            = if p(X) then 0.0975 else 0.1225
					// #a.4:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.3 else 0.7 * if p(X) then 0.78 else 0.22
					//            = if p(X) then 0.5 * 0.3 * 0.78 else 0.5 * 0.7 * 0.22
					// bound      = if p(X) then 0.117 else  0.077
					// 'convex hull'({ ([if p(X) then 0.26 else 0.035]), ([if p(X) then 0.312 else 0.022]), ([if p(X) then 0.0975 else 0.1225]), ([if p(X) then 0.117 else  0.077]) })
					// normalized =
					// 'convex hull'({ ([if p(X) then 0.881355932 else 0.118644068]), ([if p(X) then 0.934131737 else 0.0658682635]), ([if p(X) then 0.443181818 else 0.556818182]), ([if p(X) then 0.603092784 else 0.396907216]) })
					// simplified =
					// 'convex hull'({ ([ if p(a) then 0.934131737 else 0.0658682635 ]), ([ if p(a) then 0.443181818 else 0.556818182 ]) })
					//
					// Condition: else if X = b
					// #b.1:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.95 else 0.05 * if p(X) then 0.6 else 0.4
					//            = if p(X) then 0.5 * 0.95 * 0.6 else 0.5 * 0.05 * 0.4
					//            = if p(X) then 0.285 else 0.01
					// #b.2:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.95 else 0.05 * if p(X) then 0.45 else 0.55
					//            = if p(X) then 0.5 *0.95 * 0.45  else 0.5 * 0.05 * 0.55
					//            = if p(X) then 0.21375 else 0.01375
					// #b.3:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.89 else 0.11 * if p(X) then 0.6 else 0.4
					//            = if p(X) then 0.5 * 0.89 * 0.6 else 0.5 * 0.11 * 0.4
					//            = if p(X) then 0.267 else 0.022
					// #b.4:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.89 else 0.11 * if p(X) then 0.45 else 0.55
					//            = if p(X) then 0.5 * 0.89 * 0.45 else 0.5 * 0.11 * 0.55
					//            = if p(X) then 0.20025 else 0.03025
					// bound      = 
					// 'convex hull'({ ([if p(X) then 0.285 else 0.01]), ([if p(X) then 0.21375 else 0.01375]), ([if p(X) then 0.267 else 0.022]), ([if p(X) then 0.20025 else 0.03025]) })
					// normalized =
					// 'convex hull'({ ([if p(X) then 0.966101695 else 0.0338983051]), ([if p(X) then 0.93956044 else 0.0604395604]), ([if p(X) then 0.923875432 else 0.0761245675]), ([if p(X) then 0.868763557 else 0.131236443]) })
					// simplified =
					// 'convex hull'({ ([ if p(b) then 0.966101695 else 0.0338983051 ]), ([ if p(b) then 0.868763557 else 0.131236443 ]) }) 
					//
					// Condition: else
					// #e.1:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.95 else 0.05 * if p(X) then 0.65 else 0.35
					//            = if p(X) then 0.5 * 0.95 * 0.65 else 0.5 * 0.05 * 0.35
					//            = if p(X) then 0.30875 else 0.00875
					// #e.2:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.95 else 0.05 * if p(X) then 0.78 else 0.22
					//            = if p(X) then 0.5 * 0.95 * 0.78 else 0.5 * 0.05 * 0.22
					//            = if p(X) then 0.3705 else 0.0055
					// #e.3:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.89 else 0.11 * if p(X) then 0.65 else 0.35
					//            = if p(X) then 0.5 * 0.89 * 0.65 else 0.5 * 0.11 * 0.35
					//            = if p(X) then 0.28925 else 0.01925
					// #e.4:      = if p(X) then 0.5 else 0.5 * if p(X) then 0.89 else 0.11 * if p(X) then 0.78 else 0.22
					//            = if p(X) then 0.5 * 0.89 * 0.78 else 0.5 * 0.11 * 0.22
					//            = if p(X) then 0.3471 else 0.0121
					// bound      = 
					// 'convex hull'({ ([if p(X) then 0.30875 else 0.00875]), ([if p(X) then 0.3705 else 0.0055]), ([if p(X) then 0.28925 else 0.01925]), ([if p(X) then 0.3471 else 0.0121]) })
					// normalized =
					// 'convex hull'({ ([if p(X) then 0.972440945 else 0.0275590551]), ([if p(X) then 0.985372340 else 0.0146276596]), ([if p(X) then 0.937601297 else 0.0623987034]), ([if p(X) then 0.966314031 else 0.0336859688]) })
					// simplified =
					// 'convex hull'({ ([ if p(X) then 0.985372340 else 0.0146276596 ]), ([ if p(X) then 0.937601297 else 0.0623987034 ]) })
					//
					// conditioned= 
					"if X = a then 'convex hull'({ ([ if p(a) then 0.934131737 else 0.0658682635 ]), ([ if p(a) then 0.443181818 else 0.556818182 ]) }) else (if X = b then 'convex hull'({ ([ if p(b) then 0.966101695 else 0.0338983051 ]), ([ if p(b) then 0.868763557 else 0.131236443 ]) }) else 'convex hull'({ ([ if p(X) then 0.985372340 else 0.0146276596 ]), ([ if p(X) then 0.937601297 else 0.0623987034 ]) }))"),
			//
			// Illegal - delayed evaluation of messages not currently supported
			new ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(X)] from [ALL], 1)",
					"[p(X)]",
					new String[][] {
						new String[] {"message to [p(X)] from [ Factor1 ]", "if p(X) then 0.5 else 0.5"},
						new String[] {"message to [p(X)] from [ Factor2 ]", "(lambda p(X) : (previous message to [ if p(X) or q(Y) then 1 else 0 ] from [ p(X) ]))(p(X))"},
						new String[] {"message to [p(X)] from [ Factor3 ]", "'convex hull'( { [if p(a) then 0.8 else 0.2], [if p(a) then 0.3 else 0.7] } )"}
					},
					true,
					"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	@Ignore
	// Currently ignored because comparison of expected and actual output fails.
	// They actually look identical as strings:
	// if X = a then 'convex hull'({ ([ if p(a) then 0.857142857 else 0.142857143 ]), ([ if p(a) then 0.391304348 else 0.608695652 ]) }) else (if p(X) then 0.35 else 0.65)
	// However, the Rational numbers have distinct internal representations that make the comparison fail.
	// For example, 0.857142857 in the actual output has internal representation 6/7 while the expected output has internal representations 857142857/1000000000.
	// One problem in here is that increasing the precision (even to 100) does not change the output string to the correct value of 6/7 = 0.8571428571428571
	//
	// This was working before, when the rounding method was based on syntax trees rather than expressions;
	// see rounding method call in ConvexRewriterOnMessageBoundsInnerConditionalProductTestData (search for "remnant" in this file to find it quickly)
	// to revert to working version.
	// I don't understand why it was working, though, as it would seem the same problem with the internal representation would remain.
	// My guess is that the syntax tree version somehow re-used an object instance and this allowed simple instance comparison to result in true.
	// The solution must be a way to describe the expected output in such a way that it will result equal with actual output.
	// However this is tricky here because the actual results involve periodic decimal expansions so we cannot write an expected result as a decimal.
	// We either need to change the test to something returning a non-periodic decimal expansion, or find a way to specify rational number constants
	// in fractional form (simply writing a division is not enough since the expression comparison of 6/7 and a symbol with that rational number will be false).
	// I wanted to change the test to return simpler numbers but I realized, after reading the comments after the test that it seems the actual results
	// are not following what is described in the comments.
	// Therefore I decided to ignore the test for now for further later investigation.
	// See aic-expresso issue #44 and aic-praise issue #32.
	public void testConvexRewriterOnMessageBoundsBasicInnerConditional() {	
		TestData[] tests = new TestData[] {
			// Test for:
			// ALBP-233 - ConvexRewriterOnMessageBounds to substitute sub-call bounds results with unique identifier
			// to avoid possible simplification issue
			new ConvexRewriterOnMessageBoundsInnerConditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(X)] from [ALL], 1)",
					"[p(X)]",
					new String[][] {
						new String[] {"message to [p(X)] from [ Factor1 ]", "'convex hull'( { [if p(X) then 0.8 else 0.2], [if p(X) then 0.3 else 0.7] } )"},
						new String[] {"message to [p(X)] from [ Factor2 ]", "if p(X) then 0.35 else 0.65"},
						new String[] {"message to [p(X)] from [ Factor3 ]", "if p(X) then 0.6 else 0.4"}
					},
					false,
					// Calculation:
					// Condition: if X = a
					// #1:          if p(a) then 0.8 else 0.2 * if p(a) then 0.6 else 0.4
					//            = if p(a) then 0.8 * 0.6 else 0.2 * 0.4
					//            = if p(a) then 0.48 else 0.08
					// #2:          if p(a) then 0.3 else 0.7 * if p(a) then 0.6 else 0.4
					//            = if p(a) then 0.3 * 0.6 else 0.7 * 0.4 
					//            = if p(a) then 0.18 else 0.28
					// bound      =
					// 'convex hull'({ ([if p(a) then 0.48 else 0.08]), ([if p(a) then 0.18 else 0.28]) })
					// Condition: else
					// #1           if p(a) then 0.35 else 0.65 
					// simplified = 
					"if X = a then 'convex hull'({ ([ if p(a) then 0.8571428571428571 else 0.1428571428571429 ]), ([ if p(a) then 0.391304347826087 else 0.608695652173913 ]) }) else (if p(X) then 0.35 else 0.65)"),
			//
			// Basic: an inner conditionals with child conditionals.
			new ConvexRewriterOnMessageBoundsInnerConditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(X)] from [ALL], 1)",
					"[p(X)]",
					new String[][] {
						new String[] {"message to [p(X)] from [ Factor1 ]", "'convex hull'( { [if p(X) then 0.8 else 0.2], [if p(X) then 0.3 else 0.7] } )"},
						new String[] {"message to [p(X)] from [ Factor3 ]", "if X = b then 'convex hull'( { [if p(X) then 0.6 else 0.4], [if p(X) then 0.45 else 0.55] } ) else 'convex hull'( { [if p(X) then 0.65 else 0.35], [if p(X) then 0.78 else 0.22] } )"},
						new String[] {"message to [p(X)] from [ Factor2 ]", "if p(X) then 0.35 else 0.65"}
					},
					false,
					// Calculation:
					// Condition: if X = a
					// #1:          if p(a) then 0.8 else 0.2 * if p(X) then 0.35 else 0.65
					//            = if p(a) then 0.8 * 0.35 else 0.2 * 0.65
					//            = if p(a) then 0.28 else 0.13 
					// #2:          if p(a) then 0.3 else 0.7 * if p(X) then 0.35 else 0.65
					//            = if p(a) then 0.3 * 0.35 else 0.7 * 0.65 
					//            = if p(a) then 0.105 else 0.455 
					// bound      =
					// 'convex hull'({ ([if p(a) then 0.28 else 0.13]), ([if p(a) then 0.105 else 0.455]) })
					// Condition: else
					// #1         = if X = b then (if p(X) then 0.6 else 0.4)   else (if p(X) then 0.65 else 0.35)
					// #2         = if X = b then (if p(X) then 0.45 else 0.55) else (if p(X) then 0.65 else 0.35)
					// #3         = if X = b then (if p(X) then 0.6 else 0.4)   else (if p(X) then 0.78 else 0.22)
					// #4         = if X = b then (if p(X) then 0.45 else 0.55) else (if p(X) then 0.78 else 0.22)
					// simplified = 
					"if X = a then 'convex hull'({ ([ if p(a) then 0.682926829 else 0.317073171 ]), ([ if p(a) then 0.187500000 else 0.812500000 ]) }) else (if X = b then 'convex hull'({ ([ if p(b) then 0.600000000 else 0.400000000 ]), ([ if p(b) then 0.450000000 else 0.550000000 ]) }) else 'convex hull'({ ([ if p(X) then 0.650000000 else 0.350000000 ]), ([ if p(X) then 0.780000000 else 0.220000000 ]) }))"),

		};
		
		perform(tests);
	}
	
	@Test
	public void testConvexRewriterOnMessageBoundsOnMessageValuesThatSimplifyToNumbers() {
	
		TestData[] tests = new TestData[] {
			//
			// Basic: Trivial bounds returned by all calls.
			new ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(a)] from [ALL], 1)",
					"[p(a)]",
					new String[][] {
						new String[] {"message to [p(a)] from [ Factor1 ]", "'convex hull'( { [if p(a) then 1 else 0], [if p(a) then 0 else 1] } )"},
						new String[] {"message to [p(a)] from [ Factor3 ]", "'convex hull'( { [if p(a) then 1 else 0], [if p(a) then 0 else 1] } )"}
					},
					false,
					"  'convex hull'({ [if p(a) then 1 else 0], [if p(a) then 0 else 1] })"),
		};
		
		perform(tests);
	}
	
	// Note: This test simulates a real case that can occur when calling
	// R_prod_m_and_prod_factor in an anytime situation. On the first call
	// two child rewrite calls are made but only the result of the last
	// call is returned, which will be a non-conditional bounds on the first
	// call (the first child call is actually an argument to the second
	// child call and is not part of the result returned).
	@Test
	public void testConvexRewriterOnMessageBoundsInnerRewriterReturnsOnlyBoundFromLastChildCall() {
		class ConvexRewriterOnMessageBoundsInnerRewriterReturnsNonConditionalBoundsTestData extends TestData {
			private String         strInputExpr;
			private String[][]     strArgResponseExpr;
			private Expression     inputExpr;
			private Expression[][] argResponseExpr;
			
			public ConvexRewriterOnMessageBoundsInnerRewriterReturnsNonConditionalBoundsTestData(String contextualConstraint, Model model, String strInputExpr, String[][] strArgResponseExpr, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.strInputExpr       = strInputExpr;
				this.strArgResponseExpr = strArgResponseExpr;
			};
			
			@Override
			public Expression getTopExpression() {
				this.inputExpr       = parse(strInputExpr);
				this.argResponseExpr = new Expression[strArgResponseExpr.length][];
				for (int i = 0; i < strArgResponseExpr.length; i++) {
					Expression[] argResponse = new Expression[] {
						parse(strArgResponseExpr[i][0]),
						parse(strArgResponseExpr[i][1])
					};
					this.argResponseExpr[i] = argResponse;
				}
				
				return this.inputExpr;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				final DefaultRewriterLookup rewriterLookup = (DefaultRewriterLookup) ((DefaultRewritingProcess)process).getRewriterLookup();
				Rewriter normalize = LBPFactory.newNormalize();
				Rewriter inner    = new AbstractRewriter() {
					
					@Override
					public String getName() {
						return "R_inner";
					}
					
					@Override
					public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
						Expression lastChildResult = null;
						for (int i = 0; i < argResponseExpr.length; i++) {
							Expression arg         = argResponseExpr[i][0];
							Expression response    = argResponseExpr[i][1];
							
							// Note: We woulnd't do this in practice but as I want
							// to test a specific return value I'll update the
							// R_canned_response on the handler to what I want.
							Rewriter childRewriter = new CannedRewriter(response);
							rewriterLookup.put("R_bound_canned_response", childRewriter);
							
							lastChildResult = process.rewrite("R_canned_response", arg);
						}
						
						// Note: This simulates what can happen when calling R_prod_m_and_prod_factor
						Expression result = lastChildResult;
						return result;
					}
					
					@Override
					protected boolean isTraceInAndOutOfRewriter() {
						return true;
					}
				};
				
				rewriterLookup.put("R_normalize", normalize);
				rewriterLookup.put("R_inner",    inner);				
				Map<String, String> boundChildRewriters = new LinkedHashMap<String, String>();
				boundChildRewriters.put("R_canned_response", "R_bound_canned_response");
				ConvexRewriterOnMessageBounds boundsWrapper = new ConvexRewriterOnMessageBounds("R_message_bounds",
						"R_inner", new MockRandomVariableFromMessageRewriterCall(parse("[p(X)]")), boundChildRewriters);
				
				Expression result = boundsWrapper.rewrite(this.inputExpr, process);
				
				result = Expressions.roundToAGivenPrecision(result, _resultPrecision, process);
				
				return result;
			}
		};
		
		TestData[] tests = new TestData[] {
			new ConvexRewriterOnMessageBoundsInnerRewriterReturnsNonConditionalBoundsTestData(Expressions.TRUE.toString(), new TrivialPQR(),
					"(message to [p(a)] from [ALL], 1)",
					new String[][] {
						new String[] {"message to [p(X)] from [ Factor1 ]", "'convex hull'( { [if p(X) then 0.8 else 0.2], [if p(X) then 0.3 else 0.7] } )"},
						new String[] {"message to [p(X)] from [ Factor2 ]", "'convex hull'( { [if p(X) then 0.6 else 0.4], [if p(X) then 0.35 else 0.65] } )"},
					},
					false,
					"'convex hull'( { [if p(X) then 0.6 else 0.4], [if p(X) then 0.35 else 0.65] } )"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testBoundBeliefForNonLoopyModels() {
		class BoundBeliefTestData extends TestData {
			private String belief; 
			private Expression exprBelief;
			private Map<Object, Object> globalObjects;
			
			public BoundBeliefTestData(String contextualConstraint, Model model, String belief, boolean illegalArgumentTest, String expected) {
				this(contextualConstraint, model, belief, null, illegalArgumentTest, expected);
			};
			
			public BoundBeliefTestData(String contextualConstraint, Model model, String belief, Map<Object, Object> globalObjects, boolean illegalArgumentTest, String expected) {
				super(contextualConstraint, model, illegalArgumentTest, expected);
				this.belief = belief;
				this.globalObjects = globalObjects;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprBelief = parse(belief);
				return this.exprBelief;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				if (globalObjects != null) {
					process.getGlobalObjects().putAll(globalObjects);
				}
				Expression belief = process.rewrite(LBPRewriter.R_bound_belief, exprBelief);
				Expression roundedBelief = Expressions.roundToAGivenPrecision(belief, 9, process);
				return roundedBelief;
			}
		};
		
		//
		// A Sample of tests from LBPTest.testBeliefForNonLoopyModels()
		BoundBeliefTestData[] tests = new BoundBeliefTestData[] {
				new BoundBeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"union("
										+ "{{(on X in People) [if sick(X) then 0.4 else 0.6]}}, "
										+ "{{ [if sick(john) then 1 else 0] }}"
										+ ")",
								"sick/1"
						),
						"(belief([sick(X)]), 1)",
						false,
						"if X = john then if sick(john) then 1 else 0 else if sick(X) then 0.4 else 0.6"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"union("
										+ "{ [if epidemic then 0.1 else 0.9] }, "
										+ "{{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}, "
										+ "{{ (on X in People) [if sick(X) then 1 else 0] | X  = person1 or  X  = person2 or  X  = person3 }},"
										+ "{{ (on X in People) [if sick(X) then 0 else 1] | X != person1 and X != person2 and X != person3 }}"
										+ ")",
								"epidemic/0", "sick/1"
						),
						"(belief([epidemic]), 1)",
						// Util.map(parse("|People|"), Expressions.createSymbol(20)),
						false,
						// Note: old R_basic result:
						// "if epidemic then (0.0064 * 0.6 ^ (| People | - 3)) / (0.0064 * 0.6 ^ (| People | - 3) + 9E-7 * 0.99 ^ (| People | - 3)) else (9E-7 * 0.99 ^ (| People | - 3)) / (0.0064 * 0.6 ^ (| People | - 3) + 9E-7 * 0.99 ^ (| People | - 3))"
						"if epidemic then 0.995339619 else 0.00466038114"),
	
				new BoundBeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"union("
										+ "{ [if epidemic then 0.1 else 0.9] }, "
										+ "{{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}, "
										+ "{{ (on X in People) [if sick(X) then 1 else 0] | X  = person1 or  X  = person2 or  X  = person3 }},"
										+ "{{ (on X in People) [if sick(X) then 0 else 1] | X != person1 and X != person2 and X != person3 }}" +
										")",
								"epidemic/0", "sick/1"
						),
						"(belief([epidemic]), 1)",
						Util.map(parse("| People |"), Expressions.makeSymbol(20)),
						false, 
						"if epidemic then 0.588128460 else 0.411871540"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPQWithPriors(), 
						"(belief([p(X)]), 1)", 
						false, 
						"if p(X) then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPQWithPriors(), 
						"(belief([q(X)]), 1)", 
						false, 
						"if q(X) then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), 
						new WeightedPQWithPriors(), 
						"(belief([p(X)]), 1)", 
						false, 
						"if p(X) then 0.223300971 else 0.776699029"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new WeightedPQWithPriors(), 
						"(belief([q(X)]), 1)", 
						false, 
						"if q(X) then 0.320388350 else 0.679611650"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
						"(belief([ q(a1) ]), 1)", 
						false, 
						"if q(a1) then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
						"(belief([ q(a2) ]), 1)", 
						false, 
						"if q(a2) then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
						"(belief([ q(a3) ]), 1)", 
						false,				
						"if q(a3) then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), 
						new TrivialPQPeoplea1Anda2(), 
						"(belief([ q(X) ]), 1)", 
						false, 
						// Note: old R_basic result:
						// "if X = a1 then if q(a1) then 1 else 0 else if X = a2 then if q(a2) then 1 else 0 else if q(X) then 1 else 0"
						"if X = a1 then if q(a1) then 1 else 0 else if X = a2 then if q(a2) then 1 else 0 else if q(X) then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
						"(belief([ p ]), 1)", 
						false, 
						"if p then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"(belief([tall(X)]), 1)", 
						false, 
						"0.5"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"(belief([tall(a1)]), 1)", 
						false, 
						"0.5"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"(belief([american(X)]), 1)", 
						false, 
						"if american(X) then 0.687500000 else 0.312500000"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"(belief([american(a1)]), 1)", 
						false, 
						"if american(a1) then 0.687500000 else 0.312500000"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"(belief([intelligent(X)]), 1)", 
						false, 
						"0.5"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"(belief([intelligent(a1)]), 1)", 
						false, 
						"0.5"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"(belief([unintelligent(X)]), 1)", 
						false, 
						"0.5"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
						"(belief([unintelligent(a1)]), 1)", 
						false, 
						"0.5"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"(belief([ epidemic ]), 1)", 
						false, 
						// Note: old R_basic result:
						// "if epidemic then 1 / (1 + 0.8 ^ (|People| - 1)) else 0.8 ^ (|People| - 1) / (1 + 0.8 ^ (|People| - 1))"
						"if epidemic then 0.881664935 else 0.118335065"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"(belief([ sick(X) ]), 1)", 
						false, 
						// Note: old R_formula_simplification result before R_normalize used instead
						// Difference is because | People | -> 10 and new result is this expression calculated correctly with that.
						// "if X != bob then if sick(X) then (0.4 * 0.8 ^ (|People| - 2) + 0.6) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else (0.4 * 0.8 ^ (|People| - 2) + 0.4) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else 0.5"
						"if X = bob then 0.500000000 else (if sick(X) then 0.588166494 else 0.411833506)"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"(belief([ sick(ann) ]), 1)", 
						false, 
						// Note: old R_basic result:
						// "if sick(ann) then (0.4 * 0.8 ^ (|People| - 2) + 0.6) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else (0.4 * 0.8 ^ (|People| - 2) + 0.4) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2))"
						"if sick(ann) then 0.588166494 else 0.411833506"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
						"(belief([ sick(bob) ]), 1)", 
						false, 
						"0.5"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich() , 
						"(belief([rich(bob)]), 1)", 
						false,
						// Note: old R_basic result:
						// "if rich(bob) then (1 + 2 ^ |People|) ^ |Treasure| / ((1 + 2 ^ |People|) ^ |Treasure| + 1) else 1 / ((1 + 2 ^ |People|) ^ |Treasure| + 1)"
						"if rich(bob) then 1 else 0.000000000000000000000000000000781198402"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
						"(belief([rich(X)]), 1)", 
						false,
						// Note: old R_basic result
						// "if rich(X) then (1 + 2 ^ |People|) ^ |Treasure| / ((1 + 2 ^ |People|) ^ |Treasure| + 1) else 1 / ((1 + 2 ^ |People|) ^ |Treasure| + 1)"
						"if rich(X) then 1 else 0.000000000000000000000000000000781198402"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
						"(belief([gaveTreasureTo(X,Z,Y)]), 1)", 
						false, 
						// Note: old R_basic and R_formula_simlification result:
						// "if |People| > 0 then if gaveTreasureTo(X, Z, Y) then (2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else if gaveTreasureTo(X, Z, Y) then (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1))"
						"if gaveTreasureTo(X, Z, Y) then 0.499512195 else 0.500487805"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
						"(belief([owns(X,Y)]), 1)", 
						false,
						// Note: old R_basic and R_formula_simlification result:
						//"if owns(X, Y) then (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1))"
						"if owns(X, Y) then 0.999024390 else 0.000975609756"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialSickbob(), 
						"(belief([sick(Person)]), 1)", 
						false, 
						"if Person = bob then if sick(bob) then 0.8 else 0.2 else 0.5"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialSickbob(), 
						"(belief([sick(bob)]), 1)", 
						false, 
						"if sick(bob) then 0.8 else 0.2"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialSickbob(), 
						"(belief([sick(ann)]), 1)", 
						false, 
						"0.5"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
						"(belief([smoker(Person)]), 1)", 
						false, 
						// Note: old R_basic result:
						// "if Person = bob then if smoker(bob) then 0.15 else 0.85 else if smoker(Person) then 0.1 else 0.9"
						"if Person = bob then if smoker(bob) then 0.150943396 else 0.849056604 else if smoker(Person) then 0.100000000 else 0.900000000"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
						"(belief([smoker(bob)]), 1)", 
						false, 
						"if smoker(bob) then 0.150943396 else 0.849056604"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
						"(belief([sick(X)]), 1)", 
						false, 
						// Note: old R_basic result:
						// "if X = bob then if sick(bob) then 1 else 0 else if sick(X) then 0.53 else 0.47"
						// i.e. no constraint applier used, so sick(bob) is sick(X)
						"if X = bob then if sick(bob) then 1 else 0 else if sick(X) then 0.530000000 else 0.470000000"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
						"(belief([sick(bob)]), 1)", 
						false, 
						"if sick(bob) then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
						"(belief([ epidemic ]), 1)", 
						false, 
				      "if epidemic then 0.28 else 0.72"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
						"(belief([ sick(X) ]), 1)", 
						false,
						"if X = bob then if sick(bob) then 1 else 0 else if sick(X) then 0.34 else 0.66"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
						"(belief([ sick(ann) ]), 1)", 
						false,
						"if sick(ann) then 0.34 else 0.66"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
						"(belief([ sick(bob) ]), 1)", 
						false, 
						"if sick(bob) then 1 else 0"),
								
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
						"(belief([ epidemic ]), 1)", 
						false, 
						// Note: old R_basic result:
						// "if epidemic then (0.1 * 0.7 ^ |People|) / (0.1 * 0.7 ^ |People| + 0.9 * 0.2 ^ |People|) else (0.9 * 0.2 ^ |People|) / (0.1 * 0.7 ^ |People| + 0.9 * 0.2 ^ |People|)"
						"if epidemic then 0.999967375 else 0.0000326248029"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
						"(belief([ sick(X) ]), 1)", 
						false, 
						"if sick(X) then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
						"(belief([ sick(ann) ]), 1)", 
						false, 
						"if sick(ann) then 1 else 0"),
				
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"(belief([ epidemic ]), 1)", 
						false,
						// Note: old R_basic result:
						// "if epidemic then (0.03 * 0.7 ^ (|People| - 3)) / (0.03 * 0.7 ^ (|People| - 3) + 0.72 * 0.2 ^ (|People| - 3)) else (0.72 * 0.2 ^ (|People| - 3)) / (0.03 * 0.7 ^ (|People| - 3) + 0.72 * 0.2 ^ (|People| - 3))"
						"if epidemic then 0.996283639 else 0.00371636130"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"(belief([ sick(X) ]), 1)", 
						false,
						// Note: old R_basic result:
						// "if X != bob and X != mary and X != john then if sick(X) then 1 else 0 else if X = bob then if sick(bob) then 0 else 1 else if sick(X) then (0.14 * 0.20 ^ (| People | - 3) + 0.021 * 0.70 ^ (| People | - 3)) / (0.14 * 0.20 ^ (| People | - 3) + 0.021 * 0.70 ^ (| People | - 3) + 0.58 * 0.20 ^ (| People | - 3) + 0.0090 * 0.70 ^ (| People | - 3)) else (0.58 * 0.20 ^ (| People | - 3) + 0.0090 * 0.70 ^ (| People | - 3)) / (0.14 * 0.20 ^ (| People | - 3) + 0.021 * 0.70 ^ (| People | - 3) + 0.58 * 0.20 ^ (| People | - 3) + 0.0090 * 0.70 ^ (| People | - 3))"
						// Note: the previous equation at the end would have given:
	                    // 'if sick(X) then 0.69812117289589 else 0.30187882710411'
	                    // when calculated. We now calculate:
						// 'if sick(X) then 0.698141819      else 0.301858181'
						// for the equivalent branch. The difference is because the old
						// result is rounded to two decimal places and the difference
						// in the answers is because of this.
						"if X != bob and X != mary and X != john then if sick(X) then 1 else 0 else if X = bob then if sick(bob) then 0 else 1 else if sick(X) then 0.698141819 else 0.301858181"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"(belief([ sick(ann) ]), 1)", 
						false, 
						"if sick(ann) then 1 else 0"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"(belief([ sick(bob) ]), 1)", 
						false, 
						"if sick(bob) then 0 else 1"),
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
						"(belief([ sick(mary) ]), 1)", 
						false,
						// Note: old R_basic result:
						// "if sick(mary) then (0.14 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3)) / (0.14 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.58 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) else (0.58 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) / (0.14 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.58 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3))"
						"if sick(mary) then 0.698141819 else 0.301858181"),
				
				new BoundBeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"partition("
								+ "{{(on X in People) [if smokes(X) then if cancer(X) then 0.9 else 0.1 else 1] }}, "
								+ "{{ [if smokes(john) then 1 else 0] }}, "
								+ "{{ (on X in People) [if smokes(X) then 0.7 else 0.3] }})",
								"smokes/1", "cancer/1"
								/**
								 * The point of this example is to test short-circuiting.
								 * When we query cancer(john), messages about smokes(john) comes from two different parfactors.
								 * The first one, [if smokes(john) then 1 else 0], is deterministic and makes the second message irrelevant.
								 * Therefore, this second message does not need to be computed at all.
								 */
						),
						"(belief([cancer(john)]), 1)",
						false, 
						"if cancer(john) then 0.9 else 0.1"),
								
				// From ALBPTest.testCSI()
				new BoundBeliefTestData(Expressions.TRUE.toString(), new TrivialSunnyAvailableCanPlayWith(), 
						"(belief([canPlayWith(X)]), 1)", 
						false,
						"if canPlayWith(X) then 0 else 1"),
				
				// A model that looks loopy but isn't.
				new BoundBeliefTestData(Expressions.TRUE.toString(), 
						new Model(
								"union(" +
								"{{ (on X) [if p(X) and q(X) then 2 else 3]   | X  = a }}," +
								"{{ (on X) [if q(X) and p(X) then 10 else 20] | X != a }}" +
								")",
								"p/1", "q/1"
						), 
						"(belief([p(W)]), 1)", 
						false, 
						"if W = a then if p(a) then 0.454545455 else 0.545454545 else if p(W) then 0.428571429 else 0.571428571"),				

		};
		
		perform(tests);
	}
	
	//
	// PRIVATE
	//
	private class SimplifyMessagesConvexHullTestData extends TestData {
		private String expressionString; 
		private Expression expression;
		
		public SimplifyMessagesConvexHullTestData(String contextualConstraint, Model model, String expressionString,  boolean illegalArgumentTest, String expected) {
			super(contextualConstraint, model, illegalArgumentTest, expected);
			this.expressionString = expressionString;
		};
		
		@Override
		public Expression getTopExpression() {
			this.expression = parse(expressionString);
			
			return this.expression;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			SimplifyMessagesConvexHull simplifyMessagesConvexHullRewriter = new SimplifyMessagesConvexHull();
			
			Expression result = simplifyMessagesConvexHullRewriter.rewrite(this.expression, process);
			
			result = Expressions.roundToAGivenPrecision(result, _resultPrecision, process);
			
			return result;
		}
	};
	
	private class ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData extends TestData {
		private String         strInputExpr;
		private String         strRandomVariable;
		private String[][]     strArgResponseExpr;
		private Expression     inputExpr;
		private Expression     randomVariableExpr;
		private Expression[][] argResponseExpr;
		
		public ConvexRewriterOnMessageBoundsInnerUnconditionalProductTestData(String contextualConstraint, Model model, String strInputExpr, String strRandomVariable, String[][] strArgResponseExpr, boolean illegalArgumentTest, String expected) {
			super(contextualConstraint, model, illegalArgumentTest, expected);
			this.strInputExpr       = strInputExpr;
			this.strRandomVariable  = strRandomVariable;
			this.strArgResponseExpr = strArgResponseExpr;
		};
		
		@Override
		public Expression getTopExpression() {
			this.inputExpr          = parse(strInputExpr);
			this.randomVariableExpr = parse(strRandomVariable);
			this.argResponseExpr = new Expression[strArgResponseExpr.length][];
			for (int i = 0; i < strArgResponseExpr.length; i++) {
				Expression[] argResponse = new Expression[] {
					parse(strArgResponseExpr[i][0]),
					parse(strArgResponseExpr[i][1])
				};
				this.argResponseExpr[i] = argResponse;
			}
			
			return this.inputExpr;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			Rewriter normalize                         = LBPFactory.newNormalize();
			final DefaultRewriterLookup rewriterLookup = (DefaultRewriterLookup) ((DefaultRewritingProcess)process).getRewriterLookup(); 
			Rewriter inner                             = new AbstractRewriter() {
				
				@Override
				public String getName() {
					return "R_inner";
				}
				
				@Override
				public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {						
					List<Expression> timesArgs = new ArrayList<Expression>();
					for (int i = 0; i < argResponseExpr.length; i++) {
						Expression arg         = argResponseExpr[i][0];
						Expression response    = argResponseExpr[i][1];
						
						// Note: We woulnd't do this in practice but as I want
						// to test a specific return value I'll update the
						// R_canned_response on the handler to what I want.
						Rewriter childRewriter = new CannedRewriter(response);
						rewriterLookup.put("R_bound_canned_response", childRewriter);
						
						Expression childResult = process.rewrite("R_canned_response", arg);

						timesArgs.add(childResult);
					}
					Expression result = Times.make(timesArgs);
					// Note: I could simplify this before returning but for
					// these tests we won't so its clear what went where.
					return result;
				}
				
				@Override
				protected boolean isTraceInAndOutOfRewriter() {
					return true;
				}
			};
			
			rewriterLookup.put("R_normalize", normalize);
			rewriterLookup.put("R_inner",    inner);				
			
			Map<String, String> boundChildRewriters = new LinkedHashMap<String, String>();
			boundChildRewriters.put("R_canned_response", "R_bound_canned_response");
			ConvexRewriterOnMessageBounds boundsWrapper = new ConvexRewriterOnMessageBounds("R_message_bounds",
					"R_inner", new MockRandomVariableFromMessageRewriterCall(randomVariableExpr), boundChildRewriters);
			
			Expression result = boundsWrapper.rewrite(this.inputExpr, process);
			
			result = Expressions.roundToAGivenPrecision(result, _resultPrecision, process);
			
			return result;
		}
	};
	
	class ConvexRewriterOnMessageBoundsInnerConditionalProductTestData extends TestData {
		private String         strInputExpr;
		private String         strRandomVariable;
		private String[][]     strArgResponseExpr;
		private Expression     inputExpr;
		private Expression     randomVariableExpr;
		private Expression[][] argResponseExpr;
		
		public ConvexRewriterOnMessageBoundsInnerConditionalProductTestData(String contextualConstraint, Model model, String strInputExpr, String strRandomVariable, String[][] strArgResponseExpr, boolean illegalArgumentTest, String expected) {
			super(contextualConstraint, model, illegalArgumentTest, expected);
			this.strInputExpr       = strInputExpr;
			this.strRandomVariable  = strRandomVariable;
			this.strArgResponseExpr = strArgResponseExpr;
		};
		
		@Override
		public Expression getTopExpression() {
			this.inputExpr          = parse(strInputExpr);
			this.randomVariableExpr = parse(strRandomVariable);
			this.argResponseExpr    = new Expression[strArgResponseExpr.length][];
			for (int i = 0; i < strArgResponseExpr.length; i++) {
				Expression[] argResponse = new Expression[] {
					parse(strArgResponseExpr[i][0]),
					parse(strArgResponseExpr[i][1])
				};
				this.argResponseExpr[i] = argResponse;
			}
			
			return this.inputExpr;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			final DefaultRewriterLookup rewriterLookup = (DefaultRewriterLookup) ((DefaultRewritingProcess)process).getRewriterLookup();
			Rewriter normalize = LBPFactory.newNormalize();
			Rewriter inner     = new AbstractRewriter() {
				
				@Override
				public String getName() {
					return "R_inner";
				}
				
				@Override
				public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
					List<Expression> timesEvenArgs = new ArrayList<Expression>();
					List<Expression> timesOddArgs  = new ArrayList<Expression>();
					for (int i = 0; i < argResponseExpr.length; i++) {
						Expression arg         = argResponseExpr[i][0];
						Expression response    = argResponseExpr[i][1];
						
						// Note: We woulnd't do this in practice but as I want
						// to test a specific return value I'll update the
						// R_canned_response on the handler to what I want.
						Rewriter childRewriter = new CannedRewriter(response);
						rewriterLookup.put("R_bound_canned_response", childRewriter);
						
						Expression childResult = process.rewrite("R_canned_response", arg);
						if (i % 2 == 0) {
							timesEvenArgs.add(childResult);
						} 
						else {
							timesOddArgs.add(childResult);
						}
					}
					Expression condition = parse("X = a");										
					Expression result = IfThenElse.make(condition, Times.make(timesEvenArgs), Times.make(timesOddArgs));
					
					// Simplify
					result = process.rewrite("R_normalize", result);
					return result;
				}
				
				@Override
				protected boolean isTraceInAndOutOfRewriter() {
					return true;
				}
			};
			
			rewriterLookup.put("R_normalize", normalize);
			rewriterLookup.put("R_inner",    inner);				
			
			Map<String, String> boundChildRewriters = new LinkedHashMap<String, String>();
			boundChildRewriters.put("R_canned_response", "R_bound_canned_response");
			ConvexRewriterOnMessageBounds boundsWrapper = new ConvexRewriterOnMessageBounds("R_message_bounds",
					"R_inner", new MockRandomVariableFromMessageRewriterCall(randomVariableExpr), boundChildRewriters);
			
			Expression result = boundsWrapper.rewrite(this.inputExpr, process);
			
			result = Expressions.roundToAGivenPrecision(result, _resultPrecision, process);
//			result = Expressions.roundToAGivenPrecision(result, _resultPrecision); // works, if you uncomment this deprecated method in Expressions (search for "remnant")
			
			return result;
		}
	};
	
	private class CannedRewriter extends AbstractRewriter {
		private Expression cannedResponse = null;
		
		public CannedRewriter(Expression cannedResponse) {
			this.cannedResponse = cannedResponse;
		}
		
		@Override
		public String getName() {
			return "R_canned_response";
		}
		
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
			Expression result = cannedResponse;
			return result;
		}
		
		@Override
		protected boolean isTraceInAndOutOfRewriter() {
			return true;
		}
	}
	
	private class MockRandomVariableFromMessageRewriterCall implements RandomVariableFromMessageRewriterCall {
		private Expression randomVariable = null;
		
		public MockRandomVariableFromMessageRewriterCall(Expression randomVariable) {
			this.randomVariable = randomVariable;
		}
		
		@Override
		public Expression getRandomVariableFor(String rewriterName, Expression expression) {
			return randomVariable;
		}
	}
}
