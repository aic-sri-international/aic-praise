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

import java.util.Iterator;
import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.example.TrivialEpidemicAndSickNotbob;
import com.sri.ai.praise.model.example.TrivialEpidemicSickEveryone;
import com.sri.ai.praise.model.example.TrivialEpidemicSickEveryoneNotbobAmaryAjohn;
import com.sri.ai.praise.model.example.TrivialEpidemicSickbob;
import com.sri.ai.praise.model.example.TrivialGaveTreasureToOwnsRich;
import com.sri.ai.praise.model.example.TrivialPQPeoplea1Anda2;
import com.sri.ai.praise.model.example.TrivialPQWithPriors;
import com.sri.ai.praise.model.example.TrivialPeopleAmericanTallIntelligentUnintelligent;
import com.sri.ai.praise.model.example.TrivialSickSmokerbob;
import com.sri.ai.praise.model.example.TrivialSickbob;
import com.sri.ai.praise.model.example.TrivialSunnyAvailableCanPlayWith;
import com.sri.ai.praise.model.example.WeightedPQWithPriors;
import com.sri.ai.test.praise.AbstractLPITest;
import com.sri.ai.util.Util;

public class AnytimeLBPTest extends AbstractLPITest {

	@Test
	public void testAnytimeBeliefForNonLoopyModels() {
		class AnytimeBeliefTestData extends TestData {
			private String belief; 
			private Expression exprBelief;
			private Map<Object, Object> globalObjects;
			
			public AnytimeBeliefTestData(String contextualConstraint, Model model, String belief, boolean illegalArgumentTest, String expected) {
				this(contextualConstraint, model, belief, null, illegalArgumentTest, expected);
			};
			
			public AnytimeBeliefTestData(String contextualConstraint, Model model, String belief, Map<Object, Object> globalObjects, boolean illegalArgumentTest, String expected) {
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

				Expression belief = null;
				Iterator<Expression> anytimeBeliefIterator = LBPFactory.newAnytimeLBPRewriteRequest(LBPRewriter.R_bound_belief, exprBelief, process);
				int iteration = 0;
				while (anytimeBeliefIterator.hasNext()) {
					belief = anytimeBeliefIterator.next();
					iteration++;
					System.out.println("Current belief at iteration "+iteration+"="+belief);
				}
				
				Expression roundedBelief = Expressions.roundToAGivenPrecision(belief, 9, process);
				
				return roundedBelief;
			}
		};
		
		//
		// A Sample of tests from LBPTest.testBeliefForNonLoopyModels()
		AnytimeBeliefTestData[] tests = new AnytimeBeliefTestData[] {
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), 
					new Model(
							"union("
									+ "{{(on X in People) [if sick(X) then 0.4 else 0.6]}}, "
									+ "{{ [if sick(john) then 1 else 0] }}"
									+ ")",
							"sick/1"
					),
					"belief([sick(X)])",
					false,
					"if X = john then if sick(john) then 1 else 0 else if sick(X) then 0.4 else 0.6"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), 
					new Model(
							"union("
									+ "{ [if epidemic then 0.1 else 0.9] }, "
									+ "{{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}, "
									+ "{{ (on X in People) [if sick(X) then 1 else 0] | X  = person1 or  X  = person2 or  X  = person3 }},"
									+ "{{ (on X in People) [if sick(X) then 0 else 1] | X != person1 and X != person2 and X != person3 }}"
									+ ")",
							"epidemic/0", "sick/1"
					),
					"belief([epidemic])",
					// Util.map(parse("|People|"), Expressions.createSymbol(20)),
					false,
					// Note: old R_basic result:
					// "if epidemic then (0.0064 * 0.6 ^ (| People | - 3)) / (0.0064 * 0.6 ^ (| People | - 3) + 9E-7 * 0.99 ^ (| People | - 3)) else (9E-7 * 0.99 ^ (| People | - 3)) / (0.0064 * 0.6 ^ (| People | - 3) + 9E-7 * 0.99 ^ (| People | - 3))"
					"if epidemic then 0.995339619 else 0.00466038114"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), 
					new Model(
							"union("
									+ "{ [if epidemic then 0.1 else 0.9] }, "
									+ "{{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}, "
									+ "{{ (on X in People) [if sick(X) then 1 else 0] | X  = person1 or  X  = person2 or  X  = person3 }},"
									+ "{{ (on X in People) [if sick(X) then 0 else 1] | X != person1 and X != person2 and X != person3 }}" +
									")",
							"epidemic/0", "sick/1"
					),
					"belief([epidemic])",
					Util.map(parse("| People |"), Expressions.makeSymbol(20)),
					false, 
					"if epidemic then 0.588128460 else 0.411871540"),
					
			//		
			// Basic: Straight forward non-loopy (i.e. exact) cases
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPQWithPriors(), 
					"belief([p(X)])", 
					false, 
					"if p(X) then 1 else 0"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPQWithPriors(), 
					"belief([q(X)])", 
					false, 
					"if q(X) then 1 else 0"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), 
					new WeightedPQWithPriors(), 
					"belief([p(X)])", 
					false, 
					"if p(X) then 0.223300971 else 0.776699029"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new WeightedPQWithPriors(), 
					"belief([q(X)])", 
					false, 
					"if q(X) then 0.320388350 else 0.679611650"),

			// From ALBPTest.testIntensionalFanIn()
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
					"belief([ q(a1) ])", 
					false, 
					"if q(a1) then 1 else 0"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
					"belief([ q(a2) ])", 
					false, 
					"if q(a2) then 1 else 0"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
					"belief([ q(a3) ])", 
					false,				
					"if q(a3) then 1 else 0"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), 
					new TrivialPQPeoplea1Anda2(), 
					"belief([ q(X) ])", 
					false, 
					// Note: old R_basic result:
					// "if X = a1 then if q(a1) then 1 else 0 else if X = a2 then if q(a2) then 1 else 0 else if q(X) then 1 else 0"
					"if X = a1 then if q(a1) then 1 else 0 else if X = a2 then if q(a2) then 1 else 0 else if q(X) then 1 else 0"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPQPeoplea1Anda2(), 
					"belief([ p ])", 
					false, 
					"if p then 1 else 0"),
					
			// From ALBPTest.testBelief()
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
					"belief([tall(X)])", 
					false, 
					"0.5"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
					"belief([tall(a1)])", 
					false, 
					"0.5"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
					"belief([american(X)])", 
					false, 
					"if american(X) then 0.687500000 else 0.312500000"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
					"belief([american(a1)])", 
					false, 
					"if american(a1) then 0.687500000 else 0.312500000"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
					"belief([intelligent(X)])", 
					false, 
					"0.5"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
					"belief([intelligent(a1)])", 
					false, 
					"0.5"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
					"belief([unintelligent(X)])", 
					false, 
					"0.5"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialPeopleAmericanTallIntelligentUnintelligent(), 
					"belief([unintelligent(a1)])", 
					false, 
					"0.5"),
					
			// From ALBPTest.testExponentiatedLifted()
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
					"belief([ epidemic ])", 
					false, 
					// Note: old R_basic result:
					// "if epidemic then 1 / (1 + 0.8 ^ (|People| - 1)) else 0.8 ^ (|People| - 1) / (1 + 0.8 ^ (|People| - 1))"
					"if epidemic then 0.881664935 else 0.118335065"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
					"belief([ sick(X) ])", 
					false, 
					// Note: old R_formula_simplification result before R_normalize used instead
					// Difference is because | People | -> 10 and new result is this expression calculated correctly with that.
					// "if X != bob then if sick(X) then (0.4 * 0.8 ^ (|People| - 2) + 0.6) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else (0.4 * 0.8 ^ (|People| - 2) + 0.4) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else 0.5"
					"if X = bob then 0.500000000 else (if sick(X) then 0.588166494 else 0.411833506)"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
					"belief([ sick(ann) ])", 
					false, 
					// Note: old R_basic result:
					// "if sick(ann) then (0.4 * 0.8 ^ (|People| - 2) + 0.6) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2)) else (0.4 * 0.8 ^ (|People| - 2) + 0.4) / (0.4 * 0.8 ^ (|People| - 2) + 1 + 0.4 * 0.8 ^ (|People| - 2))"
					"if sick(ann) then 0.588166494 else 0.411833506"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicAndSickNotbob(), 
					"belief([ sick(bob) ])", 
					false, 
					"0.5"),
									
			// From ALBPTest.testExponentiatedLifted2()
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich() , 
					"belief([rich(bob)])", 
					false,
					// Note: old R_basic result:
					// "if rich(bob) then (1 + 2 ^ |People|) ^ |Treasure| / ((1 + 2 ^ |People|) ^ |Treasure| + 1) else 1 / ((1 + 2 ^ |People|) ^ |Treasure| + 1)"
					"if rich(bob) then 1 else 0.000000000000000000000000000000781198402"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
					"belief([rich(X)])", 
					false,
					// Note: old R_basic result
					// "if rich(X) then (1 + 2 ^ |People|) ^ |Treasure| / ((1 + 2 ^ |People|) ^ |Treasure| + 1) else 1 / ((1 + 2 ^ |People|) ^ |Treasure| + 1)"
					"if rich(X) then 1 else 0.000000000000000000000000000000781198402"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
					"belief([gaveTreasureTo(X,Z,Y)])", 
					false, 
					// Note: old R_basic and R_formula_simlification result:
					// "if |People| > 0 then if gaveTreasureTo(X, Z, Y) then (2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ (|People| - 1) * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else if gaveTreasureTo(X, Z, Y) then (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1))"
					"if gaveTreasureTo(X, Z, Y) then 0.499512195 else 0.500487805"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialGaveTreasureToOwnsRich(), 
					"belief([owns(X,Y)])", 
					false,
					// Note: old R_basic and R_formula_simlification result:
					//"if owns(X, Y) then (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1)) else (1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1)) / (2 ^ |People| * (1 + 2 ^ |People|) ^ (|Treasure| - 1) + 1 + (1 + 2 ^ |People|) ^ (|Treasure| - 1))"
					"if owns(X, Y) then 0.999024390 else 0.000975609756"),
					
			// From ALBPTest.testQueryVariableSplitting()
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialSickbob(), 
					"belief([sick(Person)])", 
					false, 
					"if Person = bob then if sick(bob) then 0.8 else 0.2 else 0.5"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialSickbob(), 
					"belief([sick(bob)])", 
					false, 
					"if sick(bob) then 0.8 else 0.2"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialSickbob(), 
					"belief([sick(ann)])", 
					false, 
					"0.5"),
					
			// From ALBPTest.testMultiLevelMessagePassing()
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
					"belief([smoker(Person)])", 
					false, 
					// Note: old R_basic result:
					// "if Person = bob then if smoker(bob) then 0.15 else 0.85 else if smoker(Person) then 0.1 else 0.9"
					"if Person = bob then if smoker(bob) then 0.150943396 else 0.849056604 else if smoker(Person) then 0.100000000 else 0.900000000"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
					"belief([smoker(bob)])", 
					false, 
					"if smoker(bob) then 0.150943396 else 0.849056604"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
					"belief([sick(X)])", 
					false, 
					// Note: old R_basic result:
					// "if X = bob then if sick(bob) then 1 else 0 else if sick(X) then 0.53 else 0.47"
					// i.e. no constraint applier used, so sick(bob) is sick(X)
					"if X = bob then if sick(bob) then 1 else 0 else if sick(X) then 0.530000000 else 0.470000000"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialSickSmokerbob(), 
					"belief([sick(bob)])", 
					false, 
					"if sick(bob) then 1 else 0"),
					
			// From ALBPTest.testEpidemic()
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
					"belief([ epidemic ])", 
					false, 
			      "if epidemic then 0.28 else 0.72"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
					"belief([ sick(X) ])", 
					false,
					"if X = bob then if sick(bob) then 1 else 0 else if sick(X) then 0.34 else 0.66"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
					"belief([ sick(ann) ])", 
					false,
					"if sick(ann) then 0.34 else 0.66"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickbob(), 
					"belief([ sick(bob) ])", 
					false, 
					"if sick(bob) then 1 else 0"),
							
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
					"belief([ epidemic ])", 
					false, 
					// Note: old R_basic result:
					// "if epidemic then (0.1 * 0.7 ^ |People|) / (0.1 * 0.7 ^ |People| + 0.9 * 0.2 ^ |People|) else (0.9 * 0.2 ^ |People|) / (0.1 * 0.7 ^ |People| + 0.9 * 0.2 ^ |People|)"
					"if epidemic then 0.999967375 else 0.0000326248029"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
					"belief([ sick(X) ])", 
					false, 
					"if sick(X) then 1 else 0"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryone(), 
					"belief([ sick(ann) ])", 
					false, 
					"if sick(ann) then 1 else 0"),
					
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
					"belief([ epidemic ])", 
					false,
					// Note: old R_basic result:
					// "if epidemic then (0.03 * 0.7 ^ (|People| - 3)) / (0.03 * 0.7 ^ (|People| - 3) + 0.72 * 0.2 ^ (|People| - 3)) else (0.72 * 0.2 ^ (|People| - 3)) / (0.03 * 0.7 ^ (|People| - 3) + 0.72 * 0.2 ^ (|People| - 3))"
					"if epidemic then 0.996283639 else 0.00371636130"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
					"belief([ sick(X) ])", 
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
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
					"belief([ sick(ann) ])", 
					false, 
					"if sick(ann) then 1 else 0"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
					"belief([ sick(bob) ])", 
					false, 
					"if sick(bob) then 0 else 1"),
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialEpidemicSickEveryoneNotbobAmaryAjohn(), 
					"belief([ sick(mary) ])", 
					false,
					// Note: old R_basic result:
					// "if sick(mary) then (0.14 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3)) / (0.14 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.58 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) else (0.58 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3)) / (0.14 * 0.2 ^ (| People | - 3) + 0.021 * 0.7 ^ (| People | - 3) + 0.58 * 0.2 ^ (| People | - 3) + 0.009 * 0.7 ^ (| People | - 3))"
					"if sick(mary) then 0.698141819 else 0.301858181"),
			
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), 
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
					"belief([cancer(john)])",
					false, 
					"if cancer(john) then 0.9 else 0.1"),
							
			// From ALBPTest.testCSI()
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), new TrivialSunnyAvailableCanPlayWith(), 
					"belief([canPlayWith(X)])", 
					false,
					"if canPlayWith(X) then 0 else 1"),
			
			// A model that looks loopy but isn't.
			new AnytimeBeliefTestData(Expressions.TRUE.toString(), 
					new Model(
							"union(" +
							"{{ (on X) [if p(X) and q(X) then 2 else 3]   | X  = a }}," +
							"{{ (on X) [if q(X) and p(X) then 10 else 20] | X != a }}" +
							")",
							"p/1", "q/1"
					), 
					"belief([p(W)])", 
					false, 
					"if W = a then if p(a) then 0.454545455 else 0.545454545 else if p(W) then 0.428571429 else 0.571428571"),				
		};
		
		perform(tests);
	}
}
