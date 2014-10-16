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
package com.sri.ai.test.praise.automorphism;

import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityOfType;
import com.sri.ai.grinder.library.set.extensional.EqualityOfExtensionalUniSets;
import com.sri.ai.grinder.library.set.intensional.EqualityOfIntensionalUniSets;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.example.TrivialPQR;
import com.sri.ai.test.praise.AbstractLPITest;
import com.sri.ai.util.Util;

public class AutomorphismTest extends AbstractLPITest {

	@Test
	public void testSimplifyPassesFormulaSimplificationTests() {
		TestData[] tests = new TestData[] {
				new AutomorphismData(
						Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"{X, Y} = {Y, X}", // input
						100,
						"true" // expected output
						), 
				new AutomorphismData(
						Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"{(on X, Y) { [p(X)], [p(Y)] } | X != Y} = {(on X, Y) { [p(Y)], [p(X)] } | X != Y}", // input
						100,
						"true" // expected output
						), 
				new AutomorphismData(
						Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"{(on X, Y) { [p(X)], [p(Y)] } | X != a and Y != a} = {(on X, Y) { [p(a)], [p(X)] } | X != Y}", // input
						100,
						"false" // expected output
						),
				new AutomorphismData(
						Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"{(on X, Y) { [p(X)], [p(Y)] } | X != a and Y != a} = {(on X, Y) { [p(a)], [p(X)] } | X != Y}", // input
						100,
						"false" // expected output
						), 
				new AutomorphismData( // example given by Hung
						Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"{ (on X, Y, Z) { [q(X,Y)], [q(Y,Z)], [q(X,Z)] } | X != Y and Y != Z and Z != X }" +
						"=" +
						"{ (on X, Y, Z) { [q(Y,X)], [q(Y,Z)], [q(X,Z)] } | X != Y and Y != Z and Z != X}", // input
						100,
						"true" // expected output
						), 
				new AutomorphismData(
						Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"{X} = {Y, X}", // input
						100,
						"Y = X" // expected output
						), 
				new AutomorphismData(
						Expressions.TRUE.toString(), 
						new TrivialPQR(), 
						"{ } = {Y, X}", // input
						100,
						"false" // expected output
						), 
		};
		
		perform(tests);
	}
	
	//
	// PRIVATE METHODS
	//
	class AutomorphismData extends TestData implements CardinalityOfType.TypeSizeOfSymbolOrType {
		private String E; 
		private Expression exprE;
		private int cardinality;
		private Map<Object, Object> globalObjects;
		
		public AutomorphismData(String contextualConstraint, Model model, String E, String expected) {
			this(contextualConstraint, model, E, 100, expected);
		}
		
		public AutomorphismData(String contextualConstraint, Model model, String E, int cardinality, String expected) {
			this(contextualConstraint, model, E, null, cardinality, expected);
		}
		
		public AutomorphismData(String contextualConstraint, Model model, String E, Map<Object, Object> globalObjects, int cardinality, String expected) {
			super(contextualConstraint, model, false, expected);
			this.E = E;
			this.cardinality = cardinality;
			this.globalObjects = globalObjects;
		}
		
		//
		// START-TypeSizeOfLogicalVariable
		@Override
		public Integer getSize(Expression logicalVariable, RewritingProcess process) {
			return cardinality;
		}
		// END-TypeSizeOfLogicalVariable
		//
		
		@Override
		public Expression getTopExpression() {
			this.exprE = parse(E);
			
			return exprE;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			if (globalObjects != null) {
				process.getGlobalObjects().putAll(globalObjects);
			}
			
			// Ensure explicit counts added for all variable types.
			CardinalityOfType.registerTypeSizeOfSymbolOrTypeWithProcess(this, process);
			
			// get rid of equalities of intensional sets first
			Rewriter exhaustiveEqualityOfIntensionalUniSets =
					new TotalRewriter(AutomorphismTest.class.getName()+" exhaustiveEqualityOfIntensionalUniSets RewriteOnce", 
										Util.<Rewriter>list(new EqualityOfIntensionalUniSets()));
			exprE = exhaustiveEqualityOfIntensionalUniSets.rewrite(exprE, process);

			// get rid of equalities of extensional sets
			Rewriter exhaustiveEqualityOfExtensionalUniSets =
					new TotalRewriter(AutomorphismTest.class.getName()+" exhaustiveEqualityOfExtensionalUniSets RewriteOnce", 
										Util.<Rewriter>list(new EqualityOfExtensionalUniSets()));
			exprE = exhaustiveEqualityOfExtensionalUniSets.rewrite(exprE, process);
			
			// a more thorough simplification 
			exprE = process.rewrite(LBPRewriter.R_complete_normalize, exprE);

			return exprE;
		}
	};
}
