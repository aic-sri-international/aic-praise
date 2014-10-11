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
package com.sri.ai.test.praise.model;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultBracketedExpression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.Model.ModelError;
import com.sri.ai.praise.model.Model.ModelException;
import com.sri.ai.praise.model.ParfactorsDeclaration;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.test.praise.AbstractLPITest;
import com.sri.ai.util.Util;

public class ModelTest extends AbstractLPITest {
	
	@Test
	public void testSortDeclaration() {
		//
		// Construct legal sort declarations

		// A sort declaration with 1 argument
		SortDeclaration sort = new SortDeclaration(parse("People"));
		Assert.assertEquals(parse("People"), sort.getName());
		Assert.assertEquals(SortDeclaration.UNKNOWN_SIZE, sort.getSize().getValue());
		Assert.assertEquals(ExtensionalSet.makeEmptySet(), sort.getConstants());

		// A sort declaration with 2 arguments
		sort = new SortDeclaration(parse("People"), parse("5"));
		Assert.assertEquals(parse("People"), sort.getName());
		Assert.assertEquals(5, sort.getSize().intValue());
		Assert.assertEquals(ExtensionalSet.makeEmptySet(), sort.getConstants());

		// A sort declaration with 3 arguments
		sort = new SortDeclaration(parse("People"), parse("5"), parse("{a1, a2}"));
		Assert.assertEquals(parse("People"), sort.getName());
		Assert.assertEquals(5, sort.getSize().intValue());
		Assert.assertEquals(parse("{a1, a2}"), sort.getConstants());

		//
		// Construct and validate using static utility method
		sort = SortDeclaration.makeSortDeclaration(parse("sort(People)"));
		Assert.assertEquals(parse("People"), sort.getName());
		Assert.assertEquals(SortDeclaration.UNKNOWN_SIZE, sort.getSize().getValue());
		Assert.assertEquals(ExtensionalSet.makeEmptySet(), sort.getConstants());
		
		sort = SortDeclaration.makeSortDeclaration(parse("sort(People, 5)"));
		Assert.assertEquals(parse("People"), sort.getName());
		Assert.assertEquals(5, sort.getSize().intValue());
		Assert.assertEquals(ExtensionalSet.makeEmptySet(), sort.getConstants());
		
		sort = SortDeclaration.makeSortDeclaration(parse("sort(People, 5, {a1, a2})"));
		Assert.assertEquals(parse("People"), sort.getName());
		Assert.assertEquals(5, sort.getSize().intValue());
		Assert.assertEquals(parse("{a1, a2}"), sort.getConstants());

		//
		// Check are legal sort declarations
		Assert.assertTrue(SortDeclaration
				.isSortDeclaration(parse("sort(People)")));
		Assert.assertTrue(SortDeclaration
				.isSortDeclaration(parse("sort(People, Unknown)")));
		Assert.assertTrue(SortDeclaration
				.isSortDeclaration(parse("sort(People, Unknown, {a1, a2})")));
		Assert.assertTrue(SortDeclaration
				.isSortDeclaration(parse("sort(People, 5, {a1, a2})")));
		// This is the in-built boolean sort declaration
		Assert.assertTrue(SortDeclaration
				.isSortDeclaration(parse("sort(Boolean, 2, {false, true})")));

		//
		// Check InBuilt Check
		Assert.assertTrue(SortDeclaration
				.isInBuilt(parse("sort(Boolean, 2, {false, true})")));
		Assert.assertFalse(SortDeclaration
				.isInBuilt(parse("sort(People, 2, {false, true})")));

		//
		// Check isNameOfInBuilt
		Assert.assertTrue(SortDeclaration.isNameOfInBuilt(parse("Boolean")));
		Assert.assertFalse(SortDeclaration.isNameOfInBuilt(parse("People")));

		//
		// Construct Illegal sort declarations
		//

		// Can't construct a non string valued symbolic name
		try {
			sort = new SortDeclaration(parse("1"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		// Can't construct a new sort declaration with the same name
		// as an in-built sort.
		try {
			sort = new SortDeclaration(parse("Boolean"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		// Can't construct a with an illegal size argument
		try {
			sort = new SortDeclaration(parse("People"), parse("1.1"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		try {
			sort = new SortDeclaration(parse("People"), parse("-1"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		// Can't construct a with a non extensional uni-set
		try {
			sort = new SortDeclaration(parse("People"), parse("5"),
					parse("{{a1, a2}}"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		// Can't construct a with an extensional uni-set with a constant with
		// the same value as the sort
		try {
			sort = new SortDeclaration(parse("People"), parse("5"),
					parse("{a1, People, a2}"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		// Can't construct a with an extensional uni-set with duplicate entries
		// in it
		try {
			sort = new SortDeclaration(parse("People"), parse("5"),
					parse("{a1, a2, a2, a3}"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		// Can't construct a with an extensional uni-set with more constants
		// than the specified size of the sort
		try {
			sort = new SortDeclaration(parse("People"), parse("5"),
					parse("{a1, a2, a3, a4, a5, a6}"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		//
		// Ensure are not legal sort declarations
		Assert.assertFalse(SortDeclaration
				.isSortDeclaration(parse("asort(People)")));
		// Can't redefine an in-built sort declaration.
		Assert.assertFalse(SortDeclaration
				.isSortDeclaration(parse("sort(Boolean, 3, {true, false, something})")));
		// Must be 1 to 3 arguments
		Assert.assertFalse(SortDeclaration.isSortDeclaration(parse("sort()")));
		Assert.assertFalse(SortDeclaration
				.isSortDeclaration(parse("sort(People, 2, {a1, a2}, AnExtraArgument)")));
		// Ensure a legal name
		Assert.assertFalse(SortDeclaration.isSortDeclaration(parse("sort(1)")));
		// Ensure a legal size
		Assert.assertFalse(SortDeclaration
				.isSortDeclaration(parse("sort(People, 5.7)")));
		// Ensure a legal set of constants
		Assert.assertFalse(SortDeclaration
				.isSortDeclaration(parse("sort(People, 5, {{a1, a2}})")));
		Assert.assertFalse(SortDeclaration
				.isSortDeclaration(parse("sort(People, 5, {a1, People, a3})")));
		Assert.assertFalse(SortDeclaration
				.isSortDeclaration(parse("sort(People, 5, {a1, a2, a2, s3})")));
		Assert.assertFalse(SortDeclaration
				.isSortDeclaration(parse("sort(People, 5, {a1, a2, a3, a4, a5, a6})")));
	}

	@Test
	public void testRandomVariableDeclaration() {
		//
		// Construct legal random variable declarations
		RandomVariableDeclaration randomVariable = new RandomVariableDeclaration(
				parse("r"));
		Assert.assertEquals(parse("r"), randomVariable.getName());
		Assert.assertEquals(parse("0"), randomVariable.getArity());
		Assert.assertEquals(0, randomVariable.getArityValue());
		Assert.assertEquals(0, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("Boolean"), randomVariable.getRangeSort());

		randomVariable = new RandomVariableDeclaration(parse("p"), parse("0"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("0"), randomVariable.getArity());
		Assert.assertEquals(0, randomVariable.getArityValue());
		Assert.assertEquals(0, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("Boolean"), randomVariable.getRangeSort());

		randomVariable = new RandomVariableDeclaration(parse("p"), parse("0"),
				parse("People"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("0"), randomVariable.getArity());
		Assert.assertEquals(0, randomVariable.getArityValue());
		Assert.assertEquals(0, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("People"), randomVariable.getRangeSort());

		randomVariable = new RandomVariableDeclaration(parse("p"), parse("1"),
				parse("People"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("1"), randomVariable.getArity());
		Assert.assertEquals(1, randomVariable.getArityValue());
		Assert.assertEquals(1, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("People"), randomVariable.getParameterSorts()
				.get(0));
		Assert.assertEquals(parse("Boolean"), randomVariable.getRangeSort());

		randomVariable = new RandomVariableDeclaration(parse("p"), parse("2"),
				parse("People"), parse("Treasure"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("2"), randomVariable.getArity());
		Assert.assertEquals(2, randomVariable.getArityValue());
		Assert.assertEquals(2, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("People"), randomVariable.getParameterSorts()
				.get(0));
		Assert.assertEquals(parse("Treasure"), randomVariable
				.getParameterSorts().get(1));
		Assert.assertEquals(parse("Boolean"), randomVariable.getRangeSort());

		randomVariable = new RandomVariableDeclaration(parse("p"), parse("2"),
				parse("People"), parse("Treasure"), parse("Loot"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("2"), randomVariable.getArity());
		Assert.assertEquals(2, randomVariable.getArityValue());
		Assert.assertEquals(2, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("People"), randomVariable.getParameterSorts()
				.get(0));
		Assert.assertEquals(parse("Treasure"), randomVariable
				.getParameterSorts().get(1));
		Assert.assertEquals(parse("Loot"), randomVariable.getRangeSort());

		//
		// Construct and validate using static utility method
		randomVariable = RandomVariableDeclaration.makeRandomVariableDeclaration(parse("randomVariable(p)"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("0"), randomVariable.getArity());
		Assert.assertEquals(0, randomVariable.getArityValue());
		Assert.assertEquals(0, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("Boolean"), randomVariable.getRangeSort());
		
		randomVariable = RandomVariableDeclaration.makeRandomVariableDeclaration(parse("randomVariable(p, 0)"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("0"), randomVariable.getArity());
		Assert.assertEquals(0, randomVariable.getArityValue());
		Assert.assertEquals(0, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("Boolean"), randomVariable.getRangeSort());

		randomVariable = RandomVariableDeclaration.makeRandomVariableDeclaration(parse("randomVariable(p, 0, People)"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("0"), randomVariable.getArity());
		Assert.assertEquals(0, randomVariable.getArityValue());
		Assert.assertEquals(0, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("People"), randomVariable.getRangeSort());

		randomVariable = RandomVariableDeclaration.makeRandomVariableDeclaration(parse("randomVariable(p, 1, People)"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("1"), randomVariable.getArity());
		Assert.assertEquals(1, randomVariable.getArityValue());
		Assert.assertEquals(1, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("People"), randomVariable.getParameterSorts()
				.get(0));
		Assert.assertEquals(parse("Boolean"), randomVariable.getRangeSort());

		randomVariable = RandomVariableDeclaration.makeRandomVariableDeclaration(parse("randomVariable(p, 2, People, Treasure)"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("2"), randomVariable.getArity());
		Assert.assertEquals(2, randomVariable.getArityValue());
		Assert.assertEquals(2, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("People"), randomVariable.getParameterSorts()
				.get(0));
		Assert.assertEquals(parse("Treasure"), randomVariable
				.getParameterSorts().get(1));
		Assert.assertEquals(parse("Boolean"), randomVariable.getRangeSort());

		randomVariable = RandomVariableDeclaration.makeRandomVariableDeclaration(parse("randomVariable(p, 2, People, Treasure, Loot)"));
		Assert.assertEquals(parse("p"), randomVariable.getName());
		Assert.assertEquals(parse("2"), randomVariable.getArity());
		Assert.assertEquals(2, randomVariable.getArityValue());
		Assert.assertEquals(2, randomVariable.getParameterSorts().size());
		Assert.assertEquals(parse("People"), randomVariable.getParameterSorts()
				.get(0));
		Assert.assertEquals(parse("Treasure"), randomVariable
				.getParameterSorts().get(1));
		Assert.assertEquals(parse("Loot"), randomVariable.getRangeSort());


		//
		// Check are legal random variable declarations
		Assert.assertTrue(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(p)")));
		Assert.assertTrue(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(p, 0)")));
		Assert.assertTrue(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(p, 0, Boolean)")));
		Assert.assertTrue(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(p, 0, People)")));
		Assert.assertTrue(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(p, 1, People)")));
		Assert.assertTrue(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(p, 2, People, Treasure)")));
		Assert.assertTrue(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(p, 2, People, Treasure, Loot)")));

		//
		// Construct Illegal random variable declarations
		//

		// Can't construct a non string valued symbolic name
		try {
			randomVariable = new RandomVariableDeclaration(parse("1"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		// Can't construct a without a legal arity and following arguments
		// specified
		try {
			// negative arity not allowed
			randomVariable = new RandomVariableDeclaration(parse("p"),
					parse("-1"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		try {
			// parameters must be specified
			randomVariable = new RandomVariableDeclaration(parse("p"),
					parse("1"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		try {
			// too many paramaters plus range specified
			randomVariable = new RandomVariableDeclaration(parse("p"),
					parse("1"), parse("Parameter1Sort"),
					parse("Parameter2Sort"), parse("RangeSort"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		// Can't construct with illegal parameter names
		try {
			// parameters must be a string valued symbol
			randomVariable = new RandomVariableDeclaration(parse("p"),
					parse("1"), parse("1"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		try {
			// parameters must not be named the same as the random variable
			randomVariable = new RandomVariableDeclaration(parse("p"),
					parse("1"), parse("p"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}

		//
		// Ensure are not legal random variable declarations
		Assert.assertFalse(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("arandomVariable(p)")));
		Assert.assertFalse(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(1)")));
		Assert.assertFalse(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(p, 1)")));
		Assert.assertFalse(RandomVariableDeclaration
				.isRandomVariableDeclaration(parse("randomVariable(p, 1, A, B, C)")));
	}

	@Test
	public void testParfactorsDeclaration() {
		//
		// Construct legal empty parfactors declarations
		ParfactorsDeclaration parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors()"));
		Assert.assertEquals(0, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors(union())"));
		Assert.assertEquals(0, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors(partition())"));
		Assert.assertEquals(0, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(parse("union()"));
		Assert.assertEquals(0, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(parse("partition()"));
		Assert.assertEquals(0, parfactorsDeclaration.getParfactors().size());

		// Construct legal populated parfactors declarations
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("{[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]}"));
		Assert.assertEquals(1, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("{{[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]}}"));
		Assert.assertEquals(1, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("{ (on X in People, Y) [if p(X) and q(X, Y) then 1 else 0] | X != a1}"));
		Assert.assertEquals(1, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("{{ (on X in People, Y) [if p(X) and q(X, Y) then 1 else 0] | X != a1}}"));
		Assert.assertEquals(1, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors({[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]})"));
		Assert.assertEquals(1, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors({[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]},  {{ (on X in People, Y) [if p(X) and q(X, Y) then 1 else 0] | X != a1}})"));
		Assert.assertEquals(2, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors(union({[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]},  {{ (on X in People, Y) [if p(X) and q(X, Y) then 1 else 0] | X != a1}}))"));
		Assert.assertEquals(2, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors(partition({[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]},  {{ (on X in People, Y) [if p(X) and q(X, Y) then 1 else 0] | X != a1}}))"));
		Assert.assertEquals(2, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("union({[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]},  {{ (on X in People, Y) [if p(X) and q(X, Y) then 1 else 0] | X != a1}})"));
		Assert.assertEquals(2, parfactorsDeclaration.getParfactors().size());
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("partition({[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]},  {{ (on X in People, Y) [if p(X) and q(X, Y) then 1 else 0] | X != a1}})"));
		Assert.assertEquals(2, parfactorsDeclaration.getParfactors().size());
		
		// Ensure legal parfactors declarations
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("parfactors()")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("parfactors(union())")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("parfactors(partition())")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("union()")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("partition()")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("parfactors({[if q(a1) then 1 else 0]})")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("parfactors(union({[if q(a1) then 1 else 0]}))")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("parfactors(partition({[if q(a1) then 1 else 0]}))")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("union({[if q(a1) then 1 else 0]})")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("{[if q(a1) then 1 else 0]}")));
		Assert.assertTrue(ParfactorsDeclaration.isParfactorsDeclaration(parse("partition({[if q(a1) then 1 else 0]})")));

		
		//
		// Construct Illegal parfactors declarations
		//

		// Can't construct a non string valued symbolic name
		try {
			// not a valid functor
			parfactorsDeclaration = new ParfactorsDeclaration(parse("aparfactors()"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		try {
			// not a parfactor in the arument
			parfactorsDeclaration = new ParfactorsDeclaration(parse("parfactors(p)"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		try {
			// not a parfactor in the arument
			parfactorsDeclaration = new ParfactorsDeclaration(parse("parfactors(union(p))"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		try {
			// not a parfactor in the arument
			parfactorsDeclaration = new ParfactorsDeclaration(parse("parfactors(partition(p))"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		try {
			// not a parfactor in the arument
			parfactorsDeclaration = new ParfactorsDeclaration(parse("union(p)"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		try {
			// not a parfactor in the arument
			parfactorsDeclaration = new ParfactorsDeclaration(parse("partition(p)"));
			Assert.fail("IllegalArgumentException should have been thrown");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
		
		// Ensure illegal parfactors declarations
		Assert.assertFalse(ParfactorsDeclaration.isParfactorsDeclaration(parse("aparfactors()")));
		Assert.assertFalse(ParfactorsDeclaration.isParfactorsDeclaration(parse("parfactors(p)")));
		Assert.assertFalse(ParfactorsDeclaration.isParfactorsDeclaration(parse("parfactors(union(p))")));
		Assert.assertFalse(ParfactorsDeclaration.isParfactorsDeclaration(parse("parfactors(partition(p))")));
		Assert.assertFalse(ParfactorsDeclaration.isParfactorsDeclaration(parse("union(p)")));
		Assert.assertFalse(ParfactorsDeclaration.isParfactorsDeclaration(parse("partition(p)")));
	}
	
	@Test
	public void testParfactorsDeclarationOfEvidence() {
		ParfactorsDeclaration parfactorsDeclaration;
		Expression parfactor;
		Expression topExpression = parse("{}"); 
		RewritingProcess process = newRewritingProcess(topExpression);
		// Note: a Model must be setup and associated with the process
		// before evidence parfactors can be instantiated (as need to be able
		// to ensure are factor and not random variable declarations).
		Expression modelExpression = parse("union({{(on X,Y) [if p(X) and q(Y) then 1 else 0]}})");
		process = Model.setRewritingProcessesModel(modelExpression, process);
		

		parfactor = parse("{[if q(a1) then 1 else 0]}");
		Assert.assertTrue(ParfactorsDeclaration.isEvidenceOnlyParfactor(parfactor, process));
		
		parfactor = parse("{[if q(X) then 1 else 0]}");
		Assert.assertTrue(ParfactorsDeclaration.isEvidenceOnlyParfactor(parfactor, process));
		
		parfactor = parse("{(on X) [if q(a1) then 1 else 0] | true}");
		Assert.assertTrue(ParfactorsDeclaration.isEvidenceOnlyParfactor(parfactor, process));
		
		parfactor = parse("{(on Y) [if q(X) then 1 else 0] | true}");
		Assert.assertTrue(ParfactorsDeclaration.isEvidenceOnlyParfactor(parfactor, process));
		
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors({[if q(a1) then 1 else 0], [if q(a2) then 1 else 0]})"));
		Assert.assertTrue(ParfactorsDeclaration.isEvidenceOnly(parfactorsDeclaration, process));
		
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors({[if q(a1) then 1 else 0], [if q(X) then 1 else 0]})"));
		Assert.assertTrue(ParfactorsDeclaration.isEvidenceOnly(parfactorsDeclaration, process));
		
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors({(on X) [if q(a1) then 1 else 0] | true}, {(on X) [if q(X) then 1 else 0] | true})"));
		Assert.assertTrue(ParfactorsDeclaration.isEvidenceOnly(parfactorsDeclaration, process));
		
		parfactorsDeclaration = new ParfactorsDeclaration(
				parse("parfactors({(on X) [if q(a1) then 1 else 0] | true}, {(on Y) [if q(X) then 1 else 0] | true})"));
		Assert.assertTrue(ParfactorsDeclaration.isEvidenceOnly(parfactorsDeclaration, process));
	}

	@Test
	public void testModelDeclarationAutoDefines() {
		
		// Declare but don't define the model
		Model model = new Model("union({{(on X,Y) [if p(X) and q(Y) then 1 else 0]}})",
				"p/1", "q/1");
		
		// Ensure declaration attributes are valid
		Assert.assertEquals(Util.set("p/1", "q/1"), model.getKnownRandomVariableNameAndArities());
		
		//
		// The following calls should no longer throw exceptions as the model should auto-define.
		model.getModelDefinition();
		model.getName();
		model.getDescription();
		model.getSortDeclarations();
		model.getRandomVariableDeclarations();
		model.getParfactorsDeclaration();
	}

	@Test
	public void testIllegalModelDefinitions() {
		try {
			// TOO_MANY_STRING_PARAMETERS
			new Model(parse("model("+
						    " 'Model Name'," +
					        " 'Model Description'," +
						    " 'Extra Model String Argument'," +
					        " parfactors()" +
		                    ")"), 
		                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.TOO_MANY_STRING_PARAMETERS, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("'Extra Model String Argument'"), mex.getErrors().get(0).getInExpression());
		}
		
		try {
			// MORE_THAN_1_PARFACTORS_DECLARATION
			new Model(parse("model("+
						    " 'Model Name'," +
					        " 'Model Description'," +
					        " parfactors({{ (on X) [if p(X) then 0.2 else 0.3] }})," +
					        " parfactors({{ [if p(a) then 1 else 0] }})" +
		                    ")"), 
		                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.MORE_THAN_1_PARFACTORS_DECLARATION, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("parfactors({{ [if p(a) then 1 else 0] }})"), mex.getErrors().get(0).getInExpression());
		}
		
		try {
			// UNRECOGNIZED_MODEL_PART
			new Model(parse("model("+
						    " 'Model Name'," +
					        " 'Model Description'," +
					        " parfactors({{ (on X) [if p(X) then 0.2 else 0.3] }})," +
					        " notAModelPart()" +
		                    ")"), 
		                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.UNRECOGNIZED_MODEL_PART, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("notAModelPart()"), mex.getErrors().get(0).getInExpression());
		}
		
		try {
			// UNRECOGNIZED_MODEL_PART - a bad sort declaration
			new Model(parse("model("+
						    " 'Model Name'," +
					        " 'Model Description'," +
						    " sort(BadSort, NotANumber, 2)" +
		                    ")"), 
		                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.UNRECOGNIZED_MODEL_PART, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("sort(BadSort, NotANumber, 2)"), mex.getErrors().get(0).getInExpression());
		}
		
		try {
			// UNRECOGNIZED_MODEL_PART - a bad random variable declaration (i.e. missing argument sorts).
			new Model(parse("model("+
						    " 'Model Name'," +
					        " 'Model Description'," +
						    " randomVariable(p, 2)" +
		                    ")"), 
		                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.UNRECOGNIZED_MODEL_PART, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("randomVariable(p, 2)"), mex.getErrors().get(0).getInExpression());
		}
		
		try {
			// UNRECOGNIZED_MODEL_DEFINITION
			new Model(parse("imAModelNot()"), 
		                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.UNRECOGNIZED_MODEL_DEFINITION, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("imAModelNot()"), mex.getErrors().get(0).getInExpression());
		}
		
		try {
			// SORT_NAME_NOT_UNIQUE
			new Model(parse("model("+
				    " 'Model Name'," +
			        " 'Model Description'," +
				    " sort(ASort, Unknown, {AConstant})," +
			        " sort(AConstant)" +
                    ")"),
                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.SORT_NAME_NOT_UNIQUE, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("sort(AConstant, Unknown, {})"), mex.getErrors().get(0).getInExpression());
		}
		
		try {
			// CONSTANT_NAME_NOT_UNIQUE
			new Model(parse("model("+
				    " 'Model Name'," +
			        " 'Model Description'," +
				    " sort(ASort1, Unknown, {AConstant})," +
			        " sort(ASort2, Unknown, {ASort1})" +
                    ")"),
                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.CONSTANT_NAME_NOT_UNIQUE, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("sort(ASort2, Unknown, {ASort1})"), mex.getErrors().get(0).getInExpression());
		}
		
		try {
			// RANDOM_VARIABLE_NAME_NOT_UNIQUE
			new Model(parse("model("+
				    " 'Model Name'," +
			        " 'Model Description'," +
				    " sort(ASort1, Unknown, {AConstant})," +
			        " randomVariable(AConstant)" +
                    ")"),
                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.RANDOM_VARIABLE_NAME_NOT_UNIQUE, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("randomVariable(AConstant, 0, Boolean)"), mex.getErrors().get(0).getInExpression());
		}
		
		
		try {
			// RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED
			new Model(parse("model("+
				    " 'Model Name'," +
			        " 'Model Description'," +
				    " sort(ASort1, Unknown, {AConstant})," +
			        " randomVariable(p, 1, ASort2)" +
                    ")"),
                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("ASort2"), mex.getErrors().get(0).getInExpression());
		}
		
		try {
			// CONSTANT_IN_MORE_THAN_1_SORT
			new Model(parse("model("+
				    " 'Model Name'," +
			        " 'Model Description'," +
				    " sort(ASort1, Unknown, {AConstant})," +
				    " sort(ASort2, Unknown, {AConstant})" +
                    ")"),
                    new LinkedHashSet<String>());
			Assert.fail("ModelException should have been thrown");
		} catch (ModelException mex) {
			Assert.assertEquals(1, mex.getErrors().size());
			Assert.assertEquals(ModelError.TYPE.CONSTANT_IN_MORE_THAN_1_SORT, mex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("sort(ASort2, Unknown, {AConstant})"), mex.getErrors().get(0).getInExpression());
		}
	}
	
	@Test
	public void testLegalTrivialModelDefinition() {
		Set<String> namesAndAritiesStrings = Util.set("epidemic/0", "sick/1");

		Collection<FunctionSignature> oldDefaultPredicatesSignatures = DefaultBracketedExpression.defaultPredicateSignatures;
		DefaultBracketedExpression.defaultPredicateSignatures = Util.mapIntoList(namesAndAritiesStrings, FunctionSignature::new);

		Model model = new Model(parse("{{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}"),
								namesAndAritiesStrings);
		Assert.assertEquals(1, model.getSortDeclarations().size());
		Assert.assertEquals(0, model.getRandomVariableDeclarations().size());
		Assert.assertEquals(2, model.getKnownRandomVariableNameAndArities().size());
		Assert.assertEquals(1, model.getParfactorsDeclaration().getParfactors().size());

		DefaultBracketedExpression.defaultPredicateSignatures = oldDefaultPredicatesSignatures;
}

	@Test
	public void testLegalModelDefinition() {
		Model model = new Model(parse("model(" +
				" 'Gave Treasure To', " +
				" 'An example of how hard it is to model things without aggregate factors.', " +
				" sort(People,   Unknown, {ann, bob})," +
				" sort(Treasure, Unknown, {gold, silver, diamonds}), " +
				" randomVariable(gaveTreasureTo, 3, People, Treasure, People), " +
				" randomVariable(owns, 2, People, Treasure), " +
				" randomVariable(rich, 1, People), " +
				" parfactors(" +
				"  {{(on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X,Z,Y) then (if owns(Y,Z) then 1 else 0)  else 1] }}," +
				"  {{(on X in People, Z in Treasure) [if owns(X,Z) then if rich(X) then 1 else 0 else 1] }}" +
				" )" +
				")"),
				new LinkedHashSet<String>());
		
		Assert.assertEquals(parse("'Gave Treasure To'"), model.getName());
		Assert.assertEquals(parse("'An example of how hard it is to model things without aggregate factors.'"), model.getDescription());
		Assert.assertEquals(2, model.getSortDeclarations().size());
		Assert.assertEquals(parse("sort(People,   Unknown, {ann, bob})"), model.getSortDeclarations().get(0).getSortDeclaration());
		Assert.assertEquals(parse("sort(Treasure, Unknown, {gold, silver, diamonds})"), model.getSortDeclarations().get(1).getSortDeclaration());
		Assert.assertEquals(3, model.getRandomVariableDeclarations().size());
		Assert.assertEquals(parse("randomVariable(gaveTreasureTo, 3, People, Treasure, People, Boolean)"), model.getRandomVariableDeclarations().get(0).getRandomVariableDeclaration());
		Assert.assertEquals(parse("randomVariable(owns, 2, People, Treasure, Boolean)"), model.getRandomVariableDeclarations().get(1).getRandomVariableDeclaration());
		Assert.assertEquals(parse("randomVariable(rich, 1, People, Boolean)"), model.getRandomVariableDeclarations().get(2).getRandomVariableDeclaration());
		Assert.assertEquals(2, model.getParfactorsDeclaration().getParfactors().size());
		Assert.assertEquals(parse("{{(on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X,Z,Y) then (if owns(Y,Z) then 1 else 0)  else 1] }}"), model.getParfactorsDeclaration().getParfactors().get(0));
		Assert.assertEquals(parse("{{(on X in People, Z in Treasure) [if owns(X,Z) then if rich(X) then 1 else 0 else 1] }}"), model.getParfactorsDeclaration().getParfactors().get(1));
	}
	
	@Test
	public void testSortDeclarationsDerivedFromMinimalModelDeclarations() {
		
		// Note: Will add a default sort declaration called 'Universe'
		Model model = new Model(parse(
				"union("
						+ "{ [if epidemic then 0.1 else 0.9] }, "
						+ "{{(on X) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}, "
						+ "{{ (on X) [if sick(X) then 1 else 0] | X  = person1 or  X  = person2 or  X  = person3 }},"
						+ "{{ (on X) [if sick(X) then 0 else 1] | X != person1 and X != person2 and X != person3 }}" +
						")"
				), new LinkedHashSet<String>());
			
			Assert.assertEquals(parse("'No name given.'"), model.getName());
			Assert.assertEquals(parse("'No description given.'"), model.getDescription());
			Assert.assertEquals(1, model.getSortDeclarations().size());
			Assert.assertEquals(parse("sort(Universe,   Unknown, {})"), model.getSortDeclarations().get(0).getSortDeclaration());
			Assert.assertEquals(0, model.getRandomVariableDeclarations().size());
			Assert.assertEquals(4, model.getParfactorsDeclaration().getParfactors().size());
			
		
		// Note: Will add a sort declaration 'People' based on being used as a type
	    // in the intensionally defined parfactors.
		model = new Model(parse(
			"union("
					+ "{ [if epidemic then 0.1 else 0.9] }, "
					+ "{{(on X in People) [if epidemic then if sick(X) then 0.4 else 0.6 else if sick(X) then 0.01 else 0.99]}}, "
					+ "{{ (on X in People) [if sick(X) then 1 else 0] | X  = person1 or  X  = person2 or  X  = person3 }},"
					+ "{{ (on X in People) [if sick(X) then 0 else 1] | X != person1 and X != person2 and X != person3 }}" +
					")"
			), new LinkedHashSet<String>());
		
		Assert.assertEquals(parse("'No name given.'"), model.getName());
		Assert.assertEquals(parse("'No description given.'"), model.getDescription());
		Assert.assertEquals(1, model.getSortDeclarations().size());
		Assert.assertEquals(parse("sort(People,   Unknown, {})"), model.getSortDeclarations().get(0).getSortDeclaration());
		Assert.assertEquals(0, model.getRandomVariableDeclarations().size());
		Assert.assertEquals(4, model.getParfactorsDeclaration().getParfactors().size());
	}
}
