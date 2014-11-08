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

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.praise.PRAiSEConfiguration;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.ModelGrounding;
import com.sri.ai.praise.model.ModelGrounding.GroundedModelResult;
import com.sri.ai.praise.model.ModelGrounding.ModelGroundingError;
import com.sri.ai.praise.model.ModelGrounding.ModelGroundingException;
import com.sri.ai.test.praise.AbstractLPITest;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.Util;

public class ModelGroundingTest extends AbstractLPITest {
	
	@Test
	public void testCantGroundDueToNoModelDefinedInProcess() {
		try {
			RewritingProcess process = newRewritingProcess(Expressions.ZERO);
			ModelGrounding.groundModel(DefaultSymbol.createSymbol("Ground Model Name"), 
					                   DefaultSymbol.createSymbol("Ground Model Description"),
					                   process);
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.NO_MODEL_DEFINITION_IN_PROCESS_TO_GROUND, gmex.getErrors().get(0).getErrorType());
		}
	}

	@Test
	public void testModelInvalidDueToFreeVariables() {
		try {
			callGroundModel(new Model(
					// Y is free in the intensional parfactor
					"union( {{ (on X) [if p(X, Y) then 0.2 else 0.3] }}, {{ [if p(a, b) then 1 else 0] }} )",
					"p/2"));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.FREE_VARIABLES, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("{{ (on X) [if p(X, Y) then 0.2 else 0.3] }}"), gmex.getErrors().get(0).getInExpression());
		}
		
		try {
			callGroundModel(new Model(
					// Y is free in the extensional parfactor
					"union( {{ (on X, Y) [if p(X, Y) then 0.2 else 0.3] }}, {{ [if p(a, Y) then 1 else 0] }} )",
					"p/2"));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.FREE_VARIABLES, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("{{ [if p(a, Y) then 1 else 0] }}"), gmex.getErrors().get(0).getInExpression());
		}
		
		try {
			callGroundModel(new Model(
					// Y is free in the intensional and extensional parfactor
					"union( {{ (on X) [if p(X, Y) then 0.2 else 0.3] }}, {{ [if p(a, Y) then 1 else 0] }} )",
					"p/2"));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(2, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.FREE_VARIABLES, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("{{ (on X) [if p(X, Y) then 0.2 else 0.3] }}"), gmex.getErrors().get(0).getInExpression());
			Assert.assertEquals(ModelGroundingError.TYPE.FREE_VARIABLES, gmex.getErrors().get(1).getErrorType());
			Assert.assertEquals(parse("{{ [if p(a, Y) then 1 else 0] }}"), gmex.getErrors().get(1).getInExpression());
		}
	}
	
	@Test
	public void testModelInvalidDueToParfactorsBeingRandomVariables() {
		try {
			callGroundModel(new Model(
					// The intensional set is on a Random Variable not a parfactor
					"union( {{ (on X, Y) [p(X, Y)] }}, {{ [if p(a, b) then 1 else 0] }} )",
					"p/2"));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.NOT_A_PARFACTOR, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("{{ (on X, Y) [p(X, Y)] }}"), gmex.getErrors().get(0).getInExpression());
		}
		
		try {
			callGroundModel(new Model(
					// The extensional set is on a Random Variable not a parfactor
					"union( {{ (on X, Y) [if p(X, Y) then 0.2 else 0.3] }}, {{ [p(a, b)] }} )",
					"p/2"));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.NOT_A_PARFACTOR, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("{{ [p(a, b)] }}"), gmex.getErrors().get(0).getInExpression());
		}
	}
	
	@Test
	public void testModelInvalidDueToRandomVariableValueExpressionsHavingInconsistenParameterSorts() {
		try {
			callGroundModel(new Model(
					"model("+
					" sort(People,   Unknown, {ann, bob})," +
					" sort(Treasure, Unknown, {gold, silver, diamonds})," +
					" randomVariable(gaveTreasureTo, 3, People, Treasure, People)," +
					" parfactors("+
					// Y and Z are the wrong way around
					"  {{ (on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X, Y, Z) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.RANDOM_VARIABLE_VALUE_EXPRESSION_HAS_INCONSISTENT_PARAMETER_SORTS, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("gaveTreasureTo(X, Y, Z)"), gmex.getErrors().get(0).getInExpression());
		}
		
		try {
			callGroundModel(new Model(
					"model("+
					" sort(People,   Unknown, {ann, bob})," +
					" sort(Treasure, Unknown, {gold, silver, diamonds})," +
					" parfactors("+
					// The random variable declaration will be inferred, however the second
					// parfactor orders the sort argumetns to gaveTreasureTo inconsistently
					// with the first usage
					"  {{ (on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}," +
					"  {{ (on X in People, Y in People, Z in Treasure) [if rich(X) and gaveTreasureTo(X, Y, Z) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3", "rich/1"));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.RANDOM_VARIABLE_VALUE_EXPRESSION_HAS_INCONSISTENT_PARAMETER_SORTS, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("gaveTreasureTo(X, Y, Z)"), gmex.getErrors().get(0).getInExpression());
		}
	}
	
	@Test
	public void testModelInvalidDueToInsufficientRndomVariableInformationForGrounding() {
		try {
			callGroundModel(new Model(
					"model("+
					" sort(People,   Unknown, {ann, bob})," +
					" sort(Treasure, Unknown, {gold, silver, diamonds})," +
					" parfactors("+
					// The random variable declaration 'gaveTreasureTo' will be inferred, 
					// from the parfactor, however, don't have enough information - i.e.
					// what does Z map to in order to perform a grounding with this.
					"  {{ (on X in People, Y in People, Z) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.INSUFFICIENT_RANDOM_VARIABLE_INFORMATION_FOR_GROUNDING, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("gaveTreasureTo"), gmex.getErrors().get(0).getInExpression());
		}
	}
	
	@Test
	public void testModelInvalidDueToSortCardinalityConflictingWithPrespecifiedSize() {
		try {
			callGroundModel(new Model(
					"model("+
					// Am explicitly stating in the model that the
					// size of the Sort is fixed at 2
					" sort(People,   2, {ann, bob})," +
					" sort(Treasure, Unknown, {gold, silver, diamonds})," +
					" parfactors("+
					"  {{ (on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"),
					// Here, I'm trying to specify it as a different value
					Util.map(parse("|People|"), parse("3"), parse("|Treasure|"), parse("3")));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.SORT_CARDINALITY_CONFLICTS_WITH_PRESPECIFIED_SIZE, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("People"), gmex.getErrors().get(0).getInExpression());
		}
		
		try {
			// Note: This case is very similar to the last one except that
			// we don't state the cardinality of Treasure as part of the global
			// objects, just on the sort declaration, which is sufficient.
			// Therefore, should only get the one error being thrown.
			callGroundModel(new Model(
					"model("+
					// Am explicitly stating in the model that the
					// size of the Sort is fixed at 2
					" sort(People,   2, {ann, bob})," +
					" sort(Treasure, 3, {gold, silver, diamonds})," +
					" parfactors("+
					"  {{ (on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"),
					// Here, I'm trying to specify it as a different value
					Util.map(parse("|People|"), parse("3")));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.SORT_CARDINALITY_CONFLICTS_WITH_PRESPECIFIED_SIZE, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("People"), gmex.getErrors().get(0).getInExpression());
		}
	}
	
	@Test
	public void testModelInvalidDueToCardinalityOfSortNotSpecified() {
		try {
			callGroundModel(new Model(
					"model("+
					" sort(People,   Unknown, {ann, bob})," +
					" sort(Treasure, Unknown, {gold, silver, diamonds})," +
					" parfactors("+
					"  {{ (on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"),
					// Have forgotten to specify the cardinality of Treasure
					Util.map(parse("|People|"), parse("2")));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.CARDINALITY_OF_SORT_NOT_SPECIFIED, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("Treasure"), gmex.getErrors().get(0).getInExpression());
		}
	}
	
	@Test
	public void testModelInvalidDueToUnableToInstantiateValidSortDeclaration() {
		try {
			callGroundModel(new Model(
					"model("+
					" sort(People,   2, {ann, bob})," +
					" sort(Treasure, Unknown, {gold, silver, diamonds})," +
					" parfactors("+
					"  {{ (on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"),
					// Can't instantiate the sort declaration as the
					// cardinality is less than the number of constants
					// assigned to it.
					Util.map(parse("|Treasure|"), parse("2")));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.UNABLE_TO_INSTANTIATE_A_VALID_SORT_DECLARATION_FOR_GROUNDING, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("Treasure"), gmex.getErrors().get(0).getInExpression());
		}
	}
	
	@Test
	public void testModelInvalidDueToErrorWhenCreatingGroundModel() {
		try {
			callGroundModel(new Model(
					"model("+
			        " 'Name', " +
				    " 'Description', " +
					" parfactors("+
					// Note: The problem here is that we are using a constant in two 
					// different Sort positions, in the first parfactor 'ann' is a People
					// constant but in the second parfactor 'ann' is a Treasure constant.
					"  {{ (on Z in Treasure) [if gaveTreasureTo(ann, Z, bob) then 1 else 0] }}," +
					"  {{ (on X in People, Y in People) [if gaveTreasureTo(X, ann, Y) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"),
					Util.map(parse("|People|"),   parse("2"),
							 parse("|Treasure|"), parse("2")));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.MODEL_ERROR_WHEN_CREATING_GROUND_MODEL, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("model(" +
					" 'Name', " +
				    " 'Description', " +
					// This is the problem that occurs, the constant has been assigned to
				    // two different sorts, when trying to instantiate the ground model
					// this should be detected.
				    // Note: 'treasure2' is generated as part of the grounding process.
					" sort(Treasure, 2, {ann, treasure2})," +
					" sort(People,   2, {ann, bob})," +
					" randomVariable(gaveTreasureTo, 3, People, Treasure, People, Boolean), " +
					" parfactors()"+
					")"), 
					gmex.getErrors().get(0).getInExpression());
			List<Model.ModelError> modelErrors = gmex.getErrors().get(0).getModelErrors();
			// Ensure that the expected model error information is correct
			Assert.assertEquals(1, modelErrors.size());
			Assert.assertEquals(Model.ModelError.TYPE.CONSTANT_IN_MORE_THAN_1_SORT, modelErrors.get(0).getErrorType());
			Assert.assertEquals(parse("sort(People, 2, {ann, bob})"), modelErrors.get(0).getInExpression());
		}
	}
	
	@Test
	public void testGroundModelDoesNotExceedAllowedSize() {		
		Configuration.setProperty(PRAiSEConfiguration.KEY_PERFORM_MAX_ALLOWED_SIZE_CHECK_FOR_GROUNDED_MODEL, Boolean.TRUE.toString());
		try {
			Configuration.setProperty(PRAiSEConfiguration.KEY_MAX_ALLOWED_SIZE_FOR_GROUNDED_MODEL, "3");
			callGroundModel(new Model(
					"model("+
			        " 'Name', " +
				    " 'Description', " +
				    // Note: Even though this is already grounded in reality
				    // we still perform the check
				    " sort(Universe, 10), " +
				    " parfactors("+
					"  {[if gaveTreasureTo(ann, gold, bob) then 1 else 0]}, " +
					"  {[if gaveTreasureTo(bob, gold, ann) then 1 else 0]}, " +
					"  {[if gaveTreasureTo(ann, silver, bob) then 1 else 0]}, " +
					"  {[if gaveTreasureTo(bob, silver, ann) then 1 else 0]} " +
					" )" +
		            ")",
					"gaveTreasureTo/3"));
			
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.GROUND_MODEL_EXCEEDS_MAX_ALLOWED_SIZE, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("3"), gmex.getErrors().get(0).getInExpression());
		}
		
		try {
			Configuration.setProperty(PRAiSEConfiguration.KEY_MAX_ALLOWED_SIZE_FOR_GROUNDED_MODEL, "4");
			callGroundModel(new Model(
					"model("+
			        " 'Name', " +
				    " 'Description', " +
				    // Note: Even though this is already grounded in reality
				    // we still perform the check
				    " sort(Universe, 10), " +
				    " parfactors("+
					"  {[if gaveTreasureTo(ann, gold, bob) then 1 else 0]}, " +
					"  {[if gaveTreasureTo(bob, gold, ann) then 1 else 0]}, " +
					"  {[if gaveTreasureTo(ann, silver, bob) then 1 else 0]}, " +
					"  {[if gaveTreasureTo(bob, silver, ann) then 1 else 0]} " +
					" )" +
		            ")",
					"gaveTreasureTo/3"));
		} catch (ModelGroundingException gmex) {
			Assert.fail("GroundModelException should not have been thrown as 4 ground factors are allowed.");
		}
		
		try {
			Configuration.setProperty(PRAiSEConfiguration.KEY_MAX_ALLOWED_SIZE_FOR_GROUNDED_MODEL, "7");
			callGroundModel(new Model(
					"model("+
			        " 'Name', " +
				    " 'Description', " +
					" parfactors("+
				    // The cartesian product for this should be 2x2x2 = 8
					// which for this test is > than the allowed number
					"  {{ (on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"),
					Util.map(parse("|People|"),   parse("2"),
							 parse("|Treasure|"), parse("2")));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.GROUND_MODEL_EXCEEDS_MAX_ALLOWED_SIZE, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("7"), gmex.getErrors().get(0).getInExpression());
		}
		
		try {
			Configuration.setProperty(PRAiSEConfiguration.KEY_MAX_ALLOWED_SIZE_FOR_GROUNDED_MODEL, "8");
			callGroundModel(new Model(
					"model("+
			        " 'Name', " +
				    " 'Description', " +
					" parfactors("+
				    // The cartesian product for this should be 2x2x2 = 8
					// which for this test is = to the max allowed
					"  {{ (on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"),
					Util.map(parse("|People|"),   parse("2"),
							 parse("|Treasure|"), parse("2")));
		} catch (ModelGroundingException gmex) {
			Assert.fail("GroundModelException should not have been thrown as 4 ground factors are allowed.");
		}
	}
	
	@Test
	public void testCannotGroundDueToBeingUnableToDetermingGroundingOfAnIntensionalParfactor() {
		try {
			callGroundModel(new Model(
					"model("+
			        " 'Name', " +
				    " 'Description', " +
					" sort(People,   2, {ann, bob})," +
					" sort(Treasure, 2, {gold, silver})," +
					" parfactors("+
					// Note: The problem here is that we have a predicate that
					// we do not know what value it should take in the grounded model
					// (this would require procedural attachment with the process in 
					// order to be able to determine the outcome for this grounding).
					"  {{ (on X in People, Z in Treasure, Y in People) [if gaveTreasureTo(X, Z, Y) then 1 else 0] | anUnknownPredicate(Z) }}" +
					" )" +
		            ")",
					"gaveTreasureTo/3"));
			Assert.fail("GroundModelException should have been thrown.");
		} catch (ModelGroundingException gmex) {
			Assert.assertEquals(1, gmex.getErrors().size());
			Assert.assertEquals(ModelGroundingError.TYPE.CANNOT_DETERMINE_GROUNDING_OF_INTENSIONAL_PARFACTOR, gmex.getErrors().get(0).getErrorType());
			Assert.assertEquals(parse("{{ (on X in People, Z in Treasure, Y in People) [if gaveTreasureTo(X, Z, Y) then 1 else 0] | anUnknownPredicate(Z) }}"), gmex.getErrors().get(0).getInExpression());
		}
	}
	
	@Test
	public void testGroundModelStraightForward() throws ModelGroundingException {
		GroundedModelResult groundedModelResult;
		
		groundedModelResult = callGroundModel(new Model(
				"model("+
		        " 'Name', " +
			    " 'Description', " +
				" parfactors("+
			    // The cartesian product for this should be 1x2x1 = 2
				"  {{ (on X in People, Y in People, Z in Treasure) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}" +
				" )" +
	            ")",
				"gaveTreasureTo/3"),
				Util.map(parse("|People|"),   parse("1"),
						 parse("|Treasure|"), parse("2")));
		
		Assert.assertEquals(parse("model(" +
		        " 'Name', " +
			    " 'Description', " +
				" sort(People,   1, {people1})," +
				" sort(Treasure, 2, {treasure1, treasure2})," +
				" randomVariable(gaveTreasureTo, 3, People, Treasure, People, Boolean)," +
				" parfactors("+
				"  { [if gaveTreasureTo(people1, treasure1, people1) then 1 else 0], " +
				"    [if gaveTreasureTo(people1, treasure2, people1) then 1 else 0]  " +
				"  })" +
				")"), 
				groundedModelResult.getGroundedModel().getModelDefinition());
		
		groundedModelResult = callGroundModel(new Model(
				"model("+
		        " 'Name', " +
			    " 'Description', " +
				" parfactors("+
			    // The cartesian product for this should be 2x2x2 = 8
				"  {{ (on X in People, Z in Treasure, Y in People) [if gaveTreasureTo(X, Z, Y) then 1 else 0] }}" +
				" )" +
	            ")",
				"gaveTreasureTo/3"),
				Util.map(parse("|People|"),   parse("2"),
						 parse("|Treasure|"), parse("2")));
		
		Assert.assertEquals(parse("model(" +
		        " 'Name', " +
			    " 'Description', " +
				" sort(People,   2, {people1, people2})," +
				" sort(Treasure, 2, {treasure1, treasure2})," +
				" randomVariable(gaveTreasureTo, 3, People, Treasure, People, Boolean)," +
				" parfactors("+
				"  { [if gaveTreasureTo(people1, treasure1, people1) then 1 else 0], " +
				"    [if gaveTreasureTo(people1, treasure1, people2) then 1 else 0], " +
				"    [if gaveTreasureTo(people1, treasure2, people1) then 1 else 0], " +
				"    [if gaveTreasureTo(people1, treasure2, people2) then 1 else 0], " +
				"    [if gaveTreasureTo(people2, treasure1, people1) then 1 else 0], " +
				"    [if gaveTreasureTo(people2, treasure1, people2) then 1 else 0], " +
				"    [if gaveTreasureTo(people2, treasure2, people1) then 1 else 0], " +
				"    [if gaveTreasureTo(people2, treasure2, people2) then 1 else 0]  " +
				"  })" +
				")"), 
				groundedModelResult.getGroundedModel().getModelDefinition());
	}
	
	@Test
	public void testGroundModelEnsuresGeneratedConstantsDontClash() throws ModelGroundingException {
		GroundedModelResult groundedModelResult;
		
		groundedModelResult = callGroundModel(new Model(
				"model("+
		        " 'Name', " +
			    " 'Description', " +
				" sort(People)," +
				" sort(Treasure)," +
			    " randomVariable(gaveTreasureTo, 3, People, Treasure, People, Boolean)," +
				" parfactors("+
			    // Note: are using treasure1 and treasure2' as constant
				// names for People and not treasure.
				"  {{ (on Z in Treasure) [if gaveTreasureTo(treasure1, Z, treasure2') then 1 else 0] }}" +
				" )" +
	            ")",
				"gaveTreasureTo/3"),
				Util.map(parse("|People|"),   parse("2"),
						 parse("|Treasure|"), parse("2")));
		
		Assert.assertEquals(parse("model(" +
		        " 'Name', " +
			    " 'Description', " +
				" sort(People,   2, {treasure1,  treasure2'})," +
				" sort(Treasure, 2, {treasure1', treasure2})," +
				" randomVariable(gaveTreasureTo, 3, People, Treasure, People, Boolean)," +
				" parfactors("+
				// Note: the generated constants for Treasure sort are treasure1'
				// and treasure2, ensuring they don't clash with their use in the People sort.
				"  { [if gaveTreasureTo(treasure1,  treasure1',  treasure2') then 1 else 0], " +
				"    [if gaveTreasureTo(treasure1,  treasure2,   treasure2') then 1 else 0]  " +
				"  })" +
				")"), 
				groundedModelResult.getGroundedModel().getModelDefinition());
	}
	
	@Test
	public void testGroundModelRespectsIntensionalCondition() throws ModelGroundingException {
		GroundedModelResult groundedModelResult;
		
		groundedModelResult = callGroundModel(new Model(
				"model("+
		        " 'Name', " +
			    " 'Description', " +
				" sort(People,   2, {ann, bob})," +
				" sort(Treasure, 2, {gold, silver})," +
				" parfactors("+
				// Here we exclude factors where treasure = silver
				"  {{ (on X in People, Z in Treasure, Y in People) [if gaveTreasureTo(X, Z, Y) then 1 else 0] | Z != silver }}" +
				" )" +
	            ")",
				"gaveTreasureTo/3"));
		
		Assert.assertEquals(parse("model(" +
		        " 'Name', " +
			    " 'Description', " +
				" sort(People,   2, {ann, bob})," +
				" sort(Treasure, 2, {gold, silver})," +
				" randomVariable(gaveTreasureTo, 3, People, Treasure, People, Boolean)," +
				" parfactors("+
				"  { [if gaveTreasureTo(ann, gold, ann) then 1 else 0], " +
				"    [if gaveTreasureTo(ann, gold, bob) then 1 else 0], " +
				"    [if gaveTreasureTo(bob, gold, ann) then 1 else 0], " +
				"    [if gaveTreasureTo(bob, gold, bob) then 1 else 0]  " +
				"  })" +
				")"), 
				groundedModelResult.getGroundedModel().getModelDefinition());
	}
	
	private GroundedModelResult callGroundModel(Model modelDeclaration) throws ModelGroundingException {
		return callGroundModel(modelDeclaration, new LinkedHashMap<Object, Object>());
	}

	private GroundedModelResult callGroundModel(Model modelDeclaration, Map<Object, Object> globalObjects) throws ModelGroundingException {
		Expression modelDefinition = parse(modelDeclaration.getModelDeclaration());
		
		RewritingProcess process = newRewritingProcess(Expressions.ZERO);
		process.getGlobalObjects().putAll(globalObjects);
		
		process = Model.setRewritingProcessesModel(modelDefinition, modelDeclaration.getKnownRandomVariableNameAndArities(), process);
		
		GroundedModelResult groundedModelResult = ModelGrounding.groundModel(process);
		
		return groundedModelResult;
	}
}
