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
package com.sri.ai.praise.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityOfType;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.praise.PRAiSEConfiguration;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.CartesianProductEnumeration;

/**
 * A utility class for taking an LPI model and grounding it based on specified
 * sort counts.
 * 
 * @author oreilly
 * 
 */
@Beta
public class ModelGrounding {
	
	// Note: this class lets us add additional information
	// related to the grounding of the model that may
	// be of use to different export routines.
	public static class GroundedModelResult {
		private Model groundedModel = null;
		private Model groundedFrom  = null;
		private List<Pair<Expression, List<Expression>>> parfactorToGroundFactors =  null;
		private RewritingProcess process = null;
		
		public Model getGroundedModel() {
			return groundedModel;
		}
		
		public Model getGroundedFrom() {
			return groundedFrom;
		}
		
		public List<Pair<Expression, List<Expression>>> getParfactorToGroundFactors() {
			return parfactorToGroundFactors;
		}
		
		public RewritingProcess getRewritingProcess() {
			return process;
		}
		
		private GroundedModelResult(Model groundModel, Model groundedFrom, List<Pair<Expression, List<Expression>>> parfactorToGroundFactors, RewritingProcess process) {
			this.groundedModel            = groundModel;
			this.groundedFrom             = groundedFrom;
			this.parfactorToGroundFactors = parfactorToGroundFactors;
			this.process                  = process;
		}
	}

	public static class ModelGroundingError {
		public static enum TYPE {
			//
			NO_MODEL_DEFINITION_IN_PROCESS_TO_GROUND, 
			//
			FREE_VARIABLES, 
			//
			NOT_A_PARFACTOR, 
			//
			RANDOM_VARIABLE_VALUE_EXPRESSION_HAS_INCONSISTENT_ARITY,
			//
			RANDOM_VARIABLE_VALUE_EXPRESSION_HAS_INCONSISTENT_PARAMETER_SORTS,
			//
			INSUFFICIENT_RANDOM_VARIABLE_INFORMATION_FOR_GROUNDING,
			//
			SORT_CARDINALITY_CONFLICTS_WITH_PRESPECIFIED_SIZE,
			//
			CARDINALITY_OF_SORT_NOT_SPECIFIED,
			//
			UNABLE_TO_INSTANTIATE_A_VALID_SORT_DECLARATION_FOR_GROUNDING,
			//
			MODEL_ERROR_WHEN_CREATING_GROUND_MODEL,
			//
			GROUND_MODEL_EXCEEDS_MAX_ALLOWED_SIZE,
			//
			CANNOT_DETERMINE_GROUNDING_OF_INTENSIONAL_PARFACTOR,
		};

		//
		private TYPE errorType = null;
		private Expression inExpression = null;
		private List<Model.ModelError> modelErrors = new ArrayList<Model.ModelError>();

		public TYPE getErrorType() {
			return errorType;
		}

		public Expression getInExpression() {
			return inExpression;
		}
		
		public List<Model.ModelError> getModelErrors() {
			return modelErrors;
		}

		private ModelGroundingError(TYPE errorType, Expression inExpression) {
			this.errorType = errorType;
			this.inExpression = inExpression;
		}
		
		private ModelGroundingError(List<Model.ModelError> modelErrors, Expression inExpression) {
			this.modelErrors.addAll(modelErrors);
			this.errorType    = ModelGroundingError.TYPE.MODEL_ERROR_WHEN_CREATING_GROUND_MODEL;
			this.inExpression = inExpression;
		}
	}

	public static class ModelGroundingException extends Exception {
		private static final long serialVersionUID = 1L;
		private List<ModelGroundingError> errors = new ArrayList<ModelGroundingError>();

		public List<ModelGroundingError> getErrors() {
			return errors;
		}

		private ModelGroundingException(String message,
				List<ModelGroundingError> errors) {
			super(message);
			this.errors.addAll(errors);
		}
	}
	
	/**
	 * Ground the model specified by the given declaration, the cardinality of
	 * all of the sorts/types used in the model should be explicitly set in the
	 * model or passed in as arguments to this call (i.e. if the model does not
	 * define a cardinality for some or all of the sorts in its declaration).
	 * 
	 * @param modelDeclaration
	 *            the declaration of the model to be grounded.
	 * @param explicitSortSizes
	 *            a list of pairs comprising of (sort name, sort size).
	 * @return a GroundedModelResult
	 * @throws ModelGroundingException
	 *             thrown if errors are encountered when attempting to create
	 *             the grounded model.
	 */
	public static GroundedModelResult groundModel(String modelDeclaration, Pair... explicitSortSizes) 
			throws ModelGroundingException {
		// Can be called with explicitly assigned sizes to the sorts associated with the model
		Map<Expression, Expression> globalObjects = new LinkedHashMap<Expression, Expression>();
		if (explicitSortSizes != null) {
			for (Pair sortSize : explicitSortSizes) {
				globalObjects.put(Expressions.parse("| "+sortSize.first+" |"), Expressions.makeSymbol(sortSize.second));
			}
		}
		
		Expression modelDefinition = Expressions.parse(modelDeclaration);
		Model      modelToExport   = new Model(modelDefinition, Collections.emptySet());
		
		RewritingProcess process = LBPFactory.newLBPProcess(modelDefinition);
		process.getGlobalObjects().putAll(globalObjects);
		
		process = Model.setRewritingProcessesModel(modelDefinition, modelToExport.getKnownRandomVariableNameAndArities(), process);

		GroundedModelResult result = ModelGrounding.groundModel(process);
		
		return result;
	}

	/**
	 * Ground the model contained in the provided rewriting process, which
	 * should indicate the cardinality of all of the sorts/types used in the
	 * model.
	 * 
	 * @param process
	 *            the rewriting process containing the model to be grounded.
	 * @return a GroundedModelResult.
	 * @throws ModelGroundingException
	 *             thrown if errors are encountered when attempting to create a
	 *             grounded model.
	 */
	public static GroundedModelResult groundModel(RewritingProcess process)
			throws ModelGroundingException {
		return groundModel(Model.getModelInstance(process).getName(), 
						   Model.getModelInstance(process).getDescription(), process);
	}

	/**
	 * Ground the model contained in the provided rewriting process, which
	 * should indicate the cardinality of all of the sorts/types used in the
	 * model.
	 * 
	 * @param name
	 *            the name to be given to the grounded model.
	 * @param description
	 *            the description to be given to the grounded model.
	 * @param process
	 *            the rewriting process containing the model to be grounded.
	 * @return a GroundedModelResult.
	 * @throws ModelGroundingException
	 *             thrown if errors are encountered when attempting to create a
	 *             grounded model.
	 */
	public static GroundedModelResult groundModel(Expression name, Expression description, RewritingProcess process)
				throws ModelGroundingException {

		// Validation/Setup Steps:
		GroundModelDeclaration groundModelDeclaration = new GroundModelDeclaration(name, description,
				process);

		if (groundModelDeclaration.errors.size() > 0) {
			throw new ModelGroundingException(
					"Cannot ground an insufficiently defined model",
					groundModelDeclaration.errors);
		}
		
		//
		// Now Ground the model declaration
		Rewriter                                 normalize      = LBPFactory.newNormalize();
		Set<Expression>                          groundFactors = new LinkedHashSet<Expression>();
		List<Pair<Expression, List<Expression>>> parfactorToGroundFactors = new ArrayList<Pair<Expression, List<Expression>>>();
		for (ParfactorInformation parfactorInformation : groundModelDeclaration.parfactorMap.values()) {
			List<Expression> groundFactorsForParfactor = new ArrayList<Expression>();
			if (parfactorInformation.isIntensionalParfactor()) {
				// Each intensional set will require a test against the condition for
				// the current assignment before adding to an extensional set for the
				// grounding.
				boolean cannotDetermineIntensionalGrounding = false;
				if (parfactorInformation.indexToSortNameMap.size() == 0) {
					// Just a single grounding for this intensional set
					// as no free variables or indices. However,
					// still need to check the condition before
					// including it in the grounded set.
					Expression ifThenElse = IfThenElse.make(parfactorInformation.intensionalCondition, Expressions.TRUE, Expressions.FALSE);
					Expression conditionTest = normalize.rewrite(ifThenElse, process);
					if (Expressions.TRUE.equals(conditionTest)) {
						Expression groundFactor = Expressions.apply(BracketedExpressionSubExpressionsProvider.SYNTAX_TREE_LABEL, 
								parfactorInformation.intensionalFactorValueExpression);	
						groundFactorsForParfactor.add(groundFactor);
					} 
					else if (!Expressions.FALSE.equals(conditionTest)) {
						cannotDetermineIntensionalGrounding = true;
					}
				} 
				else {
					// Create the information needed to enumerate a cartesian product of the
					// sort information indexed by the parfactor
					List<List<Expression>> listOfListOfElements = new ArrayList<List<Expression>>();
					// Note: different indices can refer to the same sort, so you need
					// to populate in this direction in order to ensure a valid grounding.
					for (Expression indexName : parfactorInformation.indexToSortNameMap.keySet()) {
						Expression sortName = parfactorInformation.indexToSortNameMap.get(indexName);
						List<Expression> sortConstants = new ArrayList<Expression>(groundModelDeclaration.sortMap.get(sortName).constants);
						listOfListOfElements.add(sortConstants);
					}
					CartesianProductEnumeration<Expression> cpe = new CartesianProductEnumeration<Expression>(listOfListOfElements);
					while (cpe.hasMoreElements()) {
						// Now substitute in the current ground constants into the factor and conditional
						// expressions
						List<Expression> row = cpe.nextElement();
						Map<Expression, Expression> replacements = new LinkedHashMap<Expression, Expression>();
						int i = 0;
						for (Expression indexName : parfactorInformation.indexToSortNameMap.keySet()) {
							replacements.put(indexName, row.get(i));
							i++;
						}
						Expression groundFactorValue = SemanticSubstitute.replaceAll(parfactorInformation.intensionalFactorValueExpression, replacements, process);
						Expression condition         = SemanticSubstitute.replaceAll(parfactorInformation.intensionalCondition, replacements, process);
						Expression ifThenElse = IfThenElse.make(condition, Expressions.TRUE, Expressions.FALSE);
						Expression conditionTest = normalize.rewrite(ifThenElse, process);
						if (Expressions.TRUE.equals(conditionTest)) {
							Expression groundFactor = Expressions.apply(BracketedExpressionSubExpressionsProvider.SYNTAX_TREE_LABEL, 
									groundFactorValue);	
							groundFactorsForParfactor.add(groundFactor);
						} 
						else if (!Expressions.FALSE.equals(conditionTest)) {
							cannotDetermineIntensionalGrounding = true;
							break;
						}
					}
				}
				
				if (cannotDetermineIntensionalGrounding) {
					throw new ModelGroundingException("Cannot determine grounding of an intensional parfactor",
							Arrays.asList(new ModelGroundingError(
									ModelGroundingError.TYPE.CANNOT_DETERMINE_GROUNDING_OF_INTENSIONAL_PARFACTOR, 
									parfactorInformation.parfactor)));
				}
			} 
			else {
				// All extensional sets remain as is (as will have no free variables)
				groundFactorsForParfactor.addAll(ExtensionalSet.getElements(parfactorInformation.parfactor));
			}
			
			groundFactors.addAll(groundFactorsForParfactor);
			
			parfactorToGroundFactors.add(new Pair<Expression, List<Expression>>(parfactorInformation.parfactor, groundFactorsForParfactor));
			
			// Ensure the grounded model is not too large
			if (PRAiSEConfiguration.isPerformMaxAllowedSizeCheckForGroundedModel()
				&& groundFactors.size() > PRAiSEConfiguration.getMaxAllowedSizeForGroundedModel()) {
				
				throw new ModelGroundingException("Ground model size so far of "+groundFactors.size()+" exceeds maximum allowed size of " + PRAiSEConfiguration.getMaxAllowedSizeForGroundedModel(),
						Arrays.asList(new ModelGroundingError(
								ModelGroundingError.TYPE.GROUND_MODEL_EXCEEDS_MAX_ALLOWED_SIZE, 
								Expressions.makeSymbol(PRAiSEConfiguration.getMaxAllowedSizeForGroundedModel()))));
			}
		}
		

		// Construct the grounded model from the ground factors
		Expression groundParfactor = ExtensionalSet.makeUniSetExpression(new ArrayList<Expression>(groundFactors));
		ParfactorsDeclaration groundParfactorsDeclaration = ParfactorsDeclaration.makeParfactorsDeclaration(groundParfactor);
		
		Model groundedModel = Model.constructFromParts(name, 
				description, 
				groundModelDeclaration.preGroundModel.getSortDeclarations(), 
				groundModelDeclaration.preGroundModel.getRandomVariableDeclarations(), 
				groundParfactorsDeclaration, 
				groundModelDeclaration.preGroundModel.getKnownRandomVariableNameAndArities()); 
				
		GroundedModelResult groundedModelResult = new GroundedModelResult(groundedModel, groundModelDeclaration.groundedFrom, parfactorToGroundFactors, process);
		
		return groundedModelResult;
	}

	//
	//
	private static class GroundModelDeclaration {
		// Keep track of the model from which this is to be grounded from
		public Model groundedFrom = null;
		public List<ModelGroundingError> errors = new ArrayList<ModelGroundingError>();
		public Map<Expression, SortInformation> sortMap = new LinkedHashMap<Expression, SortInformation>();
		public Map<Expression, RandomVariableInformation> randomVariableMap = new LinkedHashMap<Expression, RandomVariableInformation>();
		public Map<Expression, ParfactorInformation> parfactorMap = new LinkedHashMap<Expression, ParfactorInformation>();
		// This model will contain no parfactors but will contain all of the necessary
		// sort and random variable declaration information needed to ground the model.
		public Model preGroundModel = null;
		
		private GroundModelDeclaration(Expression name, Expression description, RewritingProcess process) {
			Expression fromModelDefinition = Model.getModelDefinition(process);
			if (fromModelDefinition == null) {
				errors.add(new ModelGroundingError(
						ModelGroundingError.TYPE.NO_MODEL_DEFINITION_IN_PROCESS_TO_GROUND,
						Expressions.ZERO));
			} 
			else {
				groundedFrom = new Model(fromModelDefinition,
						Model.getKnownRandomVariableNameAndArities(process));

				ensureNoFreeVariables(groundedFrom, process);

				if (errors.size() == 0) {
					ensureAreParfactorsAndNotRandomVariables(groundedFrom, process);
				}
				
				if (errors.size() == 0) {
					collectAndValidateSortRandomVariableAndParfactorInformation(groundedFrom, process);
				}
				
				if (errors.size() == 0) {
					// Ensure ground cardinality is specified for all of the sorts
					ensureSortCardinalitiesSetAndComply(process);
				}
				
				if (errors.size() == 0) {
					ensureCanGroundLegally(groundedFrom, name, description);
				}
			}
		}

		private void ensureNoFreeVariables(Model model, RewritingProcess process) {
			for (Expression parfactor : model.getParfactorsDeclaration()
					.getParfactors()) {
				Set<Expression> freeVariables = Expressions.freeVariables(parfactor, process);
				if (freeVariables.size() > 0) {
					// Ensure the free variables returned are not sort names
					Set<Expression> sorts = new LinkedHashSet<Expression>();
					if (Sets.isIntensionalSet(parfactor)) {
						sorts.addAll(IndexExpressions.getIndexToTypeMapWithDefaultNull(parfactor).values());
					}
					if (!sorts.containsAll(freeVariables)) {
						errors.add(new ModelGroundingError(
							ModelGroundingError.TYPE.FREE_VARIABLES, parfactor));
					}
				}
			}
		}

		private void ensureAreParfactorsAndNotRandomVariables(Model model,
				RewritingProcess process) {
			for (Expression parfactor : model.getParfactorsDeclaration()
					.getParfactors()) {
				if (!ParfactorsDeclaration.isParfactor(parfactor, process)) {
					errors.add(new ModelGroundingError(
							ModelGroundingError.TYPE.NOT_A_PARFACTOR, parfactor));
				}
			}
		}
		
		private void collectAndValidateSortRandomVariableAndParfactorInformation(Model model, 
				RewritingProcess process) {
			// Collect the known sort information from the model
			for (SortDeclaration sortDeclaration : model.getSortDeclarations()) {
				sortMap.put(sortDeclaration.getName(), new SortInformation(sortDeclaration));
			}
			
			// Collect the known random variable information from the model
			for (RandomVariableDeclaration randomVariableDeclaration : model.getRandomVariableDeclarations()) {
				randomVariableMap.put(randomVariableDeclaration.getName(), 
						new RandomVariableInformation(randomVariableDeclaration));
			}
			
			// Collect parfactor information from the model
			for (Expression parfactor : model.getParfactorsDeclaration().getParfactors()) {
				ParfactorInformation parfactorInformation = new ParfactorInformation(parfactor, process);
				parfactorMap.put(parfactor, parfactorInformation);
			}
			
			// Derive implied sort information from the parfactors
			for (ParfactorInformation parfactorInformation : parfactorMap.values()) {
				for (Map.Entry<Expression, Expression> indexToSortName : parfactorInformation.indexToSortNameMap.entrySet()) {
					Expression sortName = indexToSortName.getValue();
					if (!sortMap.containsKey(sortName)) {
						SortInformation sortInformation = new SortInformation(sortName);
						sortMap.put(sortName, sortInformation);
					}
				}
			}
			
			// Derive implied random variable information from the parfactors 
			for (ParfactorInformation parfactorInformation : parfactorMap.values()) {
				for (Expression randomVariableValueExpression : parfactorInformation.randomVariableValueExpressions) {
					Expression functorOrSymbol = randomVariableValueExpression.getFunctorOrSymbol();
					RandomVariableInformation randomVariableInformation = randomVariableMap.get(functorOrSymbol);
					if (randomVariableInformation == null) {
						randomVariableInformation = new RandomVariableInformation(functorOrSymbol, randomVariableValueExpression.numberOfArguments());
						randomVariableMap.put(functorOrSymbol, randomVariableInformation);
					} 
					else {
						// Check that the random variable value expression is consistent with the
						// its known declaration so far.
						randomVariableInformation.isArityConsistentWith(randomVariableValueExpression, errors);
					}
					// Now check the sort information
					randomVariableInformation.areParamameterSortsConsistent(randomVariableValueExpression, parfactorInformation, errors, process);
				}
			}
			
			// Ensure that I have enough random variable information for grounding
			for (RandomVariableInformation randomVariableInformation : randomVariableMap.values()) {
				if (!randomVariableInformation.isFullyDefined()) {
					errors.add(new ModelGroundingError(ModelGroundingError.TYPE.INSUFFICIENT_RANDOM_VARIABLE_INFORMATION_FOR_GROUNDING, randomVariableInformation.name));
				}
			}
			
			//
			// Only proceed with filling out sort and parfactor information
			// if all of the required random variable information has been collected
			if (errors.size() == 0) {
				for (ParfactorInformation parfactorInformation : parfactorMap.values()) {
					for (Expression randomVariableValueExpression : parfactorInformation.randomVariableValueExpressions) {
						Expression randomVariableName = randomVariableValueExpression.getFunctorOrSymbol();
						RandomVariableInformation randomVariableInformation = randomVariableMap.get(randomVariableName);
						for (int i = 0; i < randomVariableValueExpression.numberOfArguments(); i++) {
							Expression arg = randomVariableValueExpression.get(i);
							if (process.isVariable(arg)) {
								// Update the parfactors index to sort map
								if (!parfactorInformation.indexToSortNameMap.containsKey(arg)) {
									parfactorInformation.indexToSortNameMap.put(arg, randomVariableInformation.parameterSorts[i]);
								}
							} 
							else {
								// is constant, therefore map to appropriate sort
								SortInformation sortInformation = sortMap.get(randomVariableInformation.parameterSorts[i]);
								sortInformation.constants.add(arg);
							}
						}
					}
				}
			}
		}
		
		private void ensureSortCardinalitiesSetAndComply(RewritingProcess process) {
			
			Set<Expression> knownConstants = new LinkedHashSet<Expression>();
			for (SortInformation sortInformation : sortMap.values()) {
				knownConstants.addAll(sortInformation.constants);
			}
			
			// Check if we are using upper or lower case convention
			// for logical variables.
			Expression upperCaseSymbol = Expressions.makeSymbol("X");
			boolean isUpperCaseVariable = process.isVariable(upperCaseSymbol);
			
			for (SortInformation sortInformation : sortMap.values()) {
				boolean ok = true;
				Integer size = Model.getCardinalityOfSort(process, sortInformation.name);
				if (size == null && sortInformation.size == null) {
					errors.add(new ModelGroundingError(ModelGroundingError.TYPE.CARDINALITY_OF_SORT_NOT_SPECIFIED, sortInformation.name));
					ok = false;
				} 
				else {
					if (sortInformation.size == null) {
						sortInformation.size = size;
					} 
					else {
						if (size != null && sortInformation.size != size) {
							errors.add(new ModelGroundingError(ModelGroundingError.TYPE.SORT_CARDINALITY_CONFLICTS_WITH_PRESPECIFIED_SIZE, sortInformation.name));
							ok = false;
						}
					}
				}
				if (ok) {
					String constantPrefix = sortInformation.name.toString();
					if (isUpperCaseVariable) {
						constantPrefix = constantPrefix.toLowerCase();
					} 
					else {
						constantPrefix = constantPrefix.toUpperCase();
					}
					// Now fill out the constants for this sort
					while (sortInformation.constants.size() < sortInformation.size) {
						Expression newGroundSortConstant = Expressions.makeSymbol(constantPrefix+(sortInformation.constants.size()+1));
						while (knownConstants.contains(newGroundSortConstant)) {
							newGroundSortConstant = Expressions.makeSymbol(newGroundSortConstant.toString()+"'"); // Prime it.
						}
						knownConstants.add(newGroundSortConstant);
						sortInformation.constants.add(newGroundSortConstant);
					}
					// Instantiate the sort declaration as we have sufficient information
					sortInformation.instantiateGroundDeclaration(errors);
				}
			}
		}
		
		private void ensureCanGroundLegally(Model model, Expression name, Expression description) {
			List<Expression> preGroundModelParts = new ArrayList<Expression>();
			Set<String> knownRandomVariableNameAndArities = new LinkedHashSet<String>();
			
			if (name == null) {
				// Default to the name given to the model being ground from
				preGroundModelParts.add(model.getName());
			} 
			else {
				preGroundModelParts.add(name);
			}
			if (description == null) {
				// Default to the description given to the model being ground from
				preGroundModelParts.add(model.getDescription());
			} 
			else {
				preGroundModelParts.add(description);
			}
			
			for (SortInformation sortInformation : sortMap.values()) {
				preGroundModelParts.add(sortInformation.getGroundDeclaration().getSortDeclaration());
			}

			for (RandomVariableInformation randomVariableInformation : randomVariableMap.values()) {
				preGroundModelParts.add(randomVariableInformation.getGroundDeclaration().getRandomVariableDeclaration());
			}
			
			// None: will be specified initially as empty
			preGroundModelParts.add(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ParfactorsDeclaration.FUNCTOR_PARFACTORS_DECLARATION));
			
			Expression preGroundModelDefinition = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(Model.FUNCTOR_MODEL_DECLARATION, preGroundModelParts.toArray());
			
			try {
				preGroundModel = new Model(preGroundModelDefinition, knownRandomVariableNameAndArities);
			} catch (Model.ModelException mex) {
				errors.add(new ModelGroundingError(mex.getErrors(), preGroundModelDefinition));
			}
		}
	}

	private static class SortInformation {
		public  Expression name = null;
		public  Integer size = null;
		public  Set<Expression> constants = new LinkedHashSet<Expression>();
		private SortDeclaration groundDeclaration = null;
		
		
		public SortInformation(Expression name) {
			this.name = name;
		}
		
		public SortInformation(SortDeclaration declaration) {	
			this.name = declaration.getName();
			for (Expression constant : ExtensionalSet.getElements(declaration.getConstants())) {
				constants.add(constant);
			}
			size = getSize(declaration.getSize());
		}
		
		public Integer getSize(Expression sizeExpression) {
			Integer size = null;
			if (!SortDeclaration.UNKNOWN_SIZE.equals(sizeExpression)) {
				size = sizeExpression.intValue();
			}
			return size;
		}
		
		public SortDeclaration getGroundDeclaration() {
			return groundDeclaration;
		}
		
		public void instantiateGroundDeclaration(List<ModelGroundingError> errors) {
			try {
				groundDeclaration = new SortDeclaration(name, 
						Expressions.makeSymbol(size), 
						ExtensionalSet.makeUniSetExpression(new ArrayList<Expression>(constants)));
			} catch (IllegalArgumentException iae) {
				errors.add(new ModelGroundingError(ModelGroundingError.TYPE.UNABLE_TO_INSTANTIATE_A_VALID_SORT_DECLARATION_FOR_GROUNDING, name));
			}
		}
	}
	
	private static class RandomVariableInformation {
		public Expression   name  = null;
		public int          arity = 0;
		public Expression[] parameterSorts = null;
		private RandomVariableDeclaration groundDeclaration = null;
		
		public RandomVariableInformation(Expression name, int arity) {
			this.name           = name;
			this.arity          = arity;
			this.parameterSorts = new Expression[arity];
		}
		
		public RandomVariableInformation(RandomVariableDeclaration declaration) {
			this.groundDeclaration = declaration;
			this.name  = declaration.getName();
			this.arity = declaration.getArityValue();
			this.parameterSorts = new Expression[this.arity];
			for (int i = 0; i < parameterSorts.length; i++) {
				parameterSorts[i] = declaration.getParameterSorts().get(i);
			}
		}
		
		public RandomVariableDeclaration getGroundDeclaration() {
			if (isFullyDefined() && groundDeclaration == null) {
				if (arity == 0) {
					groundDeclaration = new RandomVariableDeclaration(name);
				} 
				else {
					groundDeclaration = new RandomVariableDeclaration(name, Expressions.makeSymbol(arity), parameterSorts);
				}
			}
			return groundDeclaration;
		}
		
		public boolean isFullyDefined() {
			boolean result = true;
			for (int i = 0; i < parameterSorts.length; i++) {
				// If I don't have sort information for a parameter
				// then it is underdefined.
				if (parameterSorts[i] == null) {
					result = false;
					break;
				}
			}
			
			return result;
		}
		
		public boolean isArityConsistentWith(Expression randomVariableValueExpression, List<ModelGroundingError> errors) {
			boolean result = true;
			
			if (arity != randomVariableValueExpression.numberOfArguments()) {
				result = false;
				errors.add(new ModelGroundingError(ModelGroundingError.TYPE.RANDOM_VARIABLE_VALUE_EXPRESSION_HAS_INCONSISTENT_ARITY, randomVariableValueExpression));
			} 
			
			return result;
		}
		
		public boolean areParamameterSortsConsistent(Expression randomVariableValueExpression, ParfactorInformation parfactorInformation, List<ModelGroundingError> errors, RewritingProcess process) {
			boolean result = true;
			
			if (arity == randomVariableValueExpression.numberOfArguments()) {
				for (int i = 0; i < arity; i++) {
					Expression arg = randomVariableValueExpression.get(i);
					// If a logical variable, check and see if
					// maps to a sort from the scoping expressions
					if (process.isVariable(arg)) {
						Expression sortName = parfactorInformation.indexToSortNameMap.get(arg);
						if (sortName != null) {
							if (parameterSorts[i] == null) {
								// Am discovering parameter sort information
								parameterSorts[i] = sortName;
							} 
							else {
								// Must be consistent
								if (!sortName.equals(parameterSorts[i])) {
									result = false;
									errors.add(new ModelGroundingError(ModelGroundingError.TYPE.RANDOM_VARIABLE_VALUE_EXPRESSION_HAS_INCONSISTENT_PARAMETER_SORTS, randomVariableValueExpression));
									break;
								}
							}
						}
					}
				}
			}
			
			return result;
		}
	}
	
	private static class ParfactorInformation {
		public Expression parfactor                            = null;
		public Expression intensionalFactorValueExpression     = null;
		public Expression intensionalCondition                 = null;
		public Map<Expression, Expression> indexToSortNameMap  = new LinkedHashMap<Expression, Expression>();
		public List<Expression> randomVariableValueExpressions = new ArrayList<Expression>();
		
		public ParfactorInformation(Expression parfactor, RewritingProcess process) {
			this.parfactor = parfactor;
			// Extract relevant information to aid grounding
			// from the parfactor.
			if (isIntensionalParfactor()) {
				Expression intensionalFactor = ((IntensionalSet) parfactor).getHead();
				intensionalCondition = ((IntensionalSet) parfactor).getCondition();
				Map<Expression, Expression> indexToTypeMap = IndexExpressions.getIndexToTypeMapWithDefaultNull(parfactor);
				// Add type names that correspond to sort names
				for (Map.Entry<Expression, Expression> indexToDomain : indexToTypeMap.entrySet()) {
					Expression possibleSortName = indexToDomain.getValue();
					// Exclude 'null' and 'type(<Logical Variable>)' types as these do not
					// correspond to sort names.
					if (possibleSortName != null && !CardinalityOfType.isTypeSyntacticFunctionApplication(possibleSortName)) {
						indexToSortNameMap.put(indexToDomain.getKey(), possibleSortName);
					}
				}
				intensionalFactorValueExpression = ((BracketedExpression) intensionalFactor).getInnerExpression();
				Iterator<Expression> randomVariableValueIterator = GetRandomVariables.getRandomVariableValueExpressionsIterator(intensionalFactorValueExpression, process);
				while (randomVariableValueIterator.hasNext()) {
					randomVariableValueExpressions.add(randomVariableValueIterator.next());
				}
			}
		}
		
		public boolean isIntensionalParfactor() {
			return Sets.isIntensionalSet(parfactor);
		}
	}
}
