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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.praise.PRAiSEConfiguration;

/**
 * A class for representing the definition of a declared model. The basic
 * structure of a model declaration is as follows:<br>
 * 
 * <pre>
 * model(                    
 *     // Optional: Name argument
 *     'Name',              
 *     // Optional: Description (name must be specified first) 
 *     'Description',       
 *     
 *     // Optional: 0 or more sort declarations.
 *     sort(name, size {, constants}),
 *     
 *     // Optional: 0 or more parametric random variable declarations
 *     randomVariable(name, arity, parameterSortName1,...,parameterSortNameN, rangeSortName),
 *     
 *     // Mandatory: A single parfactors (parametric factors) declaration.
 *     parfactors(  
 *         'union or partition'(parfactor1,...,parfactorN)
 *     )
 * )
 * </pre>
 * 
 * @see SortDeclaration
 * @see RandomVariableDeclaration
 * @see ParfactorsDeclaration
 * 
 * @author oreilly
 */
@Beta
public class Model {
	
	public static class ModelError {
		public static enum TYPE {
			//
			TOO_MANY_STRING_PARAMETERS,
			//
			MORE_THAN_1_PARFACTORS_DECLARATION,
			//
			UNRECOGNIZED_MODEL_PART,
			//
			UNRECOGNIZED_MODEL_DEFINITION,
			//
			SORT_NAME_NOT_UNIQUE,
			//
			CONSTANT_NAME_NOT_UNIQUE,
			//
			RANDOM_VARIABLE_NAME_NOT_UNIQUE,
			//
			RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED,
			//
			CONSTANT_IN_MORE_THAN_1_SORT
		};

		
		//
		private TYPE errorType = null;
		private Expression inExpression = null;

		public TYPE getErrorType() {
			return errorType;
		}

		public Expression getInExpression() {
			return inExpression;
		}

		private ModelError(TYPE errorType, Expression inExpression) {
			this.errorType = errorType;
			this.inExpression = inExpression;
		}
		
		public String toString() {
			String inExpressionString = inExpression.toString();
			String typeString = errorType.name();
			String result = typeString + " in " + inExpressionString;
			return result;
		}
	}

	public static class ModelException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		private List<ModelError> errors = new ArrayList<ModelError>();

		public List<ModelError> getErrors() {
			return errors;
		}

		private ModelException(String message, List<ModelError> errors) {
			super(message);
			this.errors.addAll(errors);
		}
	}

	public static final String FUNCTOR_MODEL_DECLARATION = "model";

	// GLOBAL OBJECTS KEY
	public static final String GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAMES = "known random variable names";
	public static final String GLOBAL_KEY_MODEL = "model";
	public static final String GLOBAL_KEY_MODEL_PARFACTORS = "model parfactors";
	public static final String GLOBAL_KEY_MODEL_RANDOM_PREDICATE_CATALOG = "model random predicate catalog";
	public static final String GLOBAL_KEY_MODEL_CARDINALITY_OF_TYPES_ALWAYS_GREATER_THAN_ZERO = "model cardinality of types always greater than zero";

	// Note: For now the range of all random variables is restricted to {false,
	// true}.
	private static List<Expression> _defaultRVRange = new ArrayList<Expression>();

	static {
		_defaultRVRange.add(Expressions.FALSE);
		_defaultRVRange.add(Expressions.TRUE);
		_defaultRVRange = Collections.unmodifiableList(_defaultRVRange);
	}

	//
	// PRIVATE
	//
	// Declaration information
	private String modelDeclaration = null;
	private Set<String> knownRandomVariableNames = new HashSet<String>();
	// Definition information.
	private Expression modelDefinition = null;
	private Expression name = null;
	private Expression description = null;
	private List<SortDeclaration> sortDeclarations = new ArrayList<SortDeclaration>();
	private List<RandomVariableDeclaration> randomVariableDeclarations = new ArrayList<RandomVariableDeclaration>();
	private ParfactorsDeclaration parfactorsDeclaration = null;

	/**
	 * Declare a model without defining it (useful for caching purposes).
	 * 
	 * @param modelDeclaration
	 *            a string representation of the model's declaration.
	 * @param knownRandomVariableNames
	 *            a list of the known random variables in the model.
	 */
	public Model(String modelDeclaration, String... knownRandomVariableNames) {
		this.modelDeclaration = modelDeclaration;
		for (String name : knownRandomVariableNames) {
			this.knownRandomVariableNames.add(name);
		}
		this.knownRandomVariableNames = Collections
				.unmodifiableSet(this.knownRandomVariableNames);
	}

	/**
	 * Construct a model from a model definition expression.
	 * 
	 * @param modelDefinition
	 *            an expression in one of the following forms:
	 *            <ul>
	 *            <li>union(parfactor1,...,parfactorN) - i.e. just the model's
	 *            parfactors.</li>
	 *            <li>partition(parfactor1,...,parfactorN) - i.e. just the
	 *            model's parfactors.</li>
	 *            <li>model(union(parfactor1,...,parfactorN))</li>
	 *            <li>model(partition(parfactor1,...,parfactorN))</li>
	 *            <li>model(union(parfactor1,...,parfactorN))</li>
	 *            <li>and the more expanded form detailed in the class comments.
	 *            </li>
	 *            </ul>
	 * 
	 * @throws a
	 *             ModelException if passed an illegal Model Definition
	 *             expression.
	 */
	public Model(Expression modelDefinition,
			Set<String> knownRandomVariableNames) {
		this.modelDeclaration = modelDefinition.toString();
		if (knownRandomVariableNames != null) {
			this.knownRandomVariableNames.addAll(knownRandomVariableNames);
		}
		this.modelDefinition = modelDefinition;

		collectAndValidateModelParts();
	}
	
	/**
	 * A copy constructor that lets you override the size information associated
	 * with the model's sort declarations.
	 * 
	 * @param toCopy
	 * @param knownDomainSize
	 * @param size
	 */
	public Model(Model toCopy, boolean knownDomainSize, Integer size) {
		List<Object> args = new ArrayList<Object>();
		this.knownRandomVariableNames.addAll(toCopy.knownRandomVariableNames);
		this.name = toCopy.name;
		if (this.name != null) {
			args.add(this.name);
		}
		this.description = toCopy.description;
		if (this.description != null) {
			args.add(this.description);
		}
		this.randomVariableDeclarations.addAll(toCopy.randomVariableDeclarations);
		for (RandomVariableDeclaration rvd : this.randomVariableDeclarations) {
			args.add(rvd);
		}
		this.parfactorsDeclaration = toCopy.parfactorsDeclaration;
		args.add(this.parfactorsDeclaration);
		
		// Change the sort declarations
		for (SortDeclaration toCopySortDeclaration : toCopy.sortDeclarations) {
			SortDeclaration copySD = new SortDeclaration(toCopySortDeclaration, knownDomainSize, size);
			this.sortDeclarations.add(copySD);
			args.add(copySD);
		}
		this.modelDeclaration = null;
		this.modelDefinition = Expressions.make(FUNCTOR_MODEL_DECLARATION, args);
	}

	/**
	 * 
	 * @return the declaration of the model.
	 */
	public String getModelDeclaration() {
		if (modelDeclaration == null) {
			StringBuilder md = new StringBuilder();
			md.append(FUNCTOR_MODEL_DECLARATION);
			md.append("(");
			if (this.name != null) {
				md.append(this.name.toString()+",\n");
			}
			if (this.description != null) {
				md.append(this.description.toString()+",\n");
			}
			for (RandomVariableDeclaration rvd : this.randomVariableDeclarations) {
				md.append(rvd.getRandomVariableDeclaration().toString());
				md.append(",\n");
			}
			for (SortDeclaration sd: this.sortDeclarations) {
				md.append(sd.getSortDeclaration().toString());
				md.append(",\n");
			}
			md.append(this.parfactorsDeclaration.getDefinition().toString());
			
			md.append(")");
			
			modelDeclaration = md.toString();
		}
		return modelDeclaration;
	}

	/**
	 * 
	 * @return the known random variable names associated with the model.
	 */
	public Set<String> getKnownRandomVariableNames() {
		return knownRandomVariableNames;
	}

	/**
	 * 
	 * @return true if the model is defined (i.e. is represented as an
	 *         expression) or not;
	 */
	public boolean isDefined() {
		return modelDefinition != null;
	}

	/**
	 * 
	 * @return an Expression representing the definition of the model.
	 */
	public Expression getModelDefinition() {
		assertDefined();

		return modelDefinition;
	}

	/**
	 * 
	 * @return the model definition's name.
	 */
	public Expression getName() {
		assertDefined();

		return name;
	}

	/**
	 * 
	 * @return a description of the model definition.
	 */
	public Expression getDescription() {
		assertDefined();

		return description;
	}

	/**
	 * 
	 * @return the sort declarations associated with the model.
	 */
	public List<SortDeclaration> getSortDeclarations() {
		assertDefined();

		return Collections.unmodifiableList(sortDeclarations);
	}

	/**
	 * @return the random variable declarations associated with the model.
	 */
	public List<RandomVariableDeclaration> getRandomVariableDeclarations() {
		assertDefined();

		return randomVariableDeclarations;
	}

	/**
	 * 
	 * @return the parfactors declartion associated with the model.
	 */
	public ParfactorsDeclaration getParfactorsDeclaration() {
		assertDefined();

		return parfactorsDeclaration;
	}

	/**
	 * Set the rewriting process's model and known random variable names for
	 * this Example Model.
	 * 
	 * @param process
	 *            the process in which the rewriting is occurring.
	 */
	public void setRewritingProcessesModel(RewritingProcess process) {
		assertDefined();

		setRewritingProcessesModel(modelDefinition, knownRandomVariableNames,
				process);
	}

	//
	// STATIC UTILITY METHODS for accessing Model information from a running
	// RewritingProcess.
	//
	
	/**
	 * Determine if the cardinality of all | type(.) | expressions should always
	 * be considered > 0.
	 * 
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return true if the cardinality of | type(.) | expressions should always
	 *         be considered > 0, false otherwise.
	 */
	public static boolean isCardinalityOfTypesAlwaysGreaterThanZero(RewritingProcess process) {
		Boolean result = (Boolean) process.getGlobalObject(GLOBAL_KEY_MODEL_CARDINALITY_OF_TYPES_ALWAYS_GREATER_THAN_ZERO);
		if (result == null) {
			result = PRAiSEConfiguration.isCardinalityOfTypesAlwaysGreaterThanZero();
			process.putGlobalObject(GLOBAL_KEY_MODEL_CARDINALITY_OF_TYPES_ALWAYS_GREATER_THAN_ZERO, result);
		}
		
		return result;
	}
	
	/**
	 * Set whether or not the cardinality of all | type(.) | expressions should
	 * always be considered > 0 for the model associated with the process.
	 * 
	 * @param isAlways
	 *            true if the cardinality of | type(.) | expressions should
	 *            always be considered > 0, false otherwise.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 */
	public static void setCardinalityOfTypesAlwaysGreaterThanZero(boolean isAlways, RewritingProcess process) {
		process.putGlobalObject(GLOBAL_KEY_MODEL_CARDINALITY_OF_TYPES_ALWAYS_GREATER_THAN_ZERO, isAlways);
	}

	/**
	 * Get the range associated with a Random Variable.
	 * 
	 * @param aRandomVariableValueExpression
	 *            the value expression of a Random Variable, i.e v in [v].
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return the range of the Random Variable [ v ].
	 */
	public static List<Expression> range(
			Expression aRandomVariableValueExpression, RewritingProcess process) {
		return _defaultRVRange;
	}

	/**
	 * Get the known Random Variable names associated with the rewriting
	 * process's model.
	 * 
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return the set of known random variable names associated with the
	 *         rewriting process's model.
	 */
	public static Set<String> getKnownRandomVariableNames(
			RewritingProcess process) {
		@SuppressWarnings("unchecked")
		Set<String> krvns = (Set<String>) process
				.getGlobalObject(GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAMES);
		if (null == krvns) {
			krvns = Collections.emptySet();
		}
		return krvns;
	}

	/**
	 * Set the rewriting process's model.
	 * 
	 * @param modelExpression
	 *            an expression representing the model the rewriting process is
	 *            working over.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 */
	public static void setRewritingProcessesModel(Expression modelExpression,
			RewritingProcess process) {

		@SuppressWarnings("unchecked")
		Set<String> knownVarNames = (Set<String>) process
				.getGlobalObject(GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAMES);
		if (knownVarNames == null) {
			knownVarNames = new HashSet<String>();
		}
		setRewritingProcessesModel(modelExpression, knownVarNames, process);
	}

	/**
	 * Set the rewriting process's model and known random variable names.
	 * 
	 * @param modelDefinitionExpression
	 *            an expression representing the model the rewriting process is
	 *            working over.
	 * @param knownRandomVariableNames
	 *            the set of known random variable names in a model.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 */
	public static void setRewritingProcessesModel(
			Expression modelDefinitionExpression,
			Set<String> knownRandomVariableNames, RewritingProcess process) {

		Model model = new Model(modelDefinitionExpression, knownRandomVariableNames);
		
		CardinalityTypeOfLogicalVariable.registerDomainSizeOfLogicalVariableWithProcess(
				new ModelLookupDomainSizeOfLogicalVariable(model), process);

		process.putGlobalObject(GLOBAL_KEY_MODEL, model.getModelDefinition());
		process.putGlobalObject(GLOBAL_KEY_MODEL_PARFACTORS, model
				.getParfactorsDeclaration().getParfactors());

		// Handle case where names may have been setup globally
		// outside of the model and process.
		@SuppressWarnings("unchecked")
		Set<String> knownVarNames = (Set<String>) process
				.getGlobalObject(GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAMES);
		if (knownVarNames == null) {
			knownVarNames = model.getKnownRandomVariableNames();
		} 
		else {
			knownVarNames.addAll(model.getKnownRandomVariableNames());
		}
		
		process.putGlobalObject(GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAMES,
				knownVarNames);
	}

	/**
	 * Get the expression representing the model definition associated with the
	 * process.
	 * 
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return the model definition expression.
	 */
	public static Expression getModelDefinition(RewritingProcess process) {
		Expression model = (Expression) process.getGlobalObject(GLOBAL_KEY_MODEL);
		return model;
	}
	
	public static boolean isAllDomainSizesKnown(RewritingProcess process) {
		return PRAiSEConfiguration.isAllTypeSizesKnownInModel();
	}
	
	public static RandomPredicateCatalog getRandomPredicateCatalog(RewritingProcess process) {
		RandomPredicateCatalog catalog = (RandomPredicateCatalog) process
				.getGlobalObject(GLOBAL_KEY_MODEL_RANDOM_PREDICATE_CATALOG);
		
		if (catalog == null) {
			Expression model = Model.getModelDefinition(process);
			// Can only create if a model definition is included in the process.
			if (model != null) {
				List<Expression> setsOfFactors = Model.getParfactors(process);
				catalog = new RandomPredicateCatalog(setsOfFactors, process);
				// Cache in the process
				process.putGlobalObject(GLOBAL_KEY_MODEL_RANDOM_PREDICATE_CATALOG, catalog);
			}
			else {
				throw new Error("Random predicate catalog requested but rewriting process does not contain a global object named '" + GLOBAL_KEY_MODEL + "'");
			}
		}
		
		return catalog;
	}

	/**
	 * Get the parfactors from the model.
	 * 
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return the parfactors in the model (extensionally or intensionally
	 *         defined sets of factors).
	 */
	@SuppressWarnings("unchecked")
	public static List<Expression> getParfactors(RewritingProcess process) {
		List<Expression> parfactors = new ArrayList<Expression>();

		Object modelParfactors = process
				.getGlobalObject(GLOBAL_KEY_MODEL_PARFACTORS);

		if (modelParfactors == null) {
			// This to be backward compatible with code that creates
			// the model and assigns it to a map of global objects
			// before it creates the process.
			// These calls will ensure everything gets setup
			// as expected from this point onwards.
			Expression modelExpression = getModelDefinition(process);
			setRewritingProcessesModel(modelExpression, process);
			modelParfactors = process
					.getGlobalObject(GLOBAL_KEY_MODEL_PARFACTORS);
		}

		parfactors.addAll((List<Expression>) modelParfactors);

		return parfactors;
	}

	/**
	 * Retrieve from the cardinality associated with a sort from within the
	 * process. Processes store this information in its global object map with:<br>
	 * key = | <sortName> | and value = <an Integer valued Symbol><br>
	 * 
	 * @param process
	 *            the rewriting process from which the cardinality of the sort
	 *            is to be determined.
	 * @param sortName
	 *            the name of the sort whose cardinality is being sought from
	 *            the process.
	 * @return the cardinality of the sort as specified in the process or null
	 *         if none specified.
	 */
	public static Integer getCardinalityOfSort(RewritingProcess process,
			Expression sortName) {
		Integer result = null;

		Expression sortCardinality = Expressions.make(
				FunctorConstants.CARDINALITY, sortName);
		Object cardinality = process.getGlobalObject(sortCardinality);
		if (cardinality instanceof Symbol) {
			Symbol sCard = (Symbol) cardinality;
			if (sCard.getValue() instanceof Number) {
				result = sCard.intValue();
			}
		}

		return result;
	}

	/**
	 * Simple convenience routine for indicating all the parts needed when
	 * constructing a model.
	 * 
	 * @param name
	 * @param description
	 * @param sortDeclarations
	 * @param randomVariableDeclarations
	 * @param parfactorsDeclaration
	 * @param knownRandomVariableNames
	 * @return a Model constructed from the provided parts.
	 */
	public static Model constructFromParts(Expression name,
			Expression description, List<SortDeclaration> sortDeclarations,
			List<RandomVariableDeclaration> randomVariableDeclarations,
			ParfactorsDeclaration parfactorsDeclaration,
			Set<String> knownRandomVariableNames) {
		List<Expression> modelParts = new ArrayList<Expression>();

		modelParts.add(name);
		modelParts.add(description);

		for (SortDeclaration sort : sortDeclarations) {
			modelParts.add(sort.getSortDeclaration());
		}
		for (RandomVariableDeclaration randomVariable : randomVariableDeclarations) {
			modelParts.add(randomVariable.getRandomVariableDeclaration());
		}

		modelParts.add(parfactorsDeclaration.getDefinition());

		Expression modelDefinition = Expressions.make(
				Model.FUNCTOR_MODEL_DECLARATION, modelParts);

		Model model = new Model(modelDefinition, knownRandomVariableNames);

		return model;
	}

	//
	// PRIVATE METHODS
	//
	/**
	 * Assert that there is a definition (i.e. a top level model expression)
	 * associated with this Model's declaration (i.e. its string
	 * representation).
	 */
	private void assertDefined() {
		if (!isDefined()) {
			throw new IllegalStateException(
					"This model has no definition associated with it (i.e. is purely a declaration).");
		}
	}

	private void collectAndValidateModelParts() {
		List<ModelError> errors = new ArrayList<ModelError>();
		// if model is just defined as a collection of parfactors
		if (ParfactorsDeclaration.isParfactorsDeclaration(modelDefinition)) {
			parfactorsDeclaration = new ParfactorsDeclaration(modelDefinition);
		}
		// if is a full model declaration
		else if (Expressions.hasFunctor(modelDefinition,
				FUNCTOR_MODEL_DECLARATION)) {
			for (Expression modelPart : modelDefinition.getArguments()) {
				// Name or Description field
				if (isStringValuedSymbol(modelPart)) {
					if (name == null) {
						// first string valued symbol is assigned to
						// the name attribute
						name = modelPart;
					} 
					else if (description == null) {
						// second string valued symbol is assigned to
						// the description attribute
						description = modelPart;
					} 
					else {
						errors.add(new ModelError(
								ModelError.TYPE.TOO_MANY_STRING_PARAMETERS,
								modelPart));
					}
				}
				// Sort declarations
				else if (SortDeclaration.isSortDeclaration(modelPart)) {
					SortDeclaration sortDeclaration = SortDeclaration
							.makeSortDeclaration(modelPart);
					sortDeclarations.add(sortDeclaration);
				}
				// Random variable declarations
				else if (RandomVariableDeclaration
						.isRandomVariableDeclaration(modelPart)) {
					RandomVariableDeclaration randomVariableDeclaration = RandomVariableDeclaration
							.makeRandomVariableDeclaration(modelPart);
					randomVariableDeclarations.add(randomVariableDeclaration);
					// As this is contains the name of a random variable ensure
					// the set of known random variable names are updated
					knownRandomVariableNames.add(randomVariableDeclaration
							.getName().toString());
				}
				// Parfactors declaration
				else if (ParfactorsDeclaration
						.isParfactorsDeclaration(modelPart)) {
					if (parfactorsDeclaration == null) {
						parfactorsDeclaration = new ParfactorsDeclaration(
								modelPart);
					} 
					else {
						errors.add(new ModelError(
								ModelError.TYPE.MORE_THAN_1_PARFACTORS_DECLARATION,
								modelPart));
					}
				}
				// An unrecognized model part
				else {
					errors.add(new ModelError(
							ModelError.TYPE.UNRECOGNIZED_MODEL_PART, modelPart));
				}
			}

		} 
		else {
			errors.add(new ModelError(
					ModelError.TYPE.UNRECOGNIZED_MODEL_DEFINITION,
					modelDefinition));
		}

		// Some defaults if not specified in model
		if (name == null) {
			name = DefaultSymbol.createSymbol("No name given.");
		}
		if (description == null) {
			description = DefaultSymbol.createSymbol("No description given.");
		}
		if (null == parfactorsDeclaration) {
			// Default to an empty union.
			parfactorsDeclaration = new ParfactorsDeclaration(
					Expressions
							.make(ParfactorsDeclaration.FUNCTOR_PARFACTORS_DECLARATION));
		}
		
		deriveSortDeclarations(sortDeclarations, parfactorsDeclaration);

		//
		// Validate there are not overlaps in the use of symbols in the model.
		Set<Expression> uniqueNames = new HashSet<Expression>();
		Set<Expression> sortNames = new HashSet<Expression>();
		Set<Expression> constantsAlreadyAssignedToSorts = new HashSet<Expression>();

		for (SortDeclaration sortDeclaration : sortDeclarations) {
			// SORT_NAME_NOT_UNIQUE
			Expression name = sortDeclaration.getName();
			if (uniqueNames.contains(name)) {
				errors.add(new ModelError(ModelError.TYPE.SORT_NAME_NOT_UNIQUE,
						sortDeclaration.getSortDeclaration()));
			} 
			else {
				uniqueNames.add(name);
				sortNames.add(name);
			}

			for (Expression constant : sortDeclaration.getAssignedConstants()) {
				// CONSTANT_IN_MORE_THAN_1_SORT
				if (constantsAlreadyAssignedToSorts.contains(constant)) {
					errors.add(new ModelError(
							ModelError.TYPE.CONSTANT_IN_MORE_THAN_1_SORT,
							sortDeclaration.getSortDeclaration()));
				} 
				else {
					constantsAlreadyAssignedToSorts.add(constant);
					// CONSTANT_NAME_NOT_UNIQUE
					if (uniqueNames.contains(constant)) {
						errors.add(new ModelError(
								ModelError.TYPE.CONSTANT_NAME_NOT_UNIQUE,
								sortDeclaration.getSortDeclaration()));
					} 
					else {
						uniqueNames.add(constant);
					}
				}
			}
		}
		for (RandomVariableDeclaration randomVariableDeclaration : randomVariableDeclarations) {
			// RANDOM_VARIABLE_NAME_NOT_UNIQUE - this implies we don't allow
			// random variables with different arity to have the same name.
			Expression name = randomVariableDeclaration.getName();
			if (uniqueNames.contains(name)) {
				errors.add(new ModelError(
						ModelError.TYPE.RANDOM_VARIABLE_NAME_NOT_UNIQUE,
						randomVariableDeclaration
								.getRandomVariableDeclaration()));
			} 
			else {
				uniqueNames.add(name);
			}

			// RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED
			List<Expression> argSorts = new ArrayList<Expression>();
			argSorts.addAll(randomVariableDeclaration.getParameterSorts());
			argSorts.add(randomVariableDeclaration.getRangeSort());
			for (Expression sort : argSorts) {
				if (!SortDeclaration.isNameOfInBuilt(sort)
					&& !sortNames.contains(sort)
					) {
					
					errors.add(new ModelError(
							ModelError.TYPE.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED,
							sort));
				}
			}
		}

		if (errors.size() > 0) {
			throw new ModelException("Not a valid model definition:"
					+ modelDefinition, errors);
		}
	}

	private boolean isStringValuedSymbol(Expression expression) {
		
		boolean result = false;
		if (expression instanceof Symbol
			&& ((Symbol) expression).getValue() instanceof String
			) {
				
			result = true;
		}
		return result;
	}
	
	private void deriveSortDeclarations(List<SortDeclaration> sortDeclarations, ParfactorsDeclaration parfactorsDeclaration) {
		Set<Expression> sortNamesKnown = new HashSet<Expression>();
		for (SortDeclaration sd : sortDeclarations) {
			sortNamesKnown.add(sd.getName());
		}
		for (Expression parfactor : parfactorsDeclaration.getParfactors()) {
			if (IntensionalSet.isIntensionalSet(parfactor)) {
				for (Expression domain : IntensionalSet.getIndexDomains(parfactor)) {
					if (!domain.hasFunctor(CardinalityTypeOfLogicalVariable.FUNCTOR_TYPE) &&
					    !sortNamesKnown.contains(domain)) {
						sortDeclarations.add(new SortDeclaration(domain));
						sortNamesKnown.add(domain);
					}
				}
			}
		}
		
		if (sortDeclarations.size() == 0) {
			// Assign the default Universe of Discourse if nothing defined.
			sortDeclarations.add(new SortDeclaration(SortDeclaration.UNIVERSE_OF_DISCOURSE));
		}
	}
}
