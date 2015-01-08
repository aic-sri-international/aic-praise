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

import static com.sri.ai.util.Util.mapIntoList;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.core.DefaultBracketedExpression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityOfType;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.PRAiSEConfiguration;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.rules.RuleConverter;
import com.sri.ai.util.Util;

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
 * Note: The term 'Model Declaration' refers to the string representation of a model as defined above.
 * A 'Model Definition' is one where the declaration has been parsed into an expression and the model has
 * been instantiated with this definition (required by most of the API methods). The reason for using
 * a 'Model Declaration' is that it lets you declare a model without needing to worry about how it
 * should be instantiated (i.e. no need to have a rewriting process or a parser when declaring a model). 
 * When the model is to be used and a definition is required it will automatically instantiate it using
 * a default parser for the model's language, In addition it will likely be necessary to register the model
 * with a rewriting process as follows:
 * 
 * <pre>
 * // Note: A RewritingProcess is available at this point:
 * model.setRewritingProcessesModel(process);
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
		
		@Override
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
	public static final String GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAME_AND_ARITIES = "known random variable name and arities";
	public static final String GLOBAL_KEY_MODEL_INSTANCE   = "model instance";
	public static final String GLOBAL_KEY_MODEL_DEFINITION = "model definition";
	public static final String GLOBAL_KEY_MODEL_PARFACTORS = "model parfactors";
	public static final String GLOBAL_KEY_MODEL_SORT_NAMES = "model sort names";
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
	private Set<String> knownRandomVariableNameAndArities = new LinkedHashSet<String>();
	Collection<FunctionSignature> knownRandomPredicatesSignatures;
	// Definition information.
	private Expression modelDefinition = null;
	private Expression name = null;
	private Expression description = null;
	private List<SortDeclaration> sortDeclarations = new ArrayList<SortDeclaration>();
	private List<RandomVariableDeclaration> randomVariableDeclarations = new ArrayList<RandomVariableDeclaration>();
	private ParfactorsDeclaration parfactorsDeclaration = null;

	/** A convenience method doing the same as {@link RuleConverter#makeModel(String, String, String)}. */
	public static Model fromRules(String modelName, String modelDescription, String ruleAndDeclarationsListString) {
		return RuleConverter.makeModel(modelName, modelDescription, ruleAndDeclarationsListString);
	}
	
	/** A convenience method doing the same as {@link RuleConverter#makeModel(String, String, String)} with no name or description. */
	public static Model fromRules(String ruleAndDeclarationsListString) {
		return RuleConverter.makeModel("", "", ruleAndDeclarationsListString);
	}
	
	/**
	 * Declare a model without defining it (useful for caching purposes).
	 * 
	 * @param modelDeclaration
	 *            a string representation of the model's declaration.
	 * @param knownRandomVariableNameAndArities
	 *            a list of the known random variables in the model.
	 */
	public Model(String modelDeclaration, String... knownRandomVariableNameAndArities) {
		this.modelDeclaration = modelDeclaration;
		for (String nameAndArity : knownRandomVariableNameAndArities) {
			this.knownRandomVariableNameAndArities.add(nameAndArity);
		}
		this.knownRandomVariableNameAndArities = Collections.unmodifiableSet(this.knownRandomVariableNameAndArities);
		ensureDefined();
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
	public Model(Expression modelDefinition, Set<String> knownRandomVariableNameAndArities) {
		if (knownRandomVariableNameAndArities != null) {
			this.knownRandomVariableNameAndArities.addAll(knownRandomVariableNameAndArities);
			this.knownRandomPredicatesSignatures = mapIntoList(this.knownRandomVariableNameAndArities, FunctionSignature::new);
		}
		this.modelDefinition = modelDefinition;

		Collection<FunctionSignature> oldDefaultPredicatesSignatures = DefaultBracketedExpression.defaultPredicateSignatures;
		DefaultBracketedExpression.defaultPredicateSignatures = this.knownRandomPredicatesSignatures;
		
		collectAndValidateModelParts();
		
		DefaultBracketedExpression.defaultPredicateSignatures = oldDefaultPredicatesSignatures;
	}
	
	/**
	 * A copy constructor that lets you override the size information associated
	 * with the model's sort declarations.
	 * 
	 * @param toCopy
	 * @param knownTypeSize
	 * @param size
	 */
	public Model(Model toCopy, boolean knownTypeSize, Integer size) {
		List<Object> args = new ArrayList<Object>();
		this.knownRandomVariableNameAndArities.addAll(toCopy.knownRandomVariableNameAndArities);
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
			SortDeclaration copySD = new SortDeclaration(toCopySortDeclaration, knownTypeSize, size);
			this.sortDeclarations.add(copySD);
			args.add(copySD);
		}
		this.modelDeclaration = null;
		this.modelDefinition = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_MODEL_DECLARATION, args);
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
			if (this.name != null && !this.name.toString().equals("")) {
				md.append(this.name.toString()+",\n");
			}
			if (this.description != null && !this.description.toString().equals("")) {
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
	 * @return the known random variable name and arity strings associated with the model.
	 */
	public Set<String> getKnownRandomVariableNameAndArities() {
		return knownRandomVariableNameAndArities;
	}

	/**
	 * 
	 * @return an Expression representing the definition of the model.
	 */
	public Expression getModelDefinition() {
		return modelDefinition;
	}

	/**
	 * 
	 * @return the model definition's name.
	 */
	public Expression getName() {
		return name;
	}

	/**
	 * 
	 * @return a description of the model definition.
	 */
	public Expression getDescription() {
		return description;
	}

	/**
	 * 
	 * @return the sort declarations associated with the model.
	 */
	public List<SortDeclaration> getSortDeclarations() {
		return Collections.unmodifiableList(sortDeclarations);
	}

	/**
	 * @return the random variable declarations associated with the model.
	 */
	public List<RandomVariableDeclaration> getRandomVariableDeclarations() {
		return randomVariableDeclarations;
	}

	/**
	 * @return the random variable declaration associated with a name and arity, or <code>null</code> if there is none.
	 */
	public RandomVariableDeclaration getRandomVariableDeclaration(String name, int arity) {
		RandomVariableDeclaration result = LPIUtil.getRandomVariableDeclaration(name, arity, randomVariableDeclarations);
		return result;
	}

	/**
	 * @return the random variable declaration associated with a random variable value expression,
	 * or <code>null</code> if it is not such an expression, or there is none.
	 */
	public RandomVariableDeclaration getRandomVariableDeclaration(Expression randomVariableValueExpression) {
		RandomVariableDeclaration result = LPIUtil.getRandomVariableDeclaration(randomVariableValueExpression, randomVariableDeclarations);
		return result;
	}

	/**
	 * 
	 * @return the parfactors declaration associated with the model.
	 */
	public ParfactorsDeclaration getParfactorsDeclaration() {
		return parfactorsDeclaration;
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
	public static List<Expression> range(Expression aRandomVariableValueExpression, RewritingProcess process) {
		return _defaultRVRange;
	}

	/**
	 * Get the known Random Variable names associated with the rewriting
	 * process's model (in the form of name/arity).
	 * 
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return the set of known random variable names associated with the
	 *         rewriting process's model.
	 */
	public static Set<String> getKnownRandomVariableNameAndArities(RewritingProcess process) {
		@SuppressWarnings("unchecked")
		Set<String> knownRandomVariableNameAndArities = (Set<String>) process
				.getGlobalObject(GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAME_AND_ARITIES);
		if (null == knownRandomVariableNameAndArities) {
			knownRandomVariableNameAndArities = Collections.emptySet();
		}
		return knownRandomVariableNameAndArities;
	}

	/**
	 * Get the model associated with a rewriting process.
	 */
	public static Model getRewritingProcessesModel(RewritingProcess process) {
		// It seems unnecessary to construct a new Model object. The rewriting process should store the entire Model object instead of its parts.
		Expression processModelDefinition = getModelDefinition(process);
		Set<String> processKnownRandomVariableNameAndArities = getKnownRandomVariableNameAndArities(process);
		if (processModelDefinition == null || processKnownRandomVariableNameAndArities == null) {
			return null;
		}
		Model result = new Model(processModelDefinition, processKnownRandomVariableNameAndArities);
		return result;
	}

	/**
	 * Set the rewriting process's model and known random variable names for
	 * this Example Model.
	 * 
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return 
	 */
	public RewritingProcess setRewritingProcessesModel(RewritingProcess process) {
		process = setRewritingProcessesModel(modelDefinition, knownRandomVariableNameAndArities, process);
		return process;
	}

	/**
	 * Set the rewriting process's model.
	 * 
	 * @param modelDefinition
	 *            an expression representing the model the rewriting process is
	 *            working over.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return 
	 */
	public static RewritingProcess setRewritingProcessesModel(Expression modelDefinition, RewritingProcess process) {

		@SuppressWarnings("unchecked")
		Set<String> knownVarNames = (Set<String>) process
				.getGlobalObject(GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAME_AND_ARITIES);
		if (knownVarNames == null) {
			knownVarNames = new LinkedHashSet<String>();
		}
		process = setRewritingProcessesModel(modelDefinition, knownVarNames, process);
		return process;
	}

	/**
	 * Returns a new rewriting process based on given one,
	 * extended with model and known random variable names.
	 * 
	 * @param modelDefinitionExpression
	 *            an expression representing the model the rewriting process is
	 *            working over.
	 * @param knownRandomVariableNameAndArities
	 *            the set of known random variable names in a model.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 */
	public static RewritingProcess setRewritingProcessesModel(
			Expression modelDefinitionExpression,
			Set<String> knownRandomVariableNameAndArities, RewritingProcess process) {

		Model model = new Model(modelDefinitionExpression, knownRandomVariableNameAndArities);
		
		CardinalityOfType.registerTypeSizeOfSymbolOrTypeWithProcess(
				new ModelLookupTypeSizeOfLogicalVariable(model), process);

		process.putGlobalObject(GLOBAL_KEY_MODEL_INSTANCE,   model);
		process.putGlobalObject(GLOBAL_KEY_MODEL_DEFINITION, model.getModelDefinition());
		process.putGlobalObject(GLOBAL_KEY_MODEL_PARFACTORS, model.getParfactorsDeclaration().getParfactors());

		// Handle case where names may have been setup globally
		// outside of the model and process.
		@SuppressWarnings("unchecked")
		Set<String> allKnownRandomVariableNameAndArities = (Set<String>) process.getGlobalObject(GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAME_AND_ARITIES);
		if (allKnownRandomVariableNameAndArities == null) {
			allKnownRandomVariableNameAndArities = model.getKnownRandomVariableNameAndArities();
		} 
		else {
			allKnownRandomVariableNameAndArities.addAll(model.getKnownRandomVariableNameAndArities());
		}
		
		// Ensure we have all the known random variable names
		for (RandomVariableDeclaration randomVariableDeclaration : model.getRandomVariableDeclarations()) {
			allKnownRandomVariableNameAndArities.add(nameAndArityFromRandomVariableDeclaration(randomVariableDeclaration));
		}
		// Now ensure we setup the random predicate catalog with all the known names
		Set<FunctionSignature> randomPredicates = new LinkedHashSet<FunctionSignature>();
		for (String randomVariableName : allKnownRandomVariableNameAndArities) {			
			randomPredicates.add(new FunctionSignature(randomVariableName));
		}
		process.putGlobalObject(GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAME_AND_ARITIES, allKnownRandomVariableNameAndArities);
		process.putGlobalObject(GLOBAL_KEY_MODEL_RANDOM_PREDICATE_CATALOG, new RandomPredicateCatalog(randomPredicates));
		
		process = extendContextualSymbolsOnProcessWithSortAndRandomVariableTypeInformation(model.getSortDeclarations(), model.getRandomVariableDeclarations(), process);
		
		// Setup the sort names for easy access
		List<Expression> sortNames = new ArrayList<Expression>();
		for (SortDeclaration sortDeclaration : model.getSortDeclarations()) {
			sortNames.add(sortDeclaration.getName());
		}
		process.putGlobalObject(GLOBAL_KEY_MODEL_SORT_NAMES, sortNames);
		
		for (Expression parfactor : model.getParfactorsDeclaration().getParfactors()) {
			process = LPIUtil.extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(parfactor, process);
		}
		
		return process;
	}
	
	/**
	 * Set up the known random variables and random predicate catalog to be
	 * associated with the passed in rewriting process.
	 * 
	 * @param sortDeclarationExpressions
	 *            the sort declarations.
	 * @param randomVariableDeclarationExpressions
	 *            the random variable declarations from which to instantiate the
	 *            known random variable names and catalog to be associated with
	 *            the passed in rewriting process.
	 * @param process
	 *            the rewriting process to wire up known random variable names
	 *            and a random predicate catalog to.
	 * @return a new rewriting process based on the passed in process with random variable information associated with its context.
	 */
	public static RewritingProcess setKnownSortsAndRandomVariables(Set<Expression> sortDeclarationExpressions, Set<Expression> randomVariableDeclarationExpressions, RewritingProcess process) {
		Set<SortDeclaration> sortDeclarations = new LinkedHashSet<>();
		sortDeclarationExpressions.forEach(sortDeclarationExpression -> sortDeclarations.add(SortDeclaration.makeSortDeclaration(sortDeclarationExpression)));
		
		Set<RandomVariableDeclaration> randomVariableDeclarations = new LinkedHashSet<>();
		Set<String> knownVarNames = new LinkedHashSet<String>();
		for (Expression rvd : randomVariableDeclarationExpressions) {
			RandomVariableDeclaration declaration = RandomVariableDeclaration.makeRandomVariableDeclaration(rvd);
			randomVariableDeclarations.add(declaration);
			knownVarNames.add(nameAndArityFromRandomVariableDeclaration(declaration));
		}
		// Now ensure we setup the random predicate catalog with all the known names
		Set<FunctionSignature> randomPredicates = new LinkedHashSet<FunctionSignature>();
		for (String randomVariableName : knownVarNames) {			
			randomPredicates.add(new FunctionSignature(randomVariableName));
		}
		
		process.putGlobalObject(GLOBAL_KEY_KNOWN_RANDOM_VARIABLE_NAME_AND_ARITIES, knownVarNames);
		process.putGlobalObject(GLOBAL_KEY_MODEL_RANDOM_PREDICATE_CATALOG, new RandomPredicateCatalog(randomPredicates));
		
		RewritingProcess result = extendContextualSymbolsOnProcessWithSortAndRandomVariableTypeInformation(sortDeclarations, randomVariableDeclarations, process);
		
		return result;
	}
	
	/**
	 * Get the Model instance associated with the process.
	 * 
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return the model instance associated with the rewriting process.
	 */
	public static Model getModelInstance(RewritingProcess process) {
		Model model = (Model) process.getGlobalObject(GLOBAL_KEY_MODEL_INSTANCE);
		return model;
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
		Expression model = (Expression) process.getGlobalObject(GLOBAL_KEY_MODEL_DEFINITION);
		return model;
	}
	
	public static boolean isAllTypeSizesKnown(RewritingProcess process) {
		return PRAiSEConfiguration.isAllTypeSizesKnownInModel();
	}
	
	public static RandomPredicateCatalog getRandomPredicateCatalog(RewritingProcess process) {
		RandomPredicateCatalog catalog = (RandomPredicateCatalog) process.getGlobalObject(GLOBAL_KEY_MODEL_RANDOM_PREDICATE_CATALOG);
		
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
				throw new Error("Random predicate catalog requested but rewriting process does not contain a global object named '" + GLOBAL_KEY_MODEL_DEFINITION + "'");
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

		Object modelParfactors = process.getGlobalObject(GLOBAL_KEY_MODEL_PARFACTORS);

		if (modelParfactors == null) {
			// This to be backward compatible with code that creates
			// the model and assigns it to a map of global objects
			// before it creates the process.
			// These calls will ensure everything gets setup
			// as expected from this point onwards.
			Expression modelExpression = getModelDefinition(process);
			process = setRewritingProcessesModel(modelExpression, process);
			modelParfactors = process
					.getGlobalObject(GLOBAL_KEY_MODEL_PARFACTORS);
		}

		parfactors.addAll((List<Expression>) modelParfactors);

		return parfactors;
	}
	
	/**
	 * Get the sort names from from the model.
	 * 
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return the sort names in the model.
	 */
	@SuppressWarnings("unchecked")
	public static List<Expression> getSortNames(RewritingProcess process) {
		List<Expression> sortNames = new ArrayList<Expression>();
		
		Object modelSortNames = process.getGlobalObject(GLOBAL_KEY_MODEL_SORT_NAMES);
		if (modelSortNames == null) {
			// This to be backward compatible with code that creates
			// the model and assigns it to a map of global objects
			// before it creates the process.
			// These calls will ensure everything gets setup
			// as expected from this point onwards.
			Expression modelExpression = getModelDefinition(process);
			process = setRewritingProcessesModel(modelExpression, process);
			modelSortNames = process.getGlobalObject(GLOBAL_KEY_MODEL_SORT_NAMES);
		}

		sortNames.addAll((List<Expression>) modelSortNames);
		
		return sortNames;
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

		Expression sortCardinality = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				FunctorConstants.CARDINALITY, sortName);
		Expression cardinality = (Expression) process.getGlobalObject(sortCardinality);
		if (cardinality != null) {
			if (cardinality.getValue() instanceof Number) {
				result = cardinality.intValue();
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
	 * @param knownRandomVariableNameAndArities
	 * @return a Model constructed from the provided parts.
	 */
	public static Model constructFromParts(Expression name,
			Expression description, List<SortDeclaration> sortDeclarations,
			List<RandomVariableDeclaration> randomVariableDeclarations,
			ParfactorsDeclaration parfactorsDeclaration,
			Set<String> knownRandomVariableNameAndArities) {
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

		Expression modelDefinition = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				Model.FUNCTOR_MODEL_DECLARATION, modelParts);

		Model model = new Model(modelDefinition, knownRandomVariableNameAndArities);

		return model;
	}
	
	/**
	 * Load a model declaration from a specified resource.
	 * 
	 * @param resourceName
	 * @return the model declaration contained in the resource.
	 */
	public static String getModelDeclarationFromResource(String resourceName) {
		StringBuilder sb = new StringBuilder();
		
		try {
			InputStream is = Model.class.getResourceAsStream(resourceName);
			LineNumberReader reader = new LineNumberReader(new InputStreamReader(is));
			String line = null;
			while ( (line = reader.readLine()) != null) {
				sb.append(line);
				sb.append("\n");
			}
			reader.close();
			
		} catch (IOException ioe) {
			// ignore
		}
		
		return sb.toString();
	}
	
	/** Convenience method for creating an LPI rewriting process based on this Model with given root expression and LBPConfiguration. */
	public RewritingProcess makeRewritingProcess(Expression rootExpression, LBPConfiguration lbpConfiguration) {
		RewritingProcess process = LBPFactory.newLBPProcess(rootExpression, lbpConfiguration);
		process = setRewritingProcessesModel(process);
		return process;
	}
	
	@Override
	public String toString() {
		return this.getModelDefinition().toString();
	}

	//
	// PRIVATE METHODS
	//
	/**
	 * Assert that there is a definition (i.e. a top level model expression)
	 * associated with this Model's declaration (i.e. its string
	 * representation).
	 */
	private void ensureDefined() {
		if (modelDefinition == null) {
			getRandomPredicatesSignatures();
			Parser parser = new AntlrGrinderParserWrapper(getRandomPredicatesSignatures());
			modelDefinition = parser.parse(modelDeclaration);
		}
	}
	
	public Collection<FunctionSignature> getRandomPredicatesSignatures() {
		if (knownRandomPredicatesSignatures == null) {
			knownRandomPredicatesSignatures = Util.mapIntoList(knownRandomVariableNameAndArities, FunctionSignature::new);
		}
		return knownRandomPredicatesSignatures;
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
				else if (RandomVariableDeclaration.isRandomVariableDeclaration(modelPart)) {
					RandomVariableDeclaration randomVariableDeclaration = RandomVariableDeclaration
							.makeRandomVariableDeclaration(modelPart);
					randomVariableDeclarations.add(randomVariableDeclaration);
					// As this is contains the name of a random variable ensure
					// the set of known random variable names are updated
					knownRandomVariableNameAndArities.add(nameAndArityFromRandomVariableDeclaration(randomVariableDeclaration));
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
			name = Expressions.makeSymbol("No name given.");
		}
		if (description == null) {
			description = Expressions.makeSymbol("No description given.");
		}
		if (null == parfactorsDeclaration) {
			// Default to an empty union.
			parfactorsDeclaration = new ParfactorsDeclaration(
					Expressions
							.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ParfactorsDeclaration.FUNCTOR_PARFACTORS_DECLARATION));
		}
		
		deriveSortDeclarations(sortDeclarations, parfactorsDeclaration);

		//
		// Validate there are not overlaps in the use of symbols in the model.
		Set<Expression> uniqueNames = new LinkedHashSet<Expression>();
		Set<Expression> sortNames = new LinkedHashSet<Expression>();
		Set<Expression> constantsAlreadyAssignedToSorts = new LinkedHashSet<Expression>();

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
		if (expression.getSyntacticFormType().equals("Symbol")
			&& expression.getValue() instanceof String) {
			result = true;
		}
		return result;
	}
	
	private void deriveSortDeclarations(List<SortDeclaration> sortDeclarations, ParfactorsDeclaration parfactorsDeclaration) {
		Set<Expression> sortNamesKnown = new LinkedHashSet<Expression>();
		// Ensure In-Built sort names are excluded when attempting to derive.
		for (SortDeclaration sd : SortDeclaration.IN_BUILT_SORTS) {
			sortNamesKnown.add(sd.getName());
		}
		for (SortDeclaration sd : sortDeclarations) {
			sortNamesKnown.add(sd.getName());
		}
		for (Expression parfactor : parfactorsDeclaration.getParfactors()) {
			if (Sets.isIntensionalSet(parfactor)) {
				for (Expression type : IndexExpressions.getIndexDomains(parfactor)) {
					if (!CardinalityOfType.isTypeSyntacticFunctionApplication(type) &&
					    !sortNamesKnown.contains(type)) {
						sortDeclarations.add(new SortDeclaration(type));
						sortNamesKnown.add(type);
					}
				}
			}
		}
		
		if (sortDeclarations.size() == 0) {
			// Assign the default Universe of Discourse if nothing defined.
			sortDeclarations.add(new SortDeclaration(SortDeclaration.UNIVERSE_OF_DISCOURSE));
		}
	}
	
	private static String nameAndArityFromRandomVariableDeclaration(RandomVariableDeclaration rvd) {
		String result = rvd.getName().getValue().toString() + "/" + rvd.getArityValue();
		return result;
	}
	
	private static RewritingProcess extendContextualSymbolsOnProcessWithSortAndRandomVariableTypeInformation(Collection<SortDeclaration> sortDeclarations, Collection<RandomVariableDeclaration> randomVariableDeclarations, RewritingProcess process) {
		final Map<Expression, Expression> typeMap = new LinkedHashMap<>();
		sortDeclarations.forEach(sortDeclaration -> {
			sortDeclaration.getAssignedConstants().forEach(sortConstant -> typeMap.put(sortConstant, sortDeclaration.getName()));
		});
		randomVariableDeclarations.forEach(randomVariableDeclaration -> {
			List<Expression> args = new ArrayList<>();
			if (randomVariableDeclaration.getParameterSorts().size() > 1) {
				args.add(Expressions.apply(FunctorConstants.CARTESIAN_PRODUCT, randomVariableDeclaration.getParameterSorts()));
			}
			else {
				args.addAll(randomVariableDeclaration.getParameterSorts());
			}
			args.add(randomVariableDeclaration.getRangeSort());
			typeMap.put(randomVariableDeclaration.getName(), Expressions.apply(FunctorConstants.FUNCTION_TYPE, args));
		});
		
		RewritingProcess result = GrinderUtil.extendContextualSymbols(typeMap, process);
		return result;
	}
}
