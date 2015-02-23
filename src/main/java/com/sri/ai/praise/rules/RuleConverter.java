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
package com.sri.ai.praise.rules;

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderTerminalSymbols;
import com.sri.ai.praise.BracketedExpressionSubExpressionsProvider;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.ParfactorsDeclaration;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.rules.antlr.RuleParserWrapper;
import com.sri.ai.praise.rules.antlr.RuleTerminalSymbols;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.math.Rational;

/**
 * This class contains methods for dealing with the rules syntax. It contains 
 * methods for converting from the rules syntax to the lower-level syntax, and
 * vice versa.  It also contains methods for generating a rules string from a
 * rules expression.<br>
 * <br>
 * More information about the rules syntax and converting from the rules syntax
 * to the low level syntax can be found here:<br>
 * http://code.google.com/p/aic-praise/wiki/SyntaxAndMeaningOfProbabilisticRules
 * <br>
 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
 * <br>
 * http://code.google.com/p/aic-praise/wiki/TranslatingInferenceInputAndOutput 
 * <br>
 * 
 * @author etsai
 *
 */
@Beta
public class RuleConverter {

	public static final String FUNCTOR_ATOMIC_RULE        = "atomic rule";
	public static final String FUNCTOR_CONDITIONAL_RULE   = "conditional rule";
	public static final String FUNCTOR_PROLOG_RULE        = "prolog rule";
	public static final String FUNCTOR_STANDARD_PROB_RULE = "standard probability rule";
	public static final String FUNCTOR_CAUSAL_RULE        = "causal rule";
	public static final String FUNCTOR_MAY_BE_SAME_AS     = ". may be same as .";
	public static final String FUNCTOR_QUERY              = "query";

	public static final String TYPE_BOOLEAN               = "Boolean";

	public static final String SYNTACTIC_FORM_TYPE_SYMBOL = "Symbol";
	public static final String SYNTACTIC_FORM_TYPE_FUNCTION_APPLICATION = "Function application";

	private RuleParserWrapper  ruleParser        = null;

	public class LowLevelModelParts {
		private Set<Expression> sortDeclarations           = new LinkedHashSet<Expression>();
		private Set<Expression> randomVariableDeclarations = new LinkedHashSet<Expression>();
		private Set<Expression> parfactors                 = new LinkedHashSet<Expression>();
		
		public LowLevelModelParts(Collection<Expression> sortDeclarations, Collection<Expression> randomVariableDeclarations, Collection<Expression> parfactors) {
			this.sortDeclarations.addAll(sortDeclarations);
			this.randomVariableDeclarations.addAll(randomVariableDeclarations);
			this.parfactors.addAll(parfactors);
		}
		
		public Set<Expression> getSortDeclarations() {
			return sortDeclarations;
		}		
		
		public Set<Expression> getRandomVariableDeclarations() {
			return randomVariableDeclarations;
		}
		
		public Set<Expression> getParfactors() {
			return parfactors;
		}
	}
	
	/*===================================================================================
	 * CONSTRUCTORS
	 *=================================================================================*/
	public RuleConverter() {
		// Ensure these are instantiated straight away and not when first referenced.
		// This helps ensure any global dependencies are setup correctly.
		ruleParser       = new RuleParserWrapper();
	}

	/*===================================================================================
	 * PUBLIC METHODS
	 *=================================================================================*/

	/** A convenience method building a {@link Model} from a string. */
	public static Model makeModel(String modelName, String modelDescription, String ruleAndDeclarationsListString) {
		Trace.in("Making model out of {}", ruleAndDeclarationsListString);
		RuleConverter ruleConverter = new RuleConverter();
		RewritingProcess process = LBPFactory.newLBPProcess(Expressions.TRUE);
		Model model;
		try {
			model = ruleConverter.makeModelDynamic(modelName, modelDescription, ruleAndDeclarationsListString, process);
		} catch (ReservedWordException e) {
			throw new Error(e);
		}
		Trace.out("Model is {}", model.getModelDefinition());
		return model;
	}
	
	/**
	 * Version of query String arguments that are automatically converted to HLL expressions.
	 * 
	 * @param queryFormulaString
	 *            High level language query string.
	 * @param ruleAndDeclarationsListString
	 *            High level language model string (i.e. a list of rules and declarations).
	 * @param llmName
	 *            Name to use for the low level model.
	 * @param llmDescription
	 *            Description to use for the low level model
	 * @return A triple consisting of (low-level-model, low-level-query, low-level-model-extended-by-query).
	 * @throws ReservedWordException
	 * @throws QueryContainsUnknownRandomVariablesException
	 */
	public Triple<Model, Expression, Model> query(String queryFormulaString, String ruleAndDeclarationsListString, String llmName, String llmDescription, RewritingProcess process) throws ReservedWordException, QueryContainsUnknownRandomVariablesException {
		return query(queryFormulaString != null ? ruleParser.parseFormula(queryFormulaString) : null, ruleParser.parseAll(ruleAndDeclarationsListString), llmName, llmDescription, process);
	}
	
	/**
	 * Converts the rules model and query and generates a low-level model object
	 * and query.
	 * 
	 * Description of function: 
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeQuery
	 * 
	 * @param queryFormula
	 *            The query expression for the model.
	 * @param ruleAndDeclarationsList
	 *            The list of rule expressions of the model.
	 * @param llmName
	 *            The name for the low level model.
	 * @param llmDescription
	 *            The description to be used for the low level model.
	 * @return A triple consisting of (low-level-model, low-level-query, low-level-model-extended-by-query).
	 * @throws ReservedWordException
	 * @throws QueryContainsUnknownRandomVariablesException
	 */
	public Triple<Model, Expression, Model> query(Expression queryFormula, List<Expression> ruleAndDeclarationsList, String llmName, String llmDescription, RewritingProcess process) throws ReservedWordException, QueryContainsUnknownRandomVariablesException {

		Model lowLevelModel = makeModel(ruleAndDeclarationsList, llmName, llmDescription, process);
		
		// | (low-level-query, low-level-model-extended-by-query) <- query(queryFormula, low-level-model)
		Pair<Expression, Model> queryLowLevelModelExtendedByQuery = query(queryFormula, lowLevelModel, process);
		
		// | return (low-level-model, low-level-query, low-level-model-extended-by-query)
		return new Triple<Model, Expression, Model>(lowLevelModel, queryLowLevelModelExtendedByQuery.first, queryLowLevelModelExtendedByQuery.second);
	}

	/**
	 * Converts the rules model and generates a low-level model object.
	 * 
	 * @param ruleAndDeclarationsList
	 *            The list of rule expressions of the model.
	 * @param llmName
	 *            The name for the low level model.
	 * @param llmDescription
	 *            The description to be used for the low level model.
	 * @return A {@link Model} object.
	 * @throws ReservedWordException
	 */
	public Model makeModel(List<Expression> ruleAndDeclarationsList, String llmName, String llmDescription, RewritingProcess process) throws ReservedWordException {
		// | (sortDeclarations, randomVariableDeclarations, parfactors) <- translate(ruleAndDeclarationsList)
		LowLevelModelParts lowLevelSyntax = translate(ruleAndDeclarationsList, process);
		
		//
		// Some assertion testing before moving forward:
		// Ensure the names used for sorts are legal
		for (Expression sortDeclaration: lowLevelSyntax.getSortDeclarations()) {
			String token = sortDeclaration.get(0).getValue().toString();
			checkLegalToken(token);
		}
		// Ensure the names used for random variables are legal
		for (Expression randomVariableDeclaration : lowLevelSyntax.getRandomVariableDeclarations()) {
			String token = randomVariableDeclaration.get(0).getValue().toString();
			checkLegalToken(token);
		}
		
		// | low-level-model <- model(sortDeclarations, randomVariableDeclarations, parfactors)
		Model lowLevelModel = createModel(llmName, llmDescription, lowLevelSyntax.getSortDeclarations(), lowLevelSyntax.getRandomVariableDeclarations(), lowLevelSyntax.getParfactors());
		return lowLevelModel;
	}
	
	/**
	 * Converts the rules model and generates a low-level model object.
	 * 
	 * @param ruleAndDeclarationsList
	 *            The list of rule expressions of the model.
	 * @return A {@link Model} object.
	 * @throws ReservedWordException
	 */
	private Model makeModelDynamic(String modelName, String modelDescription, String ruleAndDeclarationsListString, RewritingProcess process) throws ReservedWordException {
		return makeModel(ruleParser.parseAll(ruleAndDeclarationsListString), modelName, modelDescription, process);
	}
	
	/**
	 * Converts given query formula to low level query and extends the given low-level-model
	 * with information needed to support the query if necessary.
	 * 
	 * Description of function: 
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeIncrementalQuery
	 * 
	 * @param queryFormula
	 *            The query expression for the model.
	 * @param lowLevelModel
	 *            A low level model against which the query is to be applied.
	 * @return A triple consisting of (low-level-model, low-level-query, low-level-model-extended-by-query).
	 * @throws QueryContainsUnknownRandomVariablesException
	 */
	public Pair<Expression, Model> query(Expression queryFormula, Model lowLevelModel, RewritingProcess process) throws QueryContainsUnknownRandomVariablesException {
		Expression      queryEquivalenceRule, queryAtom = null;
		Model           lowLevelModelExtendedByQuery    = null;
		Set<Expression> sorts                           = new LinkedHashSet<Expression>();
		Set<Expression> randomVariables                 = new LinkedHashSet<Expression>();
		Set<Expression> parfactors                      = new LinkedHashSet<Expression>();
		
		// | (sortDeclarations, randomVariableDeclarations, parfactors) <- low-level-model
		for (SortDeclaration sd : lowLevelModel.getSortDeclarations()) {
			sorts.add(sd.getSortDeclaration());
		}
		for (RandomVariableDeclaration rd : lowLevelModel.getRandomVariableDeclarations()) {
			randomVariables.add(rd.getRandomVariableDeclaration());
		}
		parfactors.addAll(lowLevelModel.getParfactorsDeclaration().getParfactors());
		
		// If we actually have a query to extend the model with
		if (queryFormula != null) {
			queryAtom = queryFormula;
			
			// | (queryEquivalencyRule, queryAtom) <- queryRuleAndAtom(queryFormula, low-level-model.randomVariableDeclarations)
			Pair<Expression, Expression> queryRuleAndAtom = queryRuleAndAtom(queryFormula, randomVariables, process);
			// | if (queryEquivalencyRule, queryAtom) is null
			if (queryRuleAndAtom == null) {
				// | .... queryAtom <- queryFormula
				queryAtom = queryFormula;
			}
			else {
				queryEquivalenceRule = queryRuleAndAtom.first;
				queryAtom            = queryRuleAndAtom.second;
				// | .... add elements from translate(queryEquivalencyRule + low-level-model.declarations)
				List<Expression> rulesAndDeclarationsList = new ArrayList<Expression>();
				rulesAndDeclarationsList.add(queryEquivalenceRule);
				rulesAndDeclarationsList.addAll(sorts);
				rulesAndDeclarationsList.addAll(randomVariables);
				LowLevelModelParts llmParts = translate(rulesAndDeclarationsList, process);
				// | ................. to (sortDeclarations, randomVariableDeclarations, parfactors)
				sorts.addAll(llmParts.getSortDeclarations());
				randomVariables.addAll(llmParts.getRandomVariableDeclarations());
				// | .... add new random variable declaration for queryAtom to randomVariableDeclarations
				randomVariables.add(createQueryDeclaration(queryAtom, queryEquivalenceRule, randomVariables, process));
				parfactors.addAll(llmParts.getParfactors());
			}
		}
		
		lowLevelModelExtendedByQuery = createModel(lowLevelModel.getName().toString(), 
				                                    lowLevelModel.getDescription().toString(), 
													sorts, randomVariables, parfactors);		
		
		return new Pair<Expression, Model>(queryAtom, lowLevelModelExtendedByQuery);
	}
	
	/**
	 * Translates the high level rules into their low level syntax equivalents.
	 * 
	 * Description of function: 
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslate
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeRuleList2PotentialExpressions
	 * 
	 * @param rulesAndDeclarationsList
	 *            the high level rules to be converted into low level syntax.
	 * @return an equivalent low level syntax representation of the input rules
	 *         given.
	 */
	public LowLevelModelParts translate(List<Expression> rulesAndDeclarationsList, RewritingProcess process) {
		// it would be good to have this method return a Model, or an explanation as to why this is not possible.
		
		LowLevelModelParts result = null;

		// Convert the conjunctions of rules.
		List<Expression> extendedRulesAndDeclarationsList = translateConjunctions(rulesAndDeclarationsList);
			
		// | (sortDeclarations, randomVariableDeclarations, rules) <- separate declarations and rules into separate lists
		Set<Expression>  sortDeclarations           = new LinkedHashSet<Expression>();
		Set<Expression>  randomVariableDeclarations = new LinkedHashSet<Expression>();
		List<Expression> rules                      = new ArrayList<Expression>();
		Set<String>      sortNames                  = new LinkedHashSet<String>();

		// Sort the declarations and rules
		for (Expression ruleOrDeclaration : extendedRulesAndDeclarationsList) {
			if (ruleOrDeclaration.getFunctor().equals(RandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION)) {
				randomVariableDeclarations.add(ruleOrDeclaration);
			}
			else if (ruleOrDeclaration.getFunctor().equals(SortDeclaration.FUNCTOR_SORT_DECLARATION)) {
				sortDeclarations.add(ruleOrDeclaration);
				sortNames.add(ruleOrDeclaration.get(0).toString());
			}
			else {
				// Is not a declaration, therefore treat as a rule
				rules.add(ruleOrDeclaration);
			}
		}
		
		// Look for missing sort declarations in the random variable declarations.
		Set<String> missingSorts = new LinkedHashSet<String>();
		for (Expression randomVariableDeclaration : randomVariableDeclarations) {
			List<Expression> args = randomVariableDeclaration.getArguments();
			for (int ii = 2; ii < args.size() - 1; ii++) {
				Expression argName = args.get(ii);
				if (!sortNames.contains(argName)) {
					// Ensure we don't identify an in-built sort as being missing.
					if (!SortDeclaration.isNameOfInBuilt(argName)) {
						missingSorts.add(argName.toString());
					}
				}
			}
		}

		// Add declarations for the missing sorts.
		for (String missingSort : missingSorts) {
			sortDeclarations.add(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(SortDeclaration.FUNCTOR_SORT_DECLARATION, missingSort, SortDeclaration.UNKNOWN_SIZE, 
					ExtensionalSet.makeEmptySetExpression()));
			sortNames.add(missingSort);
		}
				
		Pair<List<Expression>, Set<Expression>> rulesAndRandomVariableDeclarations;
		// | (rules, randomVariableDeclarations) <- translateFunctions(rules, randomVariableDeclarations)
		rulesAndRandomVariableDeclarations = translateFunctions(rules, randomVariableDeclarations, process);
		rules                      = rulesAndRandomVariableDeclarations.first;
		randomVariableDeclarations = rulesAndRandomVariableDeclarations.second;

		// | (rules, randomVariableDeclarations) <- translateQuantifiers(rules, randomVariableDeclarations)
		rulesAndRandomVariableDeclarations = translateQuantifiers(rules, randomVariableDeclarations, process);
		rules                      = rulesAndRandomVariableDeclarations.first;
		randomVariableDeclarations = rulesAndRandomVariableDeclarations.second;
		
		// | potentialExpressions <- ruleList2PotentialExpressions(rules)
		List<Expression> potentialExpressions = ruleList2PotentialExpressions(rules);
		
		// Note: This is required to ensure random variable information is available on
		// the rewriting process when performing R_normalize and R_complete_normalize operations.
		process = Model.setKnownSortsAndRandomVariables(sortDeclarations, randomVariableDeclarations, process);
		
		// | constrainedPotentialExpressions <- disembedConstraints(potentialExpressions)
		List<Pair<Expression, Expression>> constrainedPotentialExpressions = disembedConstraints(potentialExpressions, process);
		
		// | parfactors <- constraintedPotentialExpressions2Parfactors(constrainedPotentialExpressions)
		Set<Expression> parfactors = constraintedPotentialExpressionsToParfactors(constrainedPotentialExpressions, randomVariableDeclarations, process);
		
		// | return (sortDeclarations, randomVariableDeclarations, parfactors)
		result = new LowLevelModelParts(sortDeclarations, randomVariableDeclarations, parfactors);
		
		return result;
	}
	
	/**
	 * Converts uses of functions in the potential expressions into relationships and
	 * adds the supporting potential expressions necessary.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateFunctions
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeFunctorAndArguments
	 * 
	 * @param rules  A list of rules possible containing the use of functions.
	 * @param randomVariableDeclarations A set of known random variable declarations.
	 * @return  (rules, randomVariableDeclarations).
	 */
	public Pair<List<Expression>, Set<Expression>> translateFunctions(List<Expression> rules, 
			Set<Expression> randomVariableDeclarations, RewritingProcess process) {
		// | newRules <- empty list
		List<Expression> newRules = new ArrayList<Expression>();
		// | newRandomVariableDeclarations <- empty list
		Set<Expression>  newRandomVariableDeclarations = new LinkedHashSet<Expression>();
		// | functions <- empty set of pairs of expressions and arities
  		Map<String, Set<Integer>> functionsIdentified = new LinkedHashMap<String, Set<Integer>>();

		// | for each rule in rules
		for (Expression rule : rules) {			
			Expression toReplace = rule;
			Expression newRule   = rule;

			// |.... newRule <- exhaustively replace each expression E in rule
			// |........ with the following replacement function:
			ReplaceFunctionWithRelation replacementFunction = 
					new ReplaceFunctionWithRelation(randomVariableDeclarations, functionsIdentified);
			do {
			    toReplace = newRule;
			    newRule = toReplace.replaceAllOccurrences(replacementFunction, process);
			} while (newRule != toReplace);
			
			// | .... NewUniqueVariables <- empty set of logical variables
			// Note: the set is represented by the keys, while the values
			// identify the predicate1 the newUniqueVariable originated from 
			// (used later on when creating '. may be same as .'s)
			Map<Expression, Expression> newUniqueVariables = new LinkedHashMap<Expression, Expression>();
			// | .... newRule <- translateFunctionsAsArgument(newRule, randomVariableDeclarations, functions, NewUniqueVariables, LogicalVariables, empty cache)
			newRule = translateFunctionsAsArgument(newRule, randomVariableDeclarations, functionsIdentified, 
							newUniqueVariables, new LinkedHashMap<Expression, Expression>(), 
							new AtomicInteger(-1), process);
			// | .... LogicalVariables <- collect logical variables in newRule
			Set<Expression> logicalVariables = Expressions.getVariables(newRule, process);
			// | .... for each NewUniqueVariable in NewUniqueVariables
			for (Expression newUniqueVariable : newUniqueVariables.keySet()) {
				Expression predicate1Functor = newUniqueVariables.get(newUniqueVariable);			
				// | ......... mayBeSame <- conjunction of formulas "NewUniqueVariable may be same as LV",
				// | .......................... for each LV in LogicalVariables alphabetically greater than NewUniqueVariable
				List<Expression> mayBeSameAsConjuncts = new ArrayList<Expression>();
				for (Expression lv : logicalVariables) {
					boolean makeMayBeSameAs = true;
					if (newUniqueVariables.containsKey(lv)) {
						// lv is another unique variable, only output if
						// compares higher than this to prevent doing
						// X0 may be same as X1 and X1 may be same as X0
						// or
						// X0 may be same as X0
						if (lv.toString().compareTo(newUniqueVariable.toString()) <= 0) {
							makeMayBeSameAs = false;
						}
					}
					
					if (makeMayBeSameAs) {
						mayBeSameAsConjuncts.add(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_MAY_BE_SAME_AS, newUniqueVariable, lv));
					}
				}
				
				// | ......... newRule <- replace predicate1(..., NewUniqueVariable, ...) in newRule
				// | ......................... by predicate1(..., NewUniqueVariable, ...) and mayBeSame
				if (mayBeSameAsConjuncts.size() > 0) {
					ExtendPredicate1WithMayBeSameAs extendPredicate1Function = 
							new ExtendPredicate1WithMayBeSameAs(predicate1Functor, mayBeSameAsConjuncts);
					newRule = newRule.replaceFirstOccurrence(extendPredicate1Function, process);
				}
			}
			
			// |.... newRules <- add newRule
	        newRules.add(newRule);
		}
		
		// TODO - remove this check once we add back in proper support for translating rules functions in them.
		if (functionsIdentified.size() > 0) {
			throw new UnsupportedOperationException("Translating rules with functions (i.e. "+functionsIdentified.keySet()+") to those without is currently not properly supported.");
		}

		Set<Expression> identifiedFunctionalRandomVariableDeclarations = new LinkedHashSet<Expression>();
		// | // Add additional rules enforcing the functional relationship:
		// | for each (predicate, n) in functions
		for (String functor : functionsIdentified.keySet()) {
			Set<Integer> arityNs = functionsIdentified.get(functor);
			for (Integer arityN : arityNs) {				
				// |.... newRules <-
				// |........ add if predicate(X1, ..., Xn, Y) then not predicate(X1, ..., Xn, Z)
				// |..................... and Y may be same as Xn and Z may be same as Xn // i.e. add a pair of conjuncts for each Xn
				// |........ add there exists Y : predicate(X1, ..., Xn, Y)
				// |..................... and Y may be same as Xn // i.e. add a conjunct for each Xn
				newRules.addAll(createTransformedFunctionConstraints(functor, arityN));
				
				// |.... identify "randomVariable(predicate, n, Type1, ..., Typen, return_type)" declaration
				// |............... for function (predicate n) in randomVariableDeclarations
				Expression functionalRandomVariableDeclaration = null;
				Expression relationalRandomVariableDeclaration = null;
				for (Expression randomVariableDeclaration : randomVariableDeclarations) {
					if (randomVariableDeclaration.get(0).equals(functor) && randomVariableDeclaration.get(1).intValue() == arityN) {
						functionalRandomVariableDeclaration = randomVariableDeclaration;
						relationalRandomVariableDeclaration = updateRandomVariableDeclaration(functionalRandomVariableDeclaration);
						break;
					}
				}
				
				// |........ add relational "randomVariable(predicate, n + 1, Type1, ..., Typen, return_type, Boolean)"
				// |............... to newRandomVariableDeclarations
				if (functionalRandomVariableDeclaration != null) {
					identifiedFunctionalRandomVariableDeclarations.add(functionalRandomVariableDeclaration);
					newRandomVariableDeclarations.add(relationalRandomVariableDeclaration);
				}
			}
		}
		
		// | copy all randomVariableDeclarations, excluding identified function declarations, to newRandomVariableDeclarations
		newRandomVariableDeclarations.addAll(Util.setDifference(randomVariableDeclarations, identifiedFunctionalRandomVariableDeclarations));
		// | return (newRules, newRandomVariableDeclarations)
		Pair<List<Expression>, Set<Expression>> result = new Pair<List<Expression>, Set<Expression>>(newRules, newRandomVariableDeclarations);
		return result;
	}

	/**
	 * Checks if the given expression is a random function application.  It will screen out
	 * instances of built in functors, such as "and", "not", "there exists . : .", "if . then . else .",
	 * etc.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeIsRandomFunctionApplication
	 * 
	 * @param e  The expression to check.
	 * @return  False if the expression is not a random function application or any of the built in
	 *          expression types.
	 */
	public boolean isRandomFunctionApplication (Expression e) {		
		// If the expression is not a function application, return false.
		if (!e.getSyntacticFormType().equals(SYNTACTIC_FORM_TYPE_FUNCTION_APPLICATION)) {
			return false;
		}

		// If the expression is one of the known functors or terminal symbols, return false.
		if (isKnownFunctorOrTerminalSymbol(e.getFunctor())) {
			return false;
		}

		// Else, return true.
		return true;
	}
	
	
	public boolean isKnownFunctorOrTerminalSymbol(Expression functorOrSymbol) {
		boolean result = false;

		String functor = functorOrSymbol.getValue().toString();	
		
		if (FunctorConstants.BOOLEAN_FUNCTORS.contains(functor) ||
			functor.equals(FunctorConstants.EQUAL) ||
			functor.equals(FunctorConstants.INEQUALITY) ||
			FunctorConstants.ARITHMETIC_FUNCTORS.contains(functor) ||
			functor.equals(FunctorConstants.IF_THEN_ELSE) ||
			functor.equals(FUNCTOR_MAY_BE_SAME_AS) ||
			functor.equals(FUNCTOR_ATOMIC_RULE) ||
			functor.equals(FUNCTOR_CONDITIONAL_RULE) ||
			functor.equals(FUNCTOR_PROLOG_RULE) ||
			functor.equals(FUNCTOR_STANDARD_PROB_RULE) ||
			functor.equals(FUNCTOR_CAUSAL_RULE) ||
			functor.equals(FUNCTOR_QUERY) ||
			RuleTerminalSymbols.isTerminalSymbol(functor) ||
			AntlrGrinderTerminalSymbols.isTerminalSymbol(functor)) {
			result = true;
		}
		
		return result;
	}
	
	/**
	 * Checks if the given expression is a random function application.  It will catches
	 * cases of functions that take no arguments
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeIsRandomVariableValue
	 * 
	 * @param e                    The expression to check.
	 * @param randomVariableDeclarations  A set of random variable declarations.
	 * @return  True if the expression is a random function application or a function that takes no arguments.
	 */
	public boolean isRandomVariableValue(Expression e, Set<Expression> randomVariableDeclarations) {
		// If the expression is a random function application, return true.
		if (isRandomFunctionApplication(e)) {
			return true;
		}

		// If it is a symbol representing a function that takes no arguments, return true.
		if (e.getSyntacticFormType().equals(SYNTACTIC_FORM_TYPE_SYMBOL)) {
			for (Expression randomVariableDeclaration : randomVariableDeclarations) {						
				if (randomVariableDeclaration.get(0).equals(e) && randomVariableDeclaration.get(1).intValue() == 0) {
					return true;
				}
			}
		}

		return false;
	}
	
	/**
	 * Outputs newRule, a version of rule with random function applications used
	 * as arguments replaced with equivalent relational representations, using
	 * new unique variables.
	 * 
	 * Description of function:<br> 
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode:<br> 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateFunctionsAsArgument
	 * 
	 * 
	 * @param rule
	 *            a rule possible containing random function applications as
	 *            arguments.
	 * @param randomVariableDeclarations
	 *            current set of random variable declarations
	 * @param functionsIdentified
	 *            function applications that have been identified so far
	 *            (key=functor name, value= list of arities).
	 * @param newUniqueVariables
	 *            new unique variables that have been created (key=new unique
	 *            variable id, value=predicate1, functor was first encountered
	 *            in).
	 * @param newUniqueVariablesCache
	 *            a cache for use by sub-routines (key=predicate2(args), i.e.
	 *            functor unique variable created for, value=unique variable
	 *            id).
	 * @param uniqueCount
	 *            used to help construct new unique variable names.
	 * @return a version of rule with random function applications used as
	 *         arguments replaced with equivalent relational representation.
	 */
	public Expression translateFunctionsAsArgument(Expression rule, Set<Expression> randomVariableDeclarations, 
			Map<String, Set<Integer>> functionsIdentified, 
			Map<Expression, Expression> newUniqueVariables,
			Map<Expression, Expression> newUniqueVariablesCache,
			AtomicInteger uniqueCount,
			RewritingProcess process) {
		
		Expression result = null;
		//
		Pair<Expression, Expression> conditionAndFunctionFreeFormula = null;
		Expression                   condition                       = null;
		Expression                   functionFreeFormula             = null;
		
		// | if rule is a potential (i.e. a number or arithmetic expression that derives a number).
		if (isPotentialExpression(rule)) {
			// |.... return rule
			result = rule;				
		} // | if rule is an atomic rule of the form Formula Potential 
		else if (rule.getFunctor().equals(FUNCTOR_ATOMIC_RULE)) {
			// | .... (Condition, functionFreeFormula)
			// | ...... <- replaceRandomFunctionApplicationsByRelations(Formula, randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)			
			Expression formula   = rule.get(0);
			Expression potential = rule.get(1);
			conditionAndFunctionFreeFormula = replaceRandomFunctionApplicationsByRelations(formula, randomVariableDeclarations,
														functionsIdentified, newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			condition = conditionAndFunctionFreeFormula.first;
			// | .... if Condition is distinct from "true"
			if (!condition.equals(Expressions.TRUE)) {
				functionFreeFormula = conditionAndFunctionFreeFormula.second;

				// | ........ return translateFunctionsAsArgument("if Condition then functionFreeFormula Potential", randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)
				Expression intermediateRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE,
														condition,
														Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_ATOMIC_RULE,
																functionFreeFormula, potential));
				result = translateFunctionsAsArgument(intermediateRule, randomVariableDeclarations, functionsIdentified,
														newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			}
			else {
				// | .... else
				// | ........ return rule // i.e. no change
				result = rule;
			}
		}
		else if (rule.getFunctor().equals(FUNCTOR_CONDITIONAL_RULE) && rule.numberOfArguments() == 2) {
			// | if rule is a conditional rule of the form if Formula then Rule1
			// | .... (Condition, functionFreeFormula)
			// | ...... <- replaceRandomFunctionApplicationsByRelations(Formula, randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)
			Expression formula = rule.get(0);
			Expression rule1   = rule.get(1);
			conditionAndFunctionFreeFormula = replaceRandomFunctionApplicationsByRelations(formula, randomVariableDeclarations,
														functionsIdentified, newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			condition           = conditionAndFunctionFreeFormula.first;
			functionFreeFormula = conditionAndFunctionFreeFormula.second;	
			
			// | .... functionFreeRule1
			// | ...... <- translateFunctionsAsArgument(Rule1, randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)
			Expression functionFreeRule1 = translateFunctionsAsArgument(rule1, randomVariableDeclarations, functionsIdentified, newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);

			// | .... if Condition is distinct from "true"
			if (!condition.equals(Expressions.TRUE)) {
					
				// | ........ return translateFunctionsAsArgument("if Condition then (if functionFreeFormula then functionFreeRule1)", randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)
				Expression intermediateRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE,
													condition,
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE,
															functionFreeFormula, functionFreeRule1));
			 
				result = translateFunctionsAsArgument(intermediateRule, randomVariableDeclarations, functionsIdentified,	
												newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			}
			else {
				// | ........ if rule1 == functionFreeRule1
				if (rule1 == functionFreeRule1) {
					// | ............ return rule // i.e. no change
					result = rule;
				}
				else {
					// | ............ return if functionFreeFormula then functionFreeRule1
					result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE, functionFreeFormula, functionFreeRule1);
				}
			}
		}
		else if (rule.getFunctor().equals(FUNCTOR_CONDITIONAL_RULE) && rule.numberOfArguments() == 3) {
			// | if rule is a conditional rule of the form if Formula then Rule1 else Rule2
			// | .... (Condition, functionFreeFormula)
			// | ...... <- replaceRandomFunctionApplicationsByRelations(Formula, randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)
			Expression formula = rule.get(0);
			Expression rule1   = rule.get(1);
			Expression rule2   = rule.get(2);
			conditionAndFunctionFreeFormula = replaceRandomFunctionApplicationsByRelations(formula, randomVariableDeclarations,
														functionsIdentified, newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			condition           = conditionAndFunctionFreeFormula.first;
			functionFreeFormula = conditionAndFunctionFreeFormula.second;
			
			// | .... functionFreeRule1
			// | ...... <- translateFunctionsAsArgument(Rule1, randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)
			Expression functionFreeRule1 = translateFunctionsAsArgument(rule1, randomVariableDeclarations, functionsIdentified, newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			
			// | .... functionFreeRule2
			// | ...... <- translateFunctionsAsArgument(Rule2, randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)
			Expression functionFreeRule2 = translateFunctionsAsArgument(rule2, randomVariableDeclarations, functionsIdentified, newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			// | .... if Condition is distinct from "true"
			if (!condition.equals(Expressions.TRUE)) {
				// | ........ return translateFunctionsAsArgument("if Condition then (if functionFreeFormula then functionFreeRule1 else functionFreeRule2)", randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)
				Expression intermediateRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE,
													condition,
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE,
															functionFreeFormula, functionFreeRule1, functionFreeRule2));
				result = translateFunctionsAsArgument(intermediateRule, randomVariableDeclarations, functionsIdentified,
														newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			}
			else {
				// | ........ if rule1 == functionFreeRule1 and rule2 == functionFreeRule2
				if (rule1 == functionFreeRule1 && rule2 == functionFreeRule2) {
					// | ............ return rule // i.e. no change
					result = rule;
				}
				else {
					// | ............ return if functionFreeFormula then functionFreeRule1 else functionFreeRule2
					result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE, functionFreeFormula, functionFreeRule1, functionFreeRule2);
				}
			}
		} 
		else if (rule.getFunctor().equals(FUNCTOR_PROLOG_RULE)) {
			// | if rule is a prolog rule
			Expression intermediateRule = null;
			if (rule.numberOfArguments() == 2) {
				// | .... if rule is "Potential Formula1."
				// | ........ intermediateRule <- Formula1 Potential // i.e. an equivalent atomic rule
				intermediateRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_ATOMIC_RULE, rule.get(1), rule.get(0));
			}
			else {
				// | .... else rule is "Potential Formula1 :- Formula2."
				// | ........ intermediateRule <- if Formula2 then Formula1 Potential // i.e. an equivalent conditional rule
				intermediateRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE, rule.get(2), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_ATOMIC_RULE, rule.get(1), rule.get(0)));
			}
			// | .... return translateFunctionsAsArgument(intermediateRule, randomVariableDeclarations, functions, newUniqueVariables, newUniqueVariablesCache)
			result = translateFunctionsAsArgument(intermediateRule, randomVariableDeclarations, functionsIdentified,
													newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			if (result == intermediateRule) {
				// i.e. no change
				result = rule;
			}
		}
		else if (rule.getFunctor().equals(FUNCTOR_STANDARD_PROB_RULE)) {
			Expression intermediateRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE, rule.get(1), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_ATOMIC_RULE, rule.get(0), rule.get(2)));
			
			result = translateFunctionsAsArgument(intermediateRule, randomVariableDeclarations, functionsIdentified,
													newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			if (result == intermediateRule) {
				// i.e. no change
				result = rule;
			}
		}
		else if (rule.getFunctor().equals(FUNCTOR_CAUSAL_RULE)) {
			Expression intermediateRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE, rule.get(0), rule.get(1));
			result = translateFunctionsAsArgument(intermediateRule, randomVariableDeclarations, functionsIdentified,
													newUniqueVariables, newUniqueVariablesCache, uniqueCount, process);
			if (result == intermediateRule) {
				// i.e. no change
				result = rule;
			}
		}
		
		if (result == null) {
			throw new UnsupportedOperationException("translateFunctionsAsArguments: does not know how to handle rule="+rule);
		}
		return result;
	}
	
	/**
	 * Pseudocode:<br> 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateFunctionsAsArgument
	 * 
	 * 
	 * @param formula
	 *            a formula possibly containing random function applications as
	 *            arguments.
	 * @param randomVariableDeclarations
	 *            current set of random variable declarations
	 * @param functionsIdentified
	 *            function applications that have been identified so far
	 *            (key=functor name, value= list of arities).
	 * @param newUniqueVariables
	 *            new unique variables that have been created (key=new unique
	 *            variable id, value=predicate1, functor was first encountered
	 *            in).
	 * @param newUniqueVariablesCache
	 *            a cache for use by sub-routines (key=predicate2(args), i.e.
	 *            functor unique variable created for, value=unique variable
	 *            id).
	 * @param uniqueCount
	 *            used to help construct new unique variable names.
	 * @return (Condition, functionFreeFormula).
	 */
	public Pair<Expression, Expression> replaceRandomFunctionApplicationsByRelations(Expression formula, 
			final Set<Expression> randomVariableDeclarations, 
			final Map<String, Set<Integer>> functionsIdentified, 
			final Map<Expression, Expression> newUniqueVariables,  
			final Map<Expression, Expression> newUniqueVariablesCache,
			final AtomicInteger uniqueCount,
			final RewritingProcess process) {
		
		// | Condition <- true
		final Expression[] condition = new Expression[1];
		condition[0] = Expressions.TRUE;
		// 
		Expression functionFreeFormula       = formula;
		Expression toReplace                 = formula;
		final Expression[] currentExpression = new Expression[1];
		// |.... newRule <- exhaustively replace each expression E in rule
		// |........ with the following replacement function:
		do {
			// | functionFreeFormula <- exhaustively replace each expression E in Formula with the following replacement function:
		    toReplace            = functionFreeFormula;
		    currentExpression[0] = toReplace;
		    functionFreeFormula  = toReplace.replaceAllOccurrences(
	    		new AbstractReplacementFunctionWithContextuallyUpdatedProcess() {
					@Override
					public Expression apply(Expression expression, RewritingProcess process) {
						Expression result = expression;
						// | .... if isRandomFunctionApplication(E) of the form predicate1(E1,..., Ei,..., En)
						// | ............ where isRandomVariableValue(Ei, declarations) is true
						int elementEi = determineIfRandomFunctionApplicationWithEmbeddedRandomVariableValue(expression, randomVariableDeclarations, process);
						if (elementEi != -1) {
							Expression predicate1 = expression;
							// | ........ (predicate2, (T1,...,Tk) ) <- functorAndArguments(Ei)
							Expression predicate2 = predicate1.get(elementEi);
							// | ........ if newUniqueVariable not cached for (predicate2, (T1,...,Tk)) in newUniqueVariablesCache
							Expression newUniqueVariable = newUniqueVariablesCache.get(predicate2);
							if (newUniqueVariable == null) {
								// | ............ functions <- add (predicate2, n) to functions
								updateFunctionsIdentified(functionsIdentified, predicate2);
								// | ............ newUniqueVariable <- make new unique variable;
								newUniqueVariable = Expressions.makeUniqueVariable("X" + (uniqueCount.incrementAndGet()), currentExpression[0], process);
								// | ............ newUniqueVariables <- add newUniqueVariable
								// Note: also keep track of the predicate1 from which the newUniqueVariable originated from.
								newUniqueVariables.put(newUniqueVariable, predicate1.getFunctor());
								// | ............ newUniqueVariablesCache <- add newUniqueVariable
								newUniqueVariablesCache.put(predicate2, newUniqueVariable);
								// | ............ newUniqueVariablesCache <- add newUniqueVariable
							} // | ........ else use cached newUniqueVariable
							
							// | ........ Condition <- Condition and predicate2(T1, ..., Tk, newUniqueVariable)
							List<Expression> conjuncts = new ArrayList<Expression>();
							if (And.isConjunction(condition[0])) {
								conjuncts.addAll(And.getConjuncts(condition[0]));
							}
							else {
								conjuncts.add(condition[0]);
							}
							// Extend predicate2 from a function to a relation
							List<Expression> predicate2Args = new ArrayList<Expression>(predicate2.getArguments());
							predicate2Args.add(newUniqueVariable);
							predicate2 = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(predicate2.getFunctorOrSymbol(), predicate2Args.toArray());
							conjuncts.add(predicate2);
							condition[0] = And.make(conjuncts);
								
							// | ........ return predicate1(E1,..., Ei-1, newUniqueVariable, Ei+1, ..., En)
							// Replace predicate1's function slot with the newUniqueVariable
							List<Expression> predicate1Args = new ArrayList<Expression>(predicate1.getArguments());
							predicate1Args.set(elementEi, newUniqueVariable);
							predicate1 = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(predicate1.getFunctor(), predicate1Args.toArray());
							result = predicate1;
						}
						return result;
					}
	    		},
	    		process);
		} while (functionFreeFormula != toReplace);
		
		Pair<Expression, Expression> result = new Pair<Expression, Expression>(condition[0], functionFreeFormula);
		
		return result;
	}

	/**
	 * Removes the quantifiers from potential expressions.
	 * 
	 * Description of function: 
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode:
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateQuantifiers
	 * 
	 * @param rules
	 *            A list of rules possible containing quantifiers.
	 * @param randomVariableDeclarations
	 *            A set of random variable declarations.
	 * @return (rules, randomVariableDeclarations).
	 */
	public Pair<List<Expression>, Set<Expression>> translateQuantifiers(List<Expression> rules, Set<Expression> randomVariableDeclarations, RewritingProcess process) {
		// | newRules <- empty list
		List<Expression> newRules = new ArrayList<Expression>();
		// | newRandomVariableDeclarations <- randomVariableDeclarations
		Set<Expression>  newRandomVariableDeclarations = new LinkedHashSet<Expression>(randomVariableDeclarations);

		// Create a local copy as will be extending the collection during
		// the following replacement logic in order to support nested
		// quantifiers.
		// | expandingRules <- rules // to support nested quantifiers
		List<Expression> expandingRules = new ArrayList<Expression>(rules);
		
		// | for each rule in expandingRules
		for (int i = 0; i < expandingRules.size(); i++) {
			Expression newRule = expandingRules.get(i);

			// | .... newRule <- exhaustively (*) replace each expression E in rule
			// | ............ with the following replacement function
			ReplaceQuantifierFunction replacementFunction = new ReplaceQuantifierFunction(expandingRules, newRandomVariableDeclarations);
			Expression toReplace;
			do {
				// Replace the quantifiers until there are none left in the potential expression.
			    toReplace = newRule;
			    newRule   = toReplace.replaceAllOccurrences(replacementFunction, process);
			} while (newRule != toReplace);

			// | .... newRules <- add newRule
			newRules.add(newRule);
		}
		
		// | return (newRules, newRandomVariableDeclarations)
		Pair<List<Expression>, Set<Expression>> result = new Pair<List<Expression>, Set<Expression>>(newRules, newRandomVariableDeclarations);
		return result;
	}
	
	/**
	 * Convert the given rules into a potential expressions.
	 * 
	 * Description of function: http://code.google.com/p/aic-praise/wiki/
	 * TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: http://code.google.com/p/aic-praise/wiki/
	 * PseudoCodeRuleList2PotentialExpressions
	 * 
	 * @param rules
	 *            The rules to translate to potential expressions.
	 * @return The potential expressions created from the rules..
	 */
	public List<Expression> ruleList2PotentialExpressions(List<Expression> rules) {
		// | potentialExpressions <- emptyList
		List<Expression> potentialExpressions = new ArrayList<Expression>();
		
		// | for each rule in ruleList
		for (Expression rule : rules) {
			// |.... potentialExpressions <- add rule2PotentialExpression(rule)
			potentialExpressions.add(rule2PotentialExpression(rule));
		}
		
		// | return potentialExpressions
		return potentialExpressions;
	}
	
	/**
	 * Convert the given rule into a potential expression.
	 * 
	 * Description of function: http://code.google.com/p/aic-praise/wiki/
	 * TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: http://code.google.com/p/aic-praise/wiki/
	 * PseudoCodeRule2PotentialExpression
	 * 
	 * @param rule
	 *            The rule to translate.
	 * @return The potential expression form of the rule.
	 */
	public Expression rule2PotentialExpression(Expression rule) {
		Expression result;
		if (rule.getFunctor() == null) {
			result = rule;
		}
		else if (rule.getFunctor().equals(FUNCTOR_ATOMIC_RULE)) {
			result = translateAtomicRule(rule);
		}
		else if (rule.getFunctor().equals(FUNCTOR_CONDITIONAL_RULE)) {
			result = translateConditionalRule(rule);
		}
		else if (rule.getFunctor().equals(FUNCTOR_PROLOG_RULE)) {
			result = translatePrologRule(rule);
		}
		else if (rule.getFunctor().equals(FUNCTOR_STANDARD_PROB_RULE)) {
			result = translateStandardProbabilityRule(rule);
		}
		else if (rule.getFunctor().equals(FUNCTOR_CAUSAL_RULE)) {
			result = translateCausalRule(rule);
		}
		else {
			result = rule;
		}
		return result;
	}
	
	/**
	 * Convert the given atomic rule into its "if . then . else ." form.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeRule2PotentialExpression
	 * 
	 * @param rule  The atomic rule to translate.
	 * @return  The equivalent "if . then . else ." form of the atomic rule.
	 */
	public Expression translateAtomicRule (Expression rule) {
		List<Expression> arguments = rule.getArguments();
		if (arguments.size() != 2) {
			return null;
		}
		// | if rule is "Formula Potential" // implementation note: tested with hasFunctor("atomic rule")
		// |.... return "if Formula then Potential else <1 - Potential>"
		return IfThenElse.make(arguments.get(0), arguments.get(1), oneMinusPotential(arguments.get(1)));
	}
	
	/**
	 * Convert the given conditional rule into its "if . then . else ." form.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeRule2PotentialExpression
	 * 
	 * @param rule  The conditional rule to translate.
	 * @return  The equivalent "if . then . else ." form of the conditional rule.
	 */
	public Expression translateConditionalRule (Expression rule) {
		List<Expression> args = rule.getArguments();
		// | if rule is "if Formula then Rule"
		if (args.size() == 2) {
			// | .... return "if Formula then rule2PotentialExpression(Rule) else 0.5"
			Expression result =
					IfThenElse.make(
							args.get(0), this.rule2PotentialExpression(args.get(1)), Expressions.makeSymbol(0.5));
			return result;
		} // | if rule is "if Formula then Rule1 else Rule2" 
		else if (args.size() == 3) {
			// | .... return "if Formula then rule2PotentialExpression(Rule1) else rule2PotentialExpression(Rule)"
			Expression result =
					IfThenElse.make(
							args.get(0),
							this.rule2PotentialExpression(args.get(1)),
							this.rule2PotentialExpression(args.get(2)));
			return result;
		}
		return null;
	}
	
	/**
	 * Convert the given prolog rule into its "if . then . else ." form.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeRule2PotentialExpression
	 * 
	 * @param rule  The prolog rule to translate.
	 * @return  The equivalent "if . then . else ." form of the prolog rule.
	 */
	public Expression translatePrologRule (Expression rule) {
		List<Expression> args = rule.getArguments();
		
		// | if rule is "Potential Formula1." // tested with hasFunctor("prolog rule")
		if (args.size() == 2) {
			// |.... return "if Formula1 then Potential else <1 - Potential>"
			return IfThenElse.make(args.get(1), args.get(0), oneMinusPotential(args.get(0)));
		} // | if rule is "Potential Formula1 :- Formula2." // tested with hasFunctor("prolog rule")
		else if (args.size() == 3){
			// |.... return "if Formula2 then if Formula1 then Potential else <1 - Potential> else 0.5"
			Expression result =
					IfThenElse.make(args.get(2), 
							IfThenElse.make(args.get(1), args.get(0), oneMinusPotential(args.get(0))),
							Expressions.makeSymbol(0.5));
			return result;
		}

		return null;
	}
	
	/**
	 * Takes a list of rules and converts any instances of rule conjunctions found within it.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/SyntaxAndMeaningOfProbabilisticRules
	 * 
	 * @param rules  The rules to check for conjunctions.
	 * @return       A copy of the rules with all instances of the conjunctions converted.
	 */
	public List<Expression> translateConjunctions (List<Expression> rules) {
		List<Expression> result = new ArrayList<Expression>();
		// First, unpack any first level conjunctions.
		for (Expression rule : rules) {
			if (Tuple.isTuple(rule)) {
				// If the rule is in a tuple (as conjunctions are packaged), unpack the contents out of the tuple.
				List<Expression> args = rule.getArguments();
				for (Expression expr : args) {
					result.add(expr);
				}
			}
			else {
				result.add(rule);
			}
		}

		// Next, unroll any conjunctions in conditional rules.
		List<Expression> temp = result;
		result = new ArrayList<Expression>();
		for (Expression rule : temp) {
			result.addAll(translateConditionalConjunctions(rule));
		}
		
		return result;
	}

	/**
	 * Translates any conjunctions found in the given rule.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/SyntaxAndMeaningOfProbabilisticRules
	 * 
	 * @param rule The rule to check for conjunctions.
	 * @return     A list of rules generated by removing any conjunctions found in the rule.  If
	 *             no conjunctions were found, the list will contain the original rule.  To check if
	 *             there were any conjunctions in the rule, check the size of the returned list as
	 *             the reference may be different.
	 */
	public List<Expression> translateConditionalConjunctions (Expression rule) {
		ArrayList<Expression> result = new ArrayList<Expression>();
		if (rule.hasFunctor(FUNCTOR_CONDITIONAL_RULE)) {
			// If has conjunction, unpack the conjunction.
			List<Expression> args = rule.getArguments();
			int size1 = 0;
			int size2 = 0;

			// Check if either of the rule arguments for the conditional rule is a tuple,
			// which means it is a conjunction.
			if (args.size() >= 2) {
				size1 = 1;
				if (Tuple.isTuple(args.get(1))) {
					size1 = args.get(1).getArguments().size();
				}
			}

			// Checking if the second rule arg (if it exists) is a conjunction.
			if (args.size() == 3) {
				size2 = 1;
				if (Tuple.isTuple(args.get(2))) {
					size2 = args.get(2).getArguments().size();
				}
			}

			// Find the max size and generate the 
			int size = (size1 > size2) ? size1 : size2;
			for (int ii = 0; ii < size; ii++) {
				Expression rule1 = null;
				Expression rule2 = null;

				// Add the first argument.
				if (ii >= size1) {
					// If we're beyond the range for argument 2, insert default value.
					rule1 = Expressions.makeSymbol(0.5);
				}
				else {
					// If we're still in range for argument 1, get the value.
					if (size1 == 1) {
						// If there was no conjunction for argument 1, just get the value.
						rule1 = args.get(1);
					}
					else {
						// Otherwise, we have to retrieve the value from the conjunction.
						rule1 = args.get(1).get(ii);
					}
				}

				// Get the value for the second rule argument.
				// If there is no second rule in the conditional rule, then don't add a second arg.
				if (!(size2 == 0)) {
					if (ii >= size2) {
						// If we're beyond the range for argument 2, insert default value.
						rule2 = Expressions.makeSymbol(0.5);
					}
					else {
						// If we're still in range for argument 2, get the value.
						if (size2 == 1) {
							// If there was no conjunction for argument 1, just get the value.
							rule2 = args.get(2);
						}
						else {
							// Otherwise, we have to retrieve the value from the conjunction.
							rule2 = args.get(2).get(ii);
						}
					}
				}
				
				// Now check if the arguments have conjunctions.
				// First, run the arguments through the conjunction translation.
				Expression formula = args.get(0);
				List<Expression> arg1List = this.translateConditionalConjunctions(rule1);
				List<Expression> arg2List = null;
				if (size2 > 0) {
					arg2List = this.translateConditionalConjunctions(rule2);
				}

				// Now, for each of the two rule positions in the conditional rule, there may be 0, 1, or many
				// rules.  Now, we work through the various cases, and generate the new conditional rules.
				if (arg2List == null) {
					// If there is only one rule argument, create new rules for the list of rules (or single rule) for
					// them.
					for (Expression arg1 : arg1List) {
						result.add(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(rule.getFunctor(), formula, arg1));
					}
				}
				else {
					if (arg1List.size() > 1 && arg2List.size() > 1) {
						// TODO: Figure out how to handle cases where there are nested conjunctions on 
						// both arguments of a conditional rule.
						throw new UnsupportedOperationException(
								"Currently, do not support nested conditional rule lists in both sides of conditional rule lists.");
					}
					else if (arg1List.size() > 1) {
						// If argument one has conjunctions in it, create new rules for those.
						for (Expression arg1 : arg1List) {
							result.add(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(rule.getFunctor(), formula, arg1, arg2List.get(0)));
						}
					}
					else {
						// If argument two has conjunctions in it, create new rules for those.  This also
						// handles cases where neither argument one or two had conjunctions.
						for (Expression arg2 : arg2List) {
							result.add(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(rule.getFunctor(), formula, arg1List.get(0), arg2));
						}
					}
				}
			}
		}
		else if (rule.hasFunctor(FUNCTOR_CAUSAL_RULE)) {
			List<Expression> args = rule.getArguments();
			Expression formula = args.get(0);
			Expression ruleArg = args.get(1);
			if (Tuple.isTuple(ruleArg)) {
				// If the rule argument is a conjunction, create new causal rules for each one.
				for (Expression conjunctionElement : ruleArg.getArguments()) {
					// Check for more conjunctions in the conjunction element.
					List<Expression> translatedList = this.translateConditionalConjunctions(conjunctionElement);
					for (Expression translatedElement: translatedList) {
						result.add(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(rule.getFunctor(), formula, translatedElement));
					}
				}
			}
			else {
				// If no conjunction, then just put the original rule in the result.
				result.add(rule);
			}
		}
		else {
			result.add(rule);
		}
		return result;
	}

	/**
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/SyntaxAndMeaningOfProbabilisticRules
	 * 
	 * @param rule
	 * @return
	 */
	public Expression translateStandardProbabilityRule (Expression rule) {
		List<Expression> args = rule.getArguments();
		if (args.size() == 3) {
			return this.translateConditionalRule(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE, args.get(1), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_ATOMIC_RULE, args.get(0), args.get(2))));
		}
		return null;
	}

	/**
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/SyntaxAndMeaningOfProbabilisticRules
	 * 
	 * @param rule
	 * @return
	 */
	public Expression translateCausalRule (Expression rule) {
		List<Expression> args = rule.getArguments();
		if (args.size() == 2) {
			return this.translateConditionalRule(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_CONDITIONAL_RULE, args.get(0), 
					args.get(1)));
		}
		return null;
	}
	

	/**
	 * Removes the constraints embedded in the given potential expressions.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeDisembedConstraints
	 * 
	 * @param potentialExpressions  The potential expressions to check for embedded constraints.
	 * @return  A list of pairs of constraint-free potential expressions and the extracted constraints.
	 */
	public List<Pair<Expression, Expression>> disembedConstraints (List<Expression> potentialExpressions, RewritingProcess process) {
		// | Let potentialExpressions be a list P1, ..., Pn of potential expressions
		// | setOfConstrainedPotentialExpressions <- empty list
		List<Pair<Expression, Expression>> setOfConstrainedPotentialExpressions = 
				new ArrayList<Pair<Expression, Expression>>();
		
		// | for each P in P1,..., Pn
		for (Expression potentialExpression : potentialExpressions) {
			RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(potentialExpression, process);

			// | .... mayBeSameAsList <- empty list
			Set<Pair<Expression, Expression>> mayBeSameAsSet = new LinkedHashSet<Pair<Expression, Expression>>();

			// Gather instances of ". may be same as .".
			Expression toReplace = potentialExpression;
			Expression replaced  = toReplace;

			// | .... P <- replace every expression in P using replacement function
			ReplaceMayBeSameAsFunction replacementFunction = new ReplaceMayBeSameAsFunction(mayBeSameAsSet);
			do {
			    toReplace = replaced;
			    replaced = toReplace.replaceAllOccurrences(replacementFunction, subProcess);
			} while (replaced != toReplace);

			potentialExpression = replaced;

			// Simplify the updated expression.
			// | .... P <- R_normalize(P)
			potentialExpression = subProcess.rewrite(LBPRewriter.R_normalize, potentialExpression);

			// Get free variables and create inequality constraints on all pairs except those
			// pairs stated to be ". may be same as .".
			// | .... C <- true
			Set<Expression> variables = Expressions.freeVariables(potentialExpression, process);
			List<Expression> constraints = new ArrayList<Expression>();
			Expression[] variableArray = new Expression[variables.size()];
			variables.toArray(variableArray);
			// | .... for every pair of distinct variables (Variable1, Variable2) in P not in mayBeSameAsList
			for (int ii = 0; ii < variables.size() - 1; ii++) {
				for (int jj = ii+1; jj < variables.size(); jj++) {
					// Check if this pair is in the ". may be same as ." set.
					Expression arg1 = variableArray[ii];
					Expression arg2 = variableArray[jj];
					if (!mayBeSameAsSet.contains(new Pair<Expression, Expression>(arg1, arg2))) {
						// If the pair is not in the ". may be same as ." set, then add it to the list of constraints.
						constraints.add(Disequality.make(arg1, arg2));
					}
				}
			}
			// | .... setOfConstrainedPotentialExpressions <- add (P, C)
			setOfConstrainedPotentialExpressions.add(
					new Pair<Expression, Expression>(potentialExpression, And.make(constraints)));
		}
		
		// | setOfConstrainedPotentialExpressions <- simplify(setOfConstrainedPotentialExpressions)
		List<Pair<Expression, Expression>> simplifiedSetOfConstrainedPotentialExpressions = 
				new ArrayList<Pair<Expression, Expression>>();
		for (int i = 0; i < setOfConstrainedPotentialExpressions.size(); i++) {
			// |.... remove all (P,C) from setOfConstrainedPotentialExpressions
			Pair<Expression, Expression> pair = setOfConstrainedPotentialExpressions.get(i);
			Expression expressionP = pair.first;
			Expression constraintC = pair.second;
			// |........ where P is a numeric constant or C is 'false'
			if (!Expressions.isNumber(expressionP) && !constraintC.equals(Expressions.FALSE)) {
				simplifiedSetOfConstrainedPotentialExpressions.add(pair);
			}
		}
		
		setOfConstrainedPotentialExpressions = simplifiedSetOfConstrainedPotentialExpressions;

		// Extract the embedded constraints from the potential expressions.
		List<Pair<Expression, Expression>> result = new ArrayList<Pair<Expression, Expression>>();
		// | while there is (P, C) in setOfConstrainedPotentialExpressions such that P contains a formula
		// | ............ AbstractEqualityConstraint involving only equalities and disequalities (a constraint)
		for (int ii = 0; ii < setOfConstrainedPotentialExpressions.size(); ii++) {
			// | .... remove (P, C) from setOfConstrainedPotentialExpressions
			// Check if the potential expression has any more embedded constraints.
			Pair<Expression, Expression> pair = setOfConstrainedPotentialExpressions.get(ii);
			Expression potentialExpression = pair.first;
			Expression constraintC         = pair.second;
			RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(Tuple.make(potentialExpression, constraintC), process);
			CollectAtomicConstraint collectAtomicConstraint = new CollectAtomicConstraint(subProcess);
			if (Util.thereExists(new SubExpressionsDepthFirstIterator(potentialExpression), collectAtomicConstraint)) {
				// // | .... for Assumption in (AbstractEqualityConstraint, not AbstractEqualityConstraint)
				Expression assumption = collectAtomicConstraint.constraint;
				addFurtherConstrainedPotentialExpression(setOfConstrainedPotentialExpressions, potentialExpression, constraintC, assumption, subProcess);
				Expression notAssumption = Not.make(collectAtomicConstraint.constraint);
				addFurtherConstrainedPotentialExpression(setOfConstrainedPotentialExpressions, potentialExpression, constraintC, notAssumption, subProcess);
			}
			else {
				result.add(pair);
			}
		}
		
		// | return setOfConstrainedPotentialExpressions
		return result;		
	}
	
	/**
	 * Convert a list of potential expressions into parfactors.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeConstraintedPotentialExpressions2Parfactors
	 * 
	 * @param constrainedPotentialExpressions
	 *            the constrainted potential expressions.
	 * @return a set of parfactors generated from the give potential
	 *         expressions.
	 */
	public Set<Expression> constraintedPotentialExpressionsToParfactors(List<Pair<Expression, Expression>> constrainedPotentialExpressions, Set<Expression> randomVariableDeclarations, RewritingProcess process) {				

		// Translate the potential expression/constraint pair into a parfactor.
		// | parfactors <- empty list
		Set<Expression> parfactors = new LinkedHashSet<Expression>();
		// | for each (P, C) in constrainedPotentialExpressions
		for (Pair<Expression, Expression> pair : constrainedPotentialExpressions) {
			// |.... parfactors <- add R_normalize({{ (on <free variables in P and C>) [ P ] | C }})
			parfactors.add(createParfactor(pair.first, pair.second, randomVariableDeclarations, process));
		}
		
		// | return parfactors
		return parfactors;
	}

	/**
	 * Makes a parfactor from the given potential expression and a constraint.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeConstraintedPotentialExpressions2Parfactors
	 * 
	 * @param potentialExpression  The potential expression to convert into a parfactor.
	 * @param constraintC          The constraint for the parfactor.
	 * @return A parfactor expression based on the potential expression on constraints.
	 */
	public Expression createParfactor(Expression potentialExpression, Expression constraintC, Set<Expression> randomVariableDeclarations, RewritingProcess process) {
		// |.... parfactors <- add R_normalize({{ (on <free variables in P and C>) [ P ] | C }})

		IndexExpressionsSet indexExpressions = null;

		indexExpressions =
				LPIUtil.getIndexExpressionsFromRandomVariableUsage(
						Tuple.make(potentialExpression, constraintC), randomVariableDeclarations, process);
		
		Expression result = new DefaultIntensionalMultiSet(indexExpressions, BracketedExpressionSubExpressionsProvider.make(potentialExpression), constraintC);
	
		result = process.rewrite(LBPRewriter.R_normalize, result);
	
		return result;
	}
	
	/**
	 * Converts a query expression into a query atom and a query rule.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingInferenceInputAndOutput
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeQueryRuleAndAtom
	 * 
	 * @param query  
	 * 			The query expression
	 * @param randomVariableDeclarations
	 *          The current collection of random variable declarations.
	 * @return A pair with the query rule and query atom.
	 * @throws QueryContainsUnknownRandomVariablesException
	 */
	public Pair<Expression, Expression> queryRuleAndAtom(Expression query, Set<Expression> randomVariableDeclarations, RewritingProcess process) throws QueryContainsUnknownRandomVariablesException {
		Pair<Expression, Expression> result;
		
		// Firstly, ensure the random variables present in the query exist in the model
		Set<Expression> unknownRandomVariables = collectUnknownRandomVariables(query, randomVariableDeclarations, process);		
		if (unknownRandomVariables.size() > 0) {
			throw new QueryContainsUnknownRandomVariablesException(unknownRandomVariables);
		}
		
		// | if queryFormula is isRandomVariableValue(queryFormula, highLevelDeclarations)
		// | .... return null
		if (isRandomVariableValue(query, randomVariableDeclarations)) {
			result = null;
		}
		else {			
			// | F <- free variables of queryFormula
			Set<Expression> variablesF = Expressions.freeVariables(query, process);
			// | queryAtom <- predicate 'query' applied to F
			Expression queryAtom;
			if (variablesF.size() > 0) {
				queryAtom = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_QUERY, variablesF);
			}
			else {
				queryAtom = Expressions.makeSymbol(FUNCTOR_QUERY);
			}
			// | rule <- queryAtom <=> queryFormula
			Expression queryRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FUNCTOR_ATOMIC_RULE, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(Equivalence.FUNCTOR, queryAtom, query), 1);
			// | return (rule, queryAtom)
			result = new Pair<Expression, Expression>(queryRule, queryAtom);
		}
		
		return result;
	}

	/**
	 * Translates a potential expression to a rule expression.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingInferenceInputAndOutput
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeQueryAnswerPotentialExpression2Rule
	 * 
	 * @param input     The potential expression to translate into a rule expression.
	 * @param query     The atom representing the query in the result potential expression.
	 * @return          A rule expression version of the potential expression.
	 */
	public Expression queryAnswerPotentialExpression2Rule(Expression input, Expression queryAtom, RewritingProcess process) {
		boolean isIfThenElse = IfThenElse.isIfThenElse(input);
		
		// we can only really simplify if then else expressions
		if (isIfThenElse) {
			Expression condition = IfThenElse.getCondition(input);
			boolean isConstraint = LPIUtil.isConstraint(condition, process);
			if (isConstraint) {
				Expression translationOfE1 = queryAnswerPotentialExpression2Rule(input.get(1), queryAtom, GrinderUtil.extendContextualConstraint(condition, process));
				Expression translationOfE2 = queryAnswerPotentialExpression2Rule(input.get(2), queryAtom, GrinderUtil.extendContextualConstraint(Not.make(condition), process));
				
				//if both clauses are true, result is true
				if (translationOfE1.equals(Expressions.TRUE) && translationOfE2.equals(Expressions.TRUE)) {
					return Expressions.TRUE;
				} 
				//if the then clause is true, return the else clause
				else if (translationOfE1.equals(Expressions.TRUE)) {
					return Expressions.apply(
							FUNCTOR_CONDITIONAL_RULE,
							Not.make(condition),
							translationOfE2);
				} 
				//if the else clause is true, return the if clause
				else if (translationOfE2.equals(Expressions.TRUE)) {
					return Expressions.apply(
							FUNCTOR_CONDITIONAL_RULE,
							condition,
							translationOfE1);
				}
				//if neither is true, then return the simplified form
				else {
					return Expressions.apply(
							FUNCTOR_CONDITIONAL_RULE, 
							condition, 
							translationOfE1, 
							translationOfE2);
				}
			}
			else {
				//assume that the 'condition' is a random variable value
				
				Rational potential = input.get(1).rationalValue();
				
				if (potential.isZero()) { // things with 0 potential are negations; it's more intuitive to convert them to that.
					if (condition.hasFunctor(FunctorConstants.NOT)) { // negate condition, avoiding double negations
						condition = condition.get(0);
					}
					else {
						condition = Not.make(condition);
					}
					potential = new Rational(1);
				}
				else if (potential.compareTo(1) < 0 && condition.hasFunctor(FunctorConstants.NOT)) {
					// 'unlikely negations' are better understood as likely statements -- eliminating a sort of double negation
					condition = condition.get(0);
					potential = potential.subtract(1).negate(); // this is the same as potential = 1.0 - potential;
				}
				
				Expression result = Expressions.apply(FUNCTOR_ATOMIC_RULE, condition, Expressions.makeSymbol(potential));
				return result;
			}
		}
		
		// the statement must have a constant potential, so the result is a uniform message.
		Expression result = Expressions.apply(FUNCTOR_ATOMIC_RULE, queryAtom, Expressions.makeSymbol(0.5));
		return result;		
	}
	
	//
	// START - SUPPORTING ROUTINES
	//
	
	/**
	 * Translates a potential expression to a rule expression.  Will replace any 
	 * instances of "query" in the expression with its equivalent.
	 * @param result       The potential expression to translate into a rule expression.
	 * @param queryAtom    The "query(...)" format.  If null, then this method will do the translation, but not substitution.
	 * @param queryString  The original form of the query.  What "query(...)" is equivalent to.
	 * @return             A rule expression with the instances of "query(...)" replaced.
	 */
	public Expression queryResultToRule (Expression result, Expression queryAtom, String queryString, RewritingProcess process) {
		Expression methodResult = queryResultToRule(result, queryAtom, ruleParser.parseFormula(queryString), process);
		return methodResult;
	}

	/**
	 * Translates a potential expression to a rule expression.  Will replace any 
	 * instances of "query" in the expression with its equivalent.
	 * @param result     The potential expression to translate into a rule expression.
	 * @param queryAtom  The "query(...)" format.  If null, then this method will do the translation, but not substitution.
	 * @param query      The original form of the query.  What "query(...)" is equivalent to.
	 * @return           A rule expression with the instances of "query(...)" replaced.
	 */
	public Expression queryResultToRule (Expression result, Expression queryAtom, Expression query, RewritingProcess process) {
		// Translate the result to a rule.
		Expression ruleExpression = queryAnswerPotentialExpression2Rule(result, queryAtom, process);

		// Perform the substitution of the query(...) with its equivalent.
		if (queryAtom != null && query != null) {
			List<Expression> queryAtomArgs = queryAtom.getArguments();
			Set<Expression> queryVariables = Expressions.freeVariables(query, process);
			if (queryVariables.containsAll(queryAtomArgs)) {
				ruleExpression = ruleExpression.replaceAllOccurrences(new ReplaceQueryFunction(queryAtom, query), process);
			}
			
		}
		return ruleExpression;
	}

	/**
	 * Generates a string in the rules format for the given expression.
	 * @param expression  A rules expression.
	 * @return            The string format for the rules expression.
	 */
	public String toRuleString (Expression expression) {
		StringBuffer sb = new StringBuffer();
		toRuleString(expression, sb, true);
		return sb.toString();
	}


	/**
	 * Creates a random variable declaration for the query atom.
	 * @param queryAtom            The query atom expression in the form of "query(...)"
	 * @param query                What the query atom is equivalent to.
	 * @param randomVariableDeclarations The random variable declarations.
	 * @return  A random variable declaration for the query.
	 */
	public Expression createQueryDeclaration (Expression queryAtom, Expression query, Set<Expression> randomVariableDeclarations, RewritingProcess process) {
		
		Expression result = createNewRandomVariableDeclaration(queryAtom, query, randomVariableDeclarations, process);

		return result;
	}
	
	public Expression createNewRandomVariableDeclaration(Expression randomVariableValue, Expression randomVariableUsedIn, Set<Expression> randomVariableDeclarations, RewritingProcess process) {
		Expression result = null;
		List<Expression> resultArguments = new ArrayList<Expression>();

		resultArguments.add(randomVariableValue.getFunctorOrSymbol());
		resultArguments.add(Expressions.makeSymbol(randomVariableValue.numberOfArguments()));
		
		List<Expression> rvValueArgs = randomVariableValue.getArguments();
		for (Expression rvArg : rvValueArgs) {
			// For each free variable in the query, search through the query's equivalent
			// for the same variable.  We need the name of the function the variable shows
			// up in and the position of the variable in the function (or if it's equal to the
			// return value of the function.)
			SearchFunctionArgumentFunction searchFunction = 
					new SearchFunctionArgumentFunction(rvArg, randomVariableDeclarations);
			randomVariableUsedIn.replaceFirstOccurrence(searchFunction, process);
			if (searchFunction.randomVariableName == null) {
				break;
			}
			for (Expression randomVariableDeclaration : randomVariableDeclarations) {
				// Once we know the function name and the argument position, we can look
				// up the declaration for that function and look up the type for the argument.
				if (randomVariableDeclaration.get(0).equals(searchFunction.randomVariableName)) {
					resultArguments.add(randomVariableDeclaration.get(searchFunction.argumentIndex + 2));
					break;
				}
			}
		}

		resultArguments.add(Expressions.makeSymbol(TYPE_BOOLEAN));
		if (resultArguments.size() == rvValueArgs.size() + 3) {
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION,  resultArguments);
		}
		
		return result;
	}

	/**
	 * Creates an instance of Model from the given components.
	 * @param name             The name of the model.
	 * @param description      Description of the model.
	 * @param sorts            The sorts in the model.
	 * @param randomVariableDeclarations  The random variable declarations in the model.
	 * @param parfactors       The parfactors in the model. 
	 * @return                 A Model representation of the given model components.
	 */
	public Model createModel (String name, String description, 
			Set<Expression> sorts, Set<Expression> randomVariableDeclarations,
			Set<Expression> parfactors) {
		ArrayList<Expression> modelArguments = new ArrayList<Expression>();
		modelArguments.add(Expressions.makeSymbol(name));
		modelArguments.add(Expressions.makeSymbol(description));
		for (Expression sort : sorts) {
			modelArguments.add(sort);
		}

		Set<String> randomVariableNameAndArities = new LinkedHashSet<String>();
		for (Expression randomVariableDeclaration : randomVariableDeclarations) {
			modelArguments.add(randomVariableDeclaration);
			Expression randomVariableName = randomVariableDeclaration.get(0);
			Expression randomVariableArity =
					randomVariableDeclaration.numberOfArguments() > 1 ?
							randomVariableDeclaration.get(1)
							: Expressions.ZERO;
			randomVariableNameAndArities.add(randomVariableName + "/" + randomVariableArity);
		}
		modelArguments.add(Expressions.apply(ParfactorsDeclaration.FUNCTOR_PARFACTORS_DECLARATION, parfactors));
		Expression modelExpression = Expressions.apply(Model.FUNCTOR_MODEL_DECLARATION, modelArguments);

		return new Model(modelExpression, randomVariableNameAndArities);
	}

	/**
	 * For the given function name and number of args, will create the additonal
	 * potential expressions to add for function transformation.
	 * 
	 * Description of function: 
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode:
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateFunctions
	 * 
	 * @param functionName
	 *            The name of the function.
	 * @param arity
	 *            The arity of the function.
	 * @param return The list of rules representing the functional constraint.
	 */
	public List<Expression> createTransformedFunctionConstraints(String functionName, int arity) {
		// |.... newRules <-
		List<Expression> rules = new ArrayList<Expression>();
		// |........ add if predicate(X1, ..., Xn, Y) then not predicate(X1, ..., Xn, Z)
		StringBuilder rule = new StringBuilder();
		int ii;
		rule.append("if ");
		rule.append(functionName);
		rule.append('(');
		for (ii = 0; ii < arity; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Y) then not ");
		rule.append(functionName);
		rule.append('(');
		for (ii = 0; ii < arity; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Z)");
		// |..................... and Y may be same as Xn and Z may be same as Xn // i.e. add a pair of conjuncts for each Xn
		for (ii = 0; ii < arity; ii++) {
			rule.append(" and Y may be same as X");
			rule.append(ii);
			rule.append(" and Z may be same as X");
			rule.append(ii);
		}

		rule.append(";");
		rules.add(ruleParser.parse(rule.toString()));

		// |........ add there exists Y : predicate(X1, ..., Xn, Y)		
		rule = new StringBuilder();
		rule.append("there exists Y : " + functionName + "(");
		for (ii = 0; ii < arity; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Y)");
		// |..................... and Y may be same as Xn // i.e. add a conjunct for each Xn
		for (ii = 0; ii < arity; ii++) {
			rule.append(" and Y may be same as X");
			rule.append(ii);
		}
		rule.append(";");
		rules.add(ruleParser.parse(rule.toString()));
		
		return rules;
	}

	/**
	 * Takes a high-level random variable declaration for a function and returns
	 * a low-level relational random variable declaration.
	 * 
	 * @param randomVariableDecl
	 *            The high-level random variable declaration.
	 * @return A low-level representation of the random variable declaration.
	 */
	public Expression updateRandomVariableDeclaration(Expression randomVariableDecl) {
		if (!randomVariableDecl.getFunctor().equals(RandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION)) {
			return null;
		}

		// Transfer all the args, but change the arg defining how many args the 
		// random variable has.
		List<Expression> oldArgs = randomVariableDecl.getArguments();
		List<Expression> newArguments = new ArrayList<Expression>(oldArgs.size()+1);
		for (int ii = 0; ii < oldArgs.size(); ii++) {
			// i.e. 1 == the arity slot
			if (ii == 1) {
				newArguments.add(Expressions.makeSymbol(oldArgs.get(1).intValue() + 1));
			}
			else {
				newArguments.add(oldArgs.get(ii));
			}
		}

		// Change the return type to boolean.
		newArguments.add(Expressions.makeSymbol(TYPE_BOOLEAN));
		return Expressions.apply(randomVariableDecl.getFunctor(), newArguments);
	}
	
	public boolean isPotentialExpression(Expression expression) {
		boolean result = Expressions.isNumber(expression) || FunctorConstants.ARITHMETIC_FUNCTORS.contains(expression.getFunctor());
		return result;
	}
	
//	public RewritingProcess getRewritingProcess() {
//		return rewritingProcess;
//	}

	/*===================================================================================
	 * PRIVATE METHODS
	 *=================================================================================*/
	private int determineIfRandomFunctionApplicationWithEmbeddedRandomVariableValue(Expression expression, Set<Expression> randomVariableDeclarations, RewritingProcess process) {
		int result = -1;
		// | .... if isRandomFunctionApplication(E) of the form predicate1(E1,..., Ei,..., En)
		if (isRandomFunctionApplication(expression)) {
			for (int i = 0; i < expression.numberOfArguments(); i++) {
				// | ............ where isRandomVariableValue(Ei, declarations) is true
				Expression expressionI = expression.get(i);
				if (isRandomVariableValue(expressionI, randomVariableDeclarations)) {
					result = i;
					break;
				}
			}
		}
		
		return result;
	}
	
	private void updateFunctionsIdentified(Map<String, Set<Integer>>   functionsIdentified, Expression functionApplication) {
		String functorName = functionApplication.getFunctorOrSymbol().toString();
		
		Set<Integer> paramCount;
		paramCount = functionsIdentified.get(functorName);
		if (paramCount == null) {
			paramCount = new LinkedHashSet<Integer>();
			functionsIdentified.put(functorName, paramCount);
		}
		paramCount.add(functionApplication.getArguments().size());
	}

	
	/**
	 * Add a potential expression/constraint pair to a list of potential expression/constraint pairs.  
	 * This method will simplify the potential expression and constraints and may eliminate the 
	 * potential expression if it reduces out.
	 * @param listOfConstrainedPotentialExpressions  The list of pairs to add the potential expression/constraint to.
	 * @param potentialExpression                    The potential expression to add.
	 * @param constraintC                            The original constraint condition for the expression.
	 * @param assumption                             An additional constraint to add to the list of constraints.
	 */
	private void addFurtherConstrainedPotentialExpression(
			List<Pair<Expression, Expression>> listOfConstrainedPotentialExpressions, 
			Expression potentialExpression, Expression constraintC, Expression assumption, RewritingProcess process) {
		// | ........ C' <- R_complete_normalize(C and Assumption)
		Expression constraintCAndAssumption = And.make(constraintC, assumption);
		Expression cPrime                   = process.rewrite(LBPRewriter.R_complete_normalize, constraintCAndAssumption);
		
		// | ........ if C' is not false
		if (!cPrime.equals(Expressions.FALSE)) {
			// | ............ P' <- R_complete_normalize(P) under C'
			RewritingProcess processUnderCPrime  = GrinderUtil.extendContextualConstraint(cPrime, process);
			Expression       pPrime              = processUnderCPrime.rewrite(LBPRewriter.R_complete_normalize, potentialExpression);			
			// | ............ if P' is not a numeric constant
			if (!Expressions.isNumber(pPrime)) {
				// | ................ setOfConstrainedPotentialExpressions <- add (P', C')
				listOfConstrainedPotentialExpressions.add(new Pair<Expression, Expression>(pPrime, cPrime));
			}
		}
	}

	/**
	 * Generates an expression representing one minus the value given.
	 * @param potential   The reference value.
	 * @return An expression representing 1 minus the given potential value.
	 */
	private Expression oneMinusPotential (Expression potential) {

		if (potential.getSyntacticFormType().equals("Symbol")) {
			try {
				NumberFormat format = NumberFormat.getNumberInstance();
				Number number = format.parse(potential.toString());
				return Expressions.makeSymbol(1 - number.doubleValue());
			}
			catch(ParseException e) {
				
			}
		}
		return Expressions.apply(FunctorConstants.MINUS, 1, potential);
	}

	/**
	 * Generates a string in the rules format for the given expression and
	 * appends it to the string buffer.
	 * Call this to generate a raw rule string without the semicolon.
	 * @param expression  The expression to generate a string for.
	 * @param sb          The string buffer to append the string to.
	 */
	private void toRuleString (Expression expression, StringBuffer sb) {
		toRuleString(expression, sb, false);
	}

	/**
	 * Generates a string in the rules format for the given expression and
	 * appends it to the string buffer.
	 * @param expression  The expression to generate a string for.
	 * @param sb          The string buffer to append the string to.
	 * @param isFirst     True if want a closing semicolon at the end of atomic and conditional rules.
	 */
	private void toRuleString (Expression expression, StringBuffer sb, boolean isFirst) {
		// If the expression is a symbol, just append the symbol name.
		if (expression.getSyntacticFormType().equals(SYNTACTIC_FORM_TYPE_SYMBOL)) {
			sb.append(expression.toString());
			return;
		}

		Expression functor = expression.getFunctor();
		String functorString = functor.getValue().toString();

		// Handle atomic rules
		if (functorString.equals(FUNCTOR_ATOMIC_RULE)) {
			toRuleString(expression.get(0), sb);
			Expression arg = expression.get(1);
			if (!arg.equals(1)) {
				sb.append(' ');
				toRuleString(expression.get(1), sb);
			}
			if (isFirst) {
				sb.append(';');
			}
			return;
		}

		// Handle conditional rules.
		if (functorString.equals(FUNCTOR_CONDITIONAL_RULE)) {
			List<Expression> args = expression.getArguments();
			sb.append("if ");
			toRuleString(args.get(0), sb);
			sb.append(" then ");
			toRuleString(args.get(1), sb);
			if (args.size() == 3) {
				sb.append(" else ");
				toRuleString(args.get(2), sb);
			}
			if (isFirst) {
				sb.append(';');
			}
			return;
		}

		// Handle prolog rules.
		if (functorString.equals(FUNCTOR_PROLOG_RULE)) {
			List<Expression> args = expression.getArguments();
			Expression arg = expression.get(0);
			if (!arg.equals(1)) {
				toRuleString(arg, sb);
				sb.append(' ');
			}
			toRuleString(args.get(1), sb);
			if (args.size() == 3) {
				sb.append(" :- ");
				toRuleString(args.get(2), sb);
			}
			sb.append('.');
			
			return;
		}

		// Handle standard probability rules.
		if (functorString.equals(FUNCTOR_STANDARD_PROB_RULE)) {
			List<Expression> args = expression.getArguments();
			sb.append("P(");
			toRuleString(args.get(0), sb);
			sb.append(" | ");
			toRuleString(args.get(1), sb);
			sb.append(") = ");
			toRuleString(args.get(2), sb);
			if (isFirst) {
				sb.append(';');
			}
			return;
		}

		// Handle causal rules.
		if (functorString.equals(FUNCTOR_CAUSAL_RULE)) {
			List<Expression> args = expression.getArguments();
			toRuleString(args.get(0), sb);
			sb.append(" -> ");
			toRuleString(args.get(1), sb);
			if (isFirst) {
				sb.append(';');
			}
			return;
		}


		// Handle minus expression.
		if (functorString.equals("minus") && expression.getArguments().size() == 2) {
			toRuleString(expression.get(0), sb);
			sb.append(" minus ");
			toRuleString(expression.get(1), sb);
			return;
		}

		sb.append(expression.toString());
	}

	private void checkLegalToken(String token) throws ReservedWordException {
		if (token.equals(FUNCTOR_QUERY) ||
			RuleTerminalSymbols.isTerminalSymbol(token) ||
			AntlrGrinderTerminalSymbols.isTerminalSymbol(token)) {
				throw new ReservedWordException("'" + token + "' is a reserved word in the rules language.");
		}
	}
	
	private String precisionedPotential(String precisionDigit, String finalDigit) {
		StringBuilder sb = new StringBuilder("0.");
		
		int numericPrecision = ExpressoConfiguration.getDisplayNumericPrecisionForSymbols();
		for (int i = 1; i < numericPrecision; i++) {
			sb.append(precisionDigit);
		}
		
		sb.append(finalDigit);
		
		return sb.toString();
	}
	
	private Set<Expression> collectUnknownRandomVariables(Expression expression, Set<Expression> randomVariableDeclarations, RewritingProcess process) {
		Map<Expression, Integer> knownRandomVariables = new LinkedHashMap<>();
		for (Expression randomVariableDeclaration : randomVariableDeclarations) {
			knownRandomVariables.put(randomVariableDeclaration.get(0), randomVariableDeclaration.get(1).intValue());
		}
		
		Set<Expression> unknownRandomVariables = new LinkedHashSet<>();
		
		collectUnknownRandomVariables(expression, knownRandomVariables, unknownRandomVariables, process);
		
		return unknownRandomVariables;
	}
	
	private void collectUnknownRandomVariables(Expression expression, Map<Expression, Integer> knownRandomVariables, Set<Expression> unknownRandomVariables, RewritingProcess process) {

		if (Expressions.isFunctionApplicationWithArguments(expression)) {
			if (isRandomFunctionApplication(expression)) {
				if (!(knownRandomVariables.containsKey(expression.getFunctorOrSymbol()) 
						&& 
				      knownRandomVariables.get(expression.getFunctorOrSymbol()) == expression.numberOfArguments())) {					
					unknownRandomVariables.add(expression);
				}
			}
			else {
				expression.getArguments().forEach(argument -> collectUnknownRandomVariables(argument, knownRandomVariables, unknownRandomVariables, process));
			}
		}
		else if (process.isUniquelyNamedConstant(expression) && !isKnownFunctorOrTerminalSymbol(expression)) {	
			if (!(knownRandomVariables.containsKey(expression.getFunctorOrSymbol()) 
					&& 
			      knownRandomVariables.get(expression.getFunctorOrSymbol()) == 0)) {	
				unknownRandomVariables.add(expression);
			}
		}
	}

	/*===================================================================================
	 * PRIVATE CLASSES
	 *=================================================================================*/	
	/**
	 * Replacement function for use by function translator.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateFunctions
	 * 
	 * @author etsai
	 *
	 */
	private class ReplaceFunctionWithRelation extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		private Set<Expression>             randomVariableDeclarations;
		private Map<String, Set<Integer>>   functionsIdentified;

		public ReplaceFunctionWithRelation(Set<Expression> randomVariableDeclarations, 
				Map<String, Set<Integer>> functionsIdentified) {
			this.randomVariableDeclarations = randomVariableDeclarations;
			this.functionsIdentified        = functionsIdentified;
		}

		// |........ lambda E
		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			Expression result = expression;
			
			// |............ if E is a function application with functor "=" or "!=" and arguments E1,...,Ek,
			// |.................... having an argument Ei such that isRandomVariableValue(Ei, declarations)
			int elementI = determineIfEqualityDisequalityWithEmbeddedRandomVariableValue(result, process);
			if (elementI != -1) {
				// |................ (predicate, (T1,...,Tn) ) <- functorAndArguments(Ei)
				Expression functionApplicationI = result.get(elementI);
				// |................ functions <- add (predicate, n) to functions
				updateFunctionsIdentified(functionsIdentified, functionApplicationI);
				// |................ j is some index distinct from i
				Expression       expressionJ  = null;
				List<Expression> equalityArgs = new ArrayList<Expression>();
				for (int i = 0; i < result.numberOfArguments(); i++) {
					if (i == elementI) {
						continue;
					}
					if (expressionJ == null) {
						expressionJ = result.get(i);
					}
					equalityArgs.add(result.get(i));
				}
				// |................ if functor "="
				if (Equality.isEquality(result)) {
					// |.................... return predicate(T1, ..., Tn, Ej) and =(G1,...,G{k-1})
					// |.............................. where G1,...G{k-1} is E1,...,Ek excluding Ei
					result = And.make(addArgToPredicate(functionApplicationI, expressionJ), Equality.make(equalityArgs.toArray()));
				}
				else { // |................ else // functor "!="
					// |.................... return not(predicate(T1, ..., Tn, Ej))
					result = Not.make(addArgToPredicate(functionApplicationI, expressionJ));
				}
			}

			return result;
		}
		
		//
		// PRIVATE
		//
		private int determineIfEqualityDisequalityWithEmbeddedRandomVariableValue(Expression expression, RewritingProcess process) {
			int result = -1;
			// if E is a function application with functor "=" or "!=" and arguments E1,...,Ek,
			if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
				for (int i = 0; i < expression.numberOfArguments(); i++) {
					// having an argument Ei such that isRandomVariableValue(Ei, declarations)
					Expression expressionI = expression.get(i);
					if (isRandomVariableValue(expressionI, randomVariableDeclarations)) {
						result = i;
						break;
					}
				}
			}
			return result;
		}
				
		private Expression addArgToPredicate(Expression functionApplication, Expression additionalArgument) {
			List<Expression> args = new ArrayList<Expression>();
			args.addAll(functionApplication.getArguments());
			args.add(additionalArgument);
			
			Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(functionApplication.getFunctorOrSymbol(), args.toArray());
			
			return result;
		}
	}
	
	/**
	 * Replacement function for use by function translator.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateFunctions
	 * 
	 * @author etsai
	 *
	 */
	private class ExtendPredicate1WithMayBeSameAs extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		private Expression       predicate1Functor    = null;
		private List<Expression> mayBeSameAsConjuncts = null;
		public ExtendPredicate1WithMayBeSameAs(Expression predicate1Functor, List<Expression> mayBeSameAsConjuncts) {
			this.predicate1Functor    = predicate1Functor;
			this.mayBeSameAsConjuncts = mayBeSameAsConjuncts;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			Expression result = expression;
			
			if (expression.getFunctor() != null && predicate1Functor.equals(expression.getFunctor())) {
				List<Expression> predicate1AndMayBeSameAsConjuncts = new ArrayList<Expression>();
				predicate1AndMayBeSameAsConjuncts.add(expression);
				predicate1AndMayBeSameAsConjuncts.addAll(mayBeSameAsConjuncts);
				result = And.make(predicate1AndMayBeSameAsConjuncts);
			}

			return result;
		}
	}

	/**
	 * Replacement function for use by quantifier translator.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateQuantifiers
	 * 
	 * @author etsai
	 *
	 */
	private class ReplaceQuantifierFunction extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		private List<Expression> expandingRules;
		private Set<Expression>  randomVariableDeclarations;

		public ReplaceQuantifierFunction(List<Expression> expandingRules, Set<Expression> randomVariableDeclarations) {
			this.expandingRules             = expandingRules;
			this.randomVariableDeclarations = randomVariableDeclarations;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			
			// TODO - remove this check once we add back in proper support for translating quantified rules into quantifier free rules
			if (ForAll.isForAll(expression) || ThereExists.isThereExists(expression)) {
				throw new UnsupportedOperationException("Translating quantified rules (i.e. "+expression+") to quantifier free rules is not properly supported.");
			}
			
			// | ............ if E is Quantifier X : Phi
			if (ForAll.isForAll(expression) || ThereExists.isThereExists(expression)) {
				// Create a new symbol based on the name of the quantifier expression.
				// This will be used as the name of a new random variable.
				// | ................ newSymbol <- string representation of E
				Expression newSymbol = Expressions.makeSymbol(expression.toString());

				// Get all the free variables in the quantifier expression to create a
				// call to our new random variable expression.
				// | ................ F <- array of free variables in E
				Set<Expression> freeVariablesF = Expressions.freeVariables(expression, process);
				Expression      newSymbolF     = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(newSymbol, freeVariablesF);

				// Then create a new rule based on the new expression.
				Expression newRule;
				// | ................ if Quantifier is "there exists"
				if (ThereExists.isThereExists(expression)) {						
					// | .................... expandingRules <- add "if Phi then newSymbol(F) else newSymbol(F) 0.000001"
					newRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_CONDITIONAL_RULE, 
							ThereExists.getBody(expression), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_ATOMIC_RULE, newSymbolF, 1),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_ATOMIC_RULE, newSymbolF, precisionedPotential("0", "1")));
				}
				else { // | ................ else // Quantifier is "for all"
					// | .................... expandingRules <- add "if not Phi then not newSymbol(F) else newSymbol(F) 0.999999"
					newRule = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_CONDITIONAL_RULE, 
							Not.make(ForAll.getBody(expression)), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_ATOMIC_RULE, Not.make(newSymbolF), 1),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(RuleConverter.FUNCTOR_ATOMIC_RULE, newSymbolF, precisionedPotential("9", "9")));						
				}
				expandingRules.add(newRule);

				// | ................ // create a corresponding random variable declaration
				// | ................ add randomVariable(newSymbol, size(F), TypeF1, ..., TypeFn, Boolean)
				// | ......................... to newRandomVariableDeclarations
				Expression newRandomVariableDeclaration = createNewRandomVariableDeclaration(newSymbolF, newRule, randomVariableDeclarations, process);
				if (newRandomVariableDeclaration != null) {
					randomVariableDeclarations.add(newRandomVariableDeclaration);
				}					

				// Replace the quantifier expression with the newSymbol(F) expression
				// | ................ return newSymbol( F )
				return newSymbolF;
			}
			return expression;
		}
	}

	/**
	 * Replacement function for use by ". may be same as ." extractor.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeDisembedConstraints
	 * 
	 * @author etsai
	 *
	 */
	private class ReplaceMayBeSameAsFunction extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		public Set<Pair<Expression, Expression>>  mayBeSameAsSet;

		public ReplaceMayBeSameAsFunction(Set<Pair<Expression, Expression>> mayBeSameAsSet) {
			this.mayBeSameAsSet = mayBeSameAsSet;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			if (expression.getFunctor() != null && expression.getArguments().size() == 2) {
				// Check for instances of ". may be same as .".				
				if (expression.getFunctor().getValue().equals(RuleConverter.FUNCTOR_MAY_BE_SAME_AS)) {
					// Mark which variables may be the same as each other.  We'll add
					// two versions of the pair, one with one variable in front and the other
					// with the other variable in front.  Later, when we're creating != constraints
					// for all the free variables, this will make checking whether to make 
					// the constraint easier.
					mayBeSameAsSet.add(
							new Pair<Expression, Expression>(
									expression.getArguments().get(0), expression.getArguments().get(1)));
					mayBeSameAsSet.add(
							new Pair<Expression, Expression>(
									expression.getArguments().get(1), expression.getArguments().get(0)));

					// Replace the ". may be same as ." expression with True.
					return Expressions.TRUE;
				}
			}
			return expression;
		}
	}
	
	/**
	 * Replacement function for use by the query result to rule translator.
	 * @author etsai
	 *
	 */
	private class ReplaceQueryFunction extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		private Expression queryAtom;
		private Expression query;

		public ReplaceQueryFunction(Expression queryAtom, Expression query) {
			this.queryAtom = queryAtom;
			this.query = query;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			// Look for instances of "query" in the query output and replace it with the equivalent
			// expression.
			if (expression.getFunctorOrSymbol().toString().equals(RuleConverter.FUNCTOR_QUERY)) {
				Expression result = query;
				
				List<Expression> queryAtomArgs = queryAtom.getArguments();
				List<Expression> expressionArgs = expression.getArguments();
				// Replace the variables in the replacement value with the values
				// used in the expression.
				// If the number of args don't match, something goofy is going on and bail.
				if (queryAtomArgs.size() == expressionArgs.size()) {
					// First check if any of the arguments are the same.
					boolean isDuplicate = false;
					for (Expression queryArg : queryAtomArgs) {
						for (Expression expressionArg : expressionArgs) {
							if (queryArg.equals(expressionArg)) {
								isDuplicate = true;
								break;
							}
						}
					}

					if (!isDuplicate) {
						// If there are no duplicates in the args, then we can do a simple replacement.
						for (int ii = 0; ii < queryAtomArgs.size(); ii++) {
							result = result.replaceAllOccurrences(
									queryAtomArgs.get(ii), expressionArgs.get(ii), process);
						}
					}
					else {
						// If there are duplicates, then we need to do the replacement in two
						// steps.  First, replace the args in the expression with a unique value, 
						// then replace the unique values with the final replacement values.
						List<Expression> uniques = new ArrayList<Expression>();
						int uniqueCount = 0;
						
						// First, replace the args in the query equivalent expression with a unique
						// variable.
						for (Expression queryAtomArg : queryAtomArgs) {
							Expression unique = Expressions.makeUniqueVariable("Unique" + uniqueCount, result, process);
							uniqueCount++;
							uniques.add(unique);
							result = result.replaceAllOccurrences(queryAtomArg, unique, process);
						}

						// Then replace the unique variables with the args from the query expression.
						for (int ii = 0; ii < queryAtomArgs.size(); ii++) {
							result = result.replaceAllOccurrences(uniques.get(ii), expressionArgs.get(ii), process);
						}
						
					}
				}
				return result;
			}
			return expression;
		}
	}

	/**
	 * Used for searching for the function argument with the given name.  This is
	 * used for figuring out the type of the arguments for the query expression.
	 * @author etsai
	 *
	 */
	private class SearchFunctionArgumentFunction extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		private Expression  searchTerm;
		private Set<Expression> randomVariableDeclarations;

		public Expression randomVariableName = null;
		public int        argumentIndex      = 0;

		public SearchFunctionArgumentFunction (Expression searchTerm, Set<Expression> randomVariableDeclarations) {
			this.searchTerm = searchTerm;
			this.randomVariableDeclarations = randomVariableDeclarations;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			if (isRandomFunctionApplication(expression)) {
				int index = 0;
				for (Expression arg : expression.getArguments()) {
					if (arg.equals(searchTerm)) {
						// If we found the argument name, then mark the random
						// variable that it was found in and the position of
						// the argument in the argument list.
						this.randomVariableName = expression.getFunctor();
						this.argumentIndex = index;
						return Expressions.TRUE;  // Mark that a match was made.
					}
					index++;
				}
			}
			else if (expression.hasFunctor(FunctorConstants.EQUAL) || 
					expression.hasFunctor(FunctorConstants.INEQUALITY)) {
				// Check for cases where the expression is "X = foo(...)".
				// In this case, the argument is equal to the return value
				// of foo.
				// First, we check the arguments of the =/!= to see if the
				// search term is there.
				List<Expression> args = expression.getArguments();
				for (int ii = 0; ii < args.size(); ii++) {
					Expression arg = args.get(ii);
					if (arg.equals(searchTerm)) {
						// If the search term was found, then check the other
						// arguments to see if one is a random variable value.
						// The return type of the random variable can then be
						// used to determine the type of the search term.
						for (int jj = 0; jj < args.size(); jj++) {
							if (jj == ii) {
								continue;
							}
							arg = args.get(jj);
							for (Expression randomVariableDeclaration : randomVariableDeclarations) {
								if (randomVariableDeclaration.get(0).equals(arg.getFunctorOrSymbol())) {
									this.randomVariableName = arg.getFunctorOrSymbol();
									// Assume that the argument position for the return type
									// in the random variable declarations come right after
									// the argument types.
									if (arg.getArguments() == null) {
										this.argumentIndex = 0;
									}
									else {
										this.argumentIndex = arg.getArguments().size();
									}
									return Expressions.TRUE;  // Mark that a match was made.
								}
							}
						}
					}
				}
			}
			return expression;
		}
	}
	
	private class CollectAtomicConstraint implements Predicate<Expression> {
		public Expression constraint = null;
		//
		private RewritingProcess process = null;
		
		public CollectAtomicConstraint(RewritingProcess process) {
			this.process = process;
		}
		
		@Override
		public boolean apply(Expression expression) {
			boolean result = false;
			
			// If we find an atomic constraint (a constraint involving only
			// equalities and disequalities), replace it with the given constant
			// value (in usage, the constant will be either True or False.)
			// Also, note the constraint so we can put it with other constraints
			// on the potential expression.
			if (LPIUtil.isConstraint(expression, process) &&
				(Equality.isEquality(expression) || Disequality.isDisequality(expression))) {
				constraint = expression;
				result = true;
			}
			
			return result;
		}
	}
}
