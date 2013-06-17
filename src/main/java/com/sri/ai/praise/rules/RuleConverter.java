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
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderTerminalSymbols;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.rules.antlr.RuleParserWrapper;
import com.sri.ai.praise.rules.antlr.RuleTerminalSymbols;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.ParfactorsDeclaration;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

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
	public static final String FUNCTOR_QUERY               = "query";

	public static final String TYPE_BOOLEAN               = "Boolean";

	public static final String SYNTACTIC_FORM_TYPE_SYMBOL = "Symbol";
	public static final String SYNTACTIC_FORM_TYPE_FUNCTION_APPLICATION = "Function application";

	private RuleParserWrapper  ruleParser        = null;
	private RewritingProcess   rewritingProcess  = null;

	public class LowLevelSyntax {
		private Set<Expression> sortDeclarations = new LinkedHashSet<Expression>();
		private Set<Expression> randomVariableDeclarations = new LinkedHashSet<Expression>();
		private Set<Expression> parfactors = new LinkedHashSet<Expression>();
		
		public LowLevelSyntax(Collection<Expression> sortDeclarations, Collection<Expression> randomVariableDeclarations, Collection<Expression> parfactors) {
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
		rewritingProcess = LBPFactory.newLBPProcess(Expressions.TRUE);
		CardinalityTypeOfLogicalVariable.registerDomainSizeOfLogicalVariableWithProcess(new CardinalityTypeOfLogicalVariable.DomainSizeOfLogicalVariable() {
			@Override
			public Integer size(Expression logicalVariable, RewritingProcess process) {
				return 1000;
			}
		}, rewritingProcess);
		ruleParser       = new RuleParserWrapper();
	}

	/*===================================================================================
	 * PUBLIC METHODS
	 *=================================================================================*/
	
	/**
	 * Converts the rules model and query and generates a low-level model object and query.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeQuery
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeIncrementalQuery
	 * 
	 * @param name         The name for the model.
	 * @param description  The description of the model.
	 * @param inputRules   The list of rule expressions of the model.
	 * @param query        The query expression for the model.
	 * @return  A pair consisting of the query expression (if inserted during translation) and a Model instance of the parsed model.
	 * @throws ReservedWordException 
	 */
	public Pair<Expression, Model> translateQuery(String name, String description, List<Expression> inputRules, Expression query) throws ReservedWordException {

		LowLevelSyntax lowLevelSyntax = translateToLowLevelSyntax(inputRules);
		
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
		
		// Run a conversion on the query before processing it with the other rules.
		Expression queryAtom = null;
		if (query != null) {
			queryAtom = query;
			
			Pair<Expression, Expression> queryPair = queryRuleAndAtom(query, lowLevelSyntax.getRandomVariableDeclarations());
			if (queryPair != null) {
				queryAtom = queryPair.first;
				Expression queryPotentialExpression = translateRule(queryPair.second);
				// Add random variable declaration for query(...).
				Expression queryDeclaration = createQueryDeclaration(
						queryAtom, queryPotentialExpression, lowLevelSyntax.getRandomVariableDeclarations());			
				if (queryDeclaration != null) {
					lowLevelSyntax.getRandomVariableDeclarations().add(queryDeclaration);
				}
				
				Set<Expression> queryParfactors = translateToParfactors(Arrays.asList(queryPotentialExpression), lowLevelSyntax.getRandomVariableDeclarations());				
				lowLevelSyntax.getParfactors().addAll(queryParfactors);
			}
		}
		
		// Create the model object output.
		return new Pair<Expression, Model>(queryAtom, createModel(name, description, lowLevelSyntax.getSortDeclarations(), lowLevelSyntax.getRandomVariableDeclarations(), lowLevelSyntax.getParfactors()));
	}
	
	//
	// Versions of translateQuery with default arguments.
	public Pair<Expression, Model> translateQuery(String name, String description, String modelString, String queryString) throws ReservedWordException {
		return translateQuery(name, description, ruleParser.parseAll(modelString), queryString != null ? ruleParser.parseFormula(queryString) : null);
	}
	
	/**
	 * Translates the high level rules into their low level syntax equivalents.
	 * 
	 * Description of function: http://code.google.com/p/aic-praise/wiki/
	 * TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslate
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeRuleList2PotentialExpressions
	 * 
	 * @param inputRules
	 *            the high level rules to be converted into low level syntax.
	 * @return an equivalent low level syntax representation of the input rules
	 *         given.
	 * 
	 * @throws ReservedWordException
	 */
	public LowLevelSyntax translateToLowLevelSyntax(List<Expression> inputRules) throws ReservedWordException {
		LowLevelSyntax result = null;
		
		List<Expression> potentialExpressions       = new ArrayList<Expression>();
		Set<Expression>  sortDeclarations           = new LinkedHashSet<Expression>();
		Set<Expression>  randomVariableDeclarations = new LinkedHashSet<Expression>();
		Set<String> sortNames                       = new HashSet<String>();

		// Convert the conjunctions of rules.
		List<Expression> inputRulesAndDeclarations = translateConjunctions(inputRules);

		// Sort the declarations and rules and convert the rules to their if-then-else forms.
		for (Expression ruleOrDeclaration : inputRulesAndDeclarations) {
			if (ruleOrDeclaration.getFunctor().equals(RandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION)) {
				randomVariableDeclarations.add(ruleOrDeclaration);
			}
			else if (ruleOrDeclaration.getFunctor().equals(SortDeclaration.FUNCTOR_SORT_DECLARATION)) {
				sortDeclarations.add(ruleOrDeclaration);
				sortNames.add(ruleOrDeclaration.get(0).toString());
			}
			else {
				// Is not a declaration, therefore treat as a rule
				potentialExpressions.add(translateRule(ruleOrDeclaration));
			}
		}
		
		// Look for missing sort declarations in the random variable declarations.
		Set<String> missingSorts = new HashSet<String>();
		for (Expression randomVariableDeclaration : randomVariableDeclarations) {
			List<Expression> args = randomVariableDeclaration.getArguments();
			for (int ii = 2; ii < args.size() - 1; ii++) {
				String argName = args.get(ii).toString();
				if (!sortNames.contains(argName)) {
					missingSorts.add(argName);
				}
			}
		}

		// Add declarations for the missing sorts.
		for (String missingSort : missingSorts) {
			sortDeclarations.add(Expressions.make(SortDeclaration.FUNCTOR_SORT_DECLARATION, missingSort, SortDeclaration.UNKNOWN_SIZE, 
					ExtensionalSet.makeEmptySetExpression()));
			sortNames.add(missingSort);
		}
		
		Set<Expression> parfactors = translateToParfactors(potentialExpressions, randomVariableDeclarations);
		
		result = new LowLevelSyntax(sortDeclarations, randomVariableDeclarations, parfactors);
		
		return result;
	}
	
	/**
	 * Convert the given rule into its "if . then . else ." form.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeRule2PotentialExpression
	 * 
	 * @param rule  The rule to translate.
	 * @return  The equivalent "if . then . else ." form of the rule.
	 */
	public Expression translateRule (Expression rule) {
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
		List<Expression> args = rule.getArguments();
		if (args.size() != 2) {
			return null;
		}

		return new DefaultCompoundSyntaxTree(IfThenElse.FUNCTOR, args.get(0), 
				args.get(1), oneMinusPotential(args.get(1)));
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
		if (args.size() == 2) {
			return new DefaultCompoundSyntaxTree(IfThenElse.FUNCTOR, args.get(0), 
					this.translateRule(args.get(1)),
					0.5);
		}
		else if (args.size() == 3) {
			return new DefaultCompoundSyntaxTree(IfThenElse.FUNCTOR, args.get(0), 
					this.translateRule(args.get(1)),
					this.translateRule(args.get(2)));
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
		
		if (args.size() == 2) {
			return new DefaultCompoundSyntaxTree(IfThenElse.FUNCTOR, args.get(1), 
					args.get(0), oneMinusPotential(args.get(0)));
		}
		else if (args.size() == 3){
			return new DefaultCompoundSyntaxTree(IfThenElse.FUNCTOR, args.get(2), 
					new DefaultCompoundSyntaxTree(IfThenElse.FUNCTOR, args.get(1), 
							args.get(0), oneMinusPotential(args.get(0))),
					0.5);
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
					rule1 = DefaultSymbol.createSymbol(0.5);
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
						rule2 = DefaultSymbol.createSymbol(0.5);
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
						result.add(Expressions.make(rule.getFunctor(), formula, arg1));
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
							result.add(Expressions.make(rule.getFunctor(), formula, arg1, arg2List.get(0)));
						}
					}
					else {
						// If argument two has conjunctions in it, create new rules for those.  This also
						// handles cases where neither argument one or two had conjunctions.
						for (Expression arg2 : arg2List) {
							result.add(Expressions.make(rule.getFunctor(), formula, arg1List.get(0), arg2));
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
						result.add(Expressions.make(rule.getFunctor(), formula, translatedElement));
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
			return this.translateConditionalRule(Expressions.make(FUNCTOR_CONDITIONAL_RULE, args.get(1), 
					Expressions.make(FUNCTOR_ATOMIC_RULE, args.get(0), args.get(2))));
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
			return this.translateConditionalRule(Expressions.make(FUNCTOR_CONDITIONAL_RULE, args.get(0), 
					args.get(1)));
		}
		return null;
	}
	
	/**
	 * Convert a list of potential expressions into parfactors.
	 * 
	 * @param potentialExpressions
	 *            the potential expressions to be converted to parfactors.
	 * @param randomVariableDeclarations
	 *            random variable declarations associated with the parfactors.
	 * @return a set of parfactors generated from the give potential
	 *         expressions.
	 */
	public Set<Expression> translateToParfactors(List<Expression> potentialExpressions, Set<Expression> randomVariableDeclarations) {		
		// Translate the functions.
		potentialExpressions = translateFunctions(potentialExpressions, randomVariableDeclarations);

		// Translate the quantifiers.
		potentialExpressions = translateQuantifiers(potentialExpressions, randomVariableDeclarations);
		
		// Note: This is required to ensure random variable information is available on
		// the rewriting process when performing R_simplify and R_complete_simplify operations.
		Model.setKnownRandomVariables(randomVariableDeclarations, rewritingProcess);

		// Extract the embedded constraints.
		List<Pair<Expression, Expression>> potentialExpressionAndConstraintList = 
				disembedConstraints(potentialExpressions);

		// Translate the potential expression/constraint pair into a parfactor.
		Set<Expression> parfactors = new LinkedHashSet<Expression>();
		for (Pair<Expression, Expression> pair : potentialExpressionAndConstraintList) {
			parfactors.add(createParfactor(pair.first, pair.second));
		}
		
		return parfactors;
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
	 * @param potentialExpressions  The potential expressions perform the function transformation upon.
	 * @param randomVariableDeclarations A set of known random variable declarations.
	 * @return  A list of potential expressions with the function references transformed.
	 */
	public List<Expression> translateFunctions (List<Expression> potentialExpressions, 
			Set<Expression> randomVariableDeclarations) {
		List<Expression> result = new ArrayList<Expression>();
		Map<String, Set<Integer>> functionsFound = new HashMap<String, Set<Integer>>();

		// For each potential expression, translate the functions.
		for (Expression potentialExpression : potentialExpressions) {			
			Expression toReplace = potentialExpression;
			Expression replaced  = potentialExpression;

			// Replace the functions in the potential expression until there are no more.
			ReplaceFunctionWithRelation replacementFunction = 
					new ReplaceFunctionWithRelation(toReplace, randomVariableDeclarations, functionsFound);
			do {
			    toReplace = replaced;
			    replaced = toReplace.replaceAllOccurrences(
			    		replacementFunction,
			    		rewritingProcess);
			           
			    // Update the reference for generating unique constant names.
			    replacementFunction.currentExpression = replaced;
			} while (replaced != toReplace);

			// Save the result.
	        result.add(replaced);
		}

		// For each of the functions found, create the additional rules defining the 
		// functional relationship.
		for (String functor : functionsFound.keySet()) {
			Set<Integer> counts = functionsFound.get(functor);
			for (Integer count : counts) {
				createTransformedFunctionConstraints(functor, count, result);
				
				// if there is a random variable declaration "randomVariable(predicate, n, Type_1, ..., Type_n, return_type)"
				Expression oldRandomVariableDeclaration = null;
				Expression newRandomVariableDeclaration = null;
				for (Expression randomVariableDeclaration : randomVariableDeclarations) {
					if (randomVariableDeclaration.get(0).equals(functor) && randomVariableDeclaration.get(1).intValue() == count) {
						oldRandomVariableDeclaration = randomVariableDeclaration;
						newRandomVariableDeclaration = updateRandomVariableDeclaration(oldRandomVariableDeclaration);
						break;
					}
				}
				
				// add "randomVariable(predicate, n + 1, Type1, ..., Type_n, return_type, Boolean)" to newDeclarations
				// Here we will just replace.
				if (oldRandomVariableDeclaration != null) {
					randomVariableDeclarations.remove(oldRandomVariableDeclaration);
					randomVariableDeclarations.add(newRandomVariableDeclaration);
				}
			}
		}

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
		String functor = e.getFunctor().getValue().toString();	
	
		if (FunctorConstants.BOOLEAN_FUNCTORS.contains(functor) ||
			functor.equals(FunctorConstants.EQUAL) ||
			functor.equals(FunctorConstants.INEQUALITY) ||
			FunctorConstants.ARITHMETIC_FUNCTORS.contains(functor) ||
			functor.equals(IfThenElse.FUNCTOR) ||
			functor.equals(FUNCTOR_MAY_BE_SAME_AS) ||
			functor.equals(FUNCTOR_ATOMIC_RULE) ||
			functor.equals(FUNCTOR_CONDITIONAL_RULE) ||
			functor.equals(FUNCTOR_PROLOG_RULE) ||
			functor.equals(FUNCTOR_STANDARD_PROB_RULE) ||
			functor.equals(FUNCTOR_CAUSAL_RULE) ||
			functor.equals(FUNCTOR_QUERY) ||
			RuleTerminalSymbols.isTerminalSymbol(functor) ||
			AntlrGrinderTerminalSymbols.isTerminalSymbol(functor)) {
			return false;
		}

		// Else, return true.
		return true;
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
	public boolean isRandomVariableValue (Expression e, Set<Expression> randomVariableDeclarations) {
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
	 * Removes the quantifiers from potential expressions.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateQuantifiers
	 * 
	 * @param potentialExpressions  The potential expressions to check for quantifiers.
	 * @param randomVariableDeclarations A set of random variable declarations.
	 * @return  Quantifier free versions of the expressions.
	 */
	public List<Expression> translateQuantifiers (List<Expression> potentialExpressions, Set<Expression> randomVariableDeclarations) {
		List<Expression> newPotentialExpressions = new ArrayList<Expression>();
		// Create a local copy as will be extending the collection during
		// the following replacement logic in order to support nested
		// quantifiers.
		List<Expression> expandingPotentialExpressions = new ArrayList<Expression>(potentialExpressions);
		
		// Translate the quantifiers in each potential expression.
		// for each potentialExpression in potentialExpressions
		for (int i = 0; i < expandingPotentialExpressions.size(); i++) {
			Expression newPotentialExpression = expandingPotentialExpressions.get(i);

			// newPotentialExpression <- exhaustively (*) replace each expression E in potentialExpression
			// with the following replacement function:
			ReplaceQuantifierFunction replacementFunction = new ReplaceQuantifierFunction(expandingPotentialExpressions, randomVariableDeclarations);
			Expression toReplace;
			do {
				// Replace the quantifiers until there are none left in the potential expression.
			    toReplace = newPotentialExpression;
			    newPotentialExpression = toReplace.replaceAllOccurrences(
			    		replacementFunction,
			    		rewritingProcess);
			} while (newPotentialExpression != toReplace);

			// newPotentialExpressions <- add newPotentialExpression
			newPotentialExpressions.add(newPotentialExpression);
		}
		
		return newPotentialExpressions;
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
	public List<Pair<Expression, Expression>> disembedConstraints (List<Expression> potentialExpressions) {
		List<Pair<Expression, Expression>> setOfConstrainedPotentialExpressions = 
				new ArrayList<Pair<Expression, Expression>>();
		
		for (Expression potentialExpression : potentialExpressions) {
			Set<Pair<Expression, Expression>> mayBeSameAsSet = new HashSet<Pair<Expression, Expression>>();

			// Gather instances of ". may be same as .".
			Expression toReplace = potentialExpression;
			Expression replaced  = toReplace;

			ReplaceMayBeSameAsFunction replacementFunction = new ReplaceMayBeSameAsFunction(mayBeSameAsSet);
			do {
			    toReplace = replaced;
			    replaced = toReplace.replaceAllOccurrences(
			    		replacementFunction,
			    		rewritingProcess);
			} while (replaced != toReplace);

			potentialExpression = replaced;

			// Simplify the updated expression.
			potentialExpression = rewritingProcess.rewrite(LBPRewriter.R_simplify, potentialExpression);

			// Get free variables and create inequality constraints on all pairs except those
			// pairs stated to be ". may be same as .".
			List<Expression> constraints = new ArrayList<Expression>();
			Set<Expression> variables = Expressions.freeVariables(potentialExpression, rewritingProcess);
			Expression[] variableArray = new Expression[variables.size()];
			variables.toArray(variableArray);
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

			setOfConstrainedPotentialExpressions.add(
					new Pair<Expression, Expression>(potentialExpression, And.make(constraints)));
		}

		// Extract the embedded constraints from the potential expressions.
		List<Pair<Expression, Expression>> result = new ArrayList<Pair<Expression, Expression>>();
		for (int ii = 0; ii < setOfConstrainedPotentialExpressions.size(); ii++) {

			// Check if the potential expression has any more embedded constraints.
			Pair<Expression, Expression> pair = setOfConstrainedPotentialExpressions.get(ii);
			Expression potentialExpression = pair.first;
			Expression constraintC         = pair.second;
			CollectAtomicConstraint collectAtomicConstraint = new CollectAtomicConstraint(rewritingProcess);
			if (Util.thereExists(new SubExpressionsDepthFirstIterator(potentialExpression), collectAtomicConstraint)) {
				// for Assumption in (Constraint, not Constraint)
				Expression assumption = collectAtomicConstraint.constraint;
				addFurtherConstrainedPotentialExpression(setOfConstrainedPotentialExpressions, potentialExpression, constraintC, assumption);
				Expression notAssumption = Not.make(collectAtomicConstraint.constraint);
				addFurtherConstrainedPotentialExpression(setOfConstrainedPotentialExpressions, potentialExpression, constraintC, notAssumption);
			}
			else {
				result.add(pair);
			}
		}
		
		return result;		
	}

	/**
	 * Makes a parfactor from the given potential expression and a list of constraints.
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
	public Expression createParfactor(Expression potentialExpression, Expression constraintC) {
		// parfactors <- add R_simplify({{ (on <free variables in P and C>) [ P ] | C }})
		Set<Expression> freeVariablesInPandC = new LinkedHashSet<Expression>();
		freeVariablesInPandC.addAll(Expressions.freeVariables(potentialExpression, rewritingProcess));
		freeVariablesInPandC.addAll(Expressions.freeVariables(constraintC, rewritingProcess));

		Expression result = IntensionalSet.makeMultiSetFromIndexExpressionsList(
				 			new ArrayList<Expression>(freeVariablesInPandC), 
				 			Expressions.make(FunctorConstants.LEFT_DOT_RIGHT, potentialExpression), 
				 			constraintC);
	
		result = rewritingProcess.rewrite(LBPRewriter.R_simplify, result);
	
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
	 *          The current set of random variable declarations.
	 * @return A pair with the query atom and query rule.
	 */
	public Pair<Expression, Expression> queryRuleAndAtom (Expression query, Set<Expression> randomVariableDeclarations) {
		Pair<Expression, Expression> result = null;
		boolean createQueryTerm = true;
		for (Expression randomVariableDeclaration : randomVariableDeclarations) {
			if (randomVariableDeclaration.get(0).equals(query.getFunctorOrSymbol()) && randomVariableDeclaration.get(1).intValue() == query.numberOfArguments()) {
				createQueryTerm = false;
				break;
			}
		}

		if (createQueryTerm) {
			Set<Expression> variables = Expressions.freeVariables(query, rewritingProcess);
			Expression queryAtom;
			if (variables.size() > 0) {
				queryAtom = Expressions.make(FUNCTOR_QUERY, variables);
			}
			else {
				queryAtom = DefaultSymbol.createSymbol(FUNCTOR_QUERY);
			}
			Expression queryRule = Expressions.make(FUNCTOR_ATOMIC_RULE, Expressions.make(Equivalence.FUNCTOR, queryAtom, query), 1);		
			result = new Pair<Expression, Expression>(queryAtom, queryRule);
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
	 * @return           A rule expression version of the potential expression.
	 */
	public Expression potentialExpressionToRule(Expression input) {
		boolean isIfThenElse = IfThenElse.isIfThenElse(input);
		
		//we can only really simplify if then else expressions
		if (isIfThenElse) {
			Expression condition = IfThenElse.getCondition(input);
			boolean isConstraint = LPIUtil.isConstraint(condition, rewritingProcess);
			if (isConstraint) {
				Expression translationOfE1 = potentialExpressionToRule(input.get(1));
				Expression translationOfE2 = potentialExpressionToRule(input.get(2));
				
				//if both clauses are true, result is true
				if (translationOfE1.equals(Expressions.TRUE) && translationOfE2.equals(Expressions.TRUE)) {
					return Expressions.TRUE;
				} 
				//if the then clause is true, return the else clause
				else if (translationOfE1.equals(Expressions.TRUE)) {
					return new DefaultCompoundSyntaxTree(FUNCTOR_CONDITIONAL_RULE,
							Not.make(condition),
							translationOfE2);
				} 
				//if the else clause is true, return the if clause
				else if (translationOfE2.equals(Expressions.TRUE)) {
					return new DefaultCompoundSyntaxTree(FUNCTOR_CONDITIONAL_RULE,
							condition,
							translationOfE1);
				}
				//if neither is true, then return the simplified form
				else {
					return new DefaultCompoundSyntaxTree(FUNCTOR_CONDITIONAL_RULE, 
							condition, 
							translationOfE1, 
							translationOfE2);
				}
			}
			else {
				//assume that the 'condition' is a random variable value
				return Expressions.apply(FUNCTOR_ATOMIC_RULE, condition, input.get(1));
			}
		}
		
		//the statement must have a constant potential, so it adds nothing
		//of value.  We simply return true here
		return Expressions.TRUE;
		
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
	public Expression queryResultToRule (Expression result, Expression queryAtom, 
			String queryString) {
		return queryResultToRule(result, queryAtom, ruleParser.parseFormula(queryString));
	}

	/**
	 * Translates a potential expression to a rule expression.  Will replace any 
	 * instances of "query" in the expression with its equivalent.
	 * @param result     The potential expression to translate into a rule expression.
	 * @param queryAtom  The "query(...)" format.  If null, then this method will do the translation, but not substitution.
	 * @param query      The original form of the query.  What "query(...)" is equivalent to.
	 * @return           A rule expression with the instances of "query(...)" replaced.
	 */
	public Expression queryResultToRule (Expression result, Expression queryAtom, Expression query) {
		// Translate the result to a rule.
		Expression ruleExpression = potentialExpressionToRule(result);

		// Perform the substitution of the query(...) with its equivalent.
		if (queryAtom != null && query != null) {
			List<Expression> queryAtomArgs = queryAtom.getArguments();
			Set<Expression> queryVariables = Expressions.freeVariables(query, rewritingProcess);
			if (queryVariables.containsAll(queryAtomArgs)) {
				ruleExpression = ruleExpression.replaceAllOccurrences(new ReplaceQueryFunction(queryAtom, query), rewritingProcess);
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
	public Expression createQueryDeclaration (Expression queryAtom, Expression query, 
			Set<Expression> randomVariableDeclarations) {
		
		Expression result = createNewRandomVariableDeclaration(queryAtom, query, randomVariableDeclarations);

		return result;
	}
	
	public Expression createNewRandomVariableDeclaration(Expression randomVariableValue, Expression randomVariableUsedIn, Set<Expression> randomVariableDeclarations) {
		Expression result = null;
		List<Expression> resultArgs = new ArrayList<Expression>();

		resultArgs.add(randomVariableValue.getFunctorOrSymbol());
		resultArgs.add(DefaultSymbol.createSymbol(randomVariableValue.numberOfArguments()));
		
		List<Expression> rvValueArgs = randomVariableValue.getArguments();
		for (Expression rvArg : rvValueArgs) {
			// For each free variable in the query, search through the query's equivalent
			// for the same variable.  We need the name of the function the variable shows
			// up in and the position of the variable in the function (or if it's equal to the
			// return value of the function.)
			SearchFunctionArgumentFunction searchFunction = 
					new SearchFunctionArgumentFunction(rvArg, randomVariableDeclarations);
			randomVariableUsedIn.replaceFirstOccurrence(searchFunction, rewritingProcess);
			if (searchFunction.randomVariableName == null) {
				break;
			}
			for (Expression randomVariableDeclaration : randomVariableDeclarations) {
				// Once we know the function name and the argument position, we can look
				// up the declaration for that function and look up the type for the argument.
				if (randomVariableDeclaration.get(0).equals(searchFunction.randomVariableName)) {
					resultArgs.add(randomVariableDeclaration.get(searchFunction.argumentIndex + 2));
					break;
				}
			}
		}

		resultArgs.add(DefaultSymbol.createSymbol(TYPE_BOOLEAN));
		if (resultArgs.size() == rvValueArgs.size() + 3) {
			result = Expressions.make(RandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION,  resultArgs);
		}
		
		return result;
	}

	/**
	 * Creates an instance of Model from the given components.
	 * @param name             The name of the model.
	 * @param description      Description of the model.
	 * @param sorts            The sorts in the model.
	 * @param randomVariables  The random variable declarations in the model.
	 * @param parfactors       The parfactors in the model. 
	 * @return                 A Model representation of the given model components.
	 */
	public Model createModel (String name, String description, 
			Set<Expression> sorts, Set<Expression> randomVariables,
			Set<Expression> parfactors) {
		ArrayList<Expression> modelArgs = new ArrayList<Expression>();
		modelArgs.add(DefaultSymbol.createSymbol(name));
		modelArgs.add(DefaultSymbol.createSymbol(description));
		for (Expression sort : sorts)
			modelArgs.add(sort);

		Set<String> randomVariableNames = new HashSet<String>();
		for (Expression randomVariable : randomVariables) {
			modelArgs.add(randomVariable);
			randomVariableNames.add(randomVariable.get(0).toString());
		}
		modelArgs.add(Expressions.apply(ParfactorsDeclaration.FUNCTOR_PARFACTORS_DECLARATION, parfactors));
		Expression modelExpression = Expressions.apply(Model.FUNCTOR_MODEL_DECLARATION, modelArgs);

		return new Model(modelExpression, randomVariableNames);
	}

	/**
	 * For the given function name and number of args, will create the additonal potential
	 * expressions to add for function transformation.
	 * 
	 * Description of function:
	 * http://code.google.com/p/aic-praise/wiki/TranslatingFromHighToLowLevelModelSyntax
	 * 
	 * Pseudocode: 
	 * http://code.google.com/p/aic-praise/wiki/PseudoCodeTranslateFunctions
	 * 
	 * @param functionName  The name of the function.
	 * @param numArgs       The number of arguments of the function.
	 * @param potentialExpressions  The list to add the new potential expressions to.
	 */
	public void createTransformedFunctionConstraints (String functionName, int numArgs, List<Expression> potentialExpressions) {
		StringBuilder rule = new StringBuilder();
		int ii;
		rule.append("if ");
		rule.append(functionName);
		rule.append('(');
		for (ii = 0; ii < numArgs; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Y) then not ");
		rule.append(functionName);
		rule.append('(');
		for (ii = 0; ii < numArgs; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Z);");

		potentialExpressions.add(translateConditionalRule(ruleParser.parse(rule.toString())));

		rule = new StringBuilder();
		rule.append("there exists Y : " + functionName + "(");
		for (ii = 0; ii < numArgs; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Y);");

		potentialExpressions.add(translateAtomicRule(ruleParser.parse(rule.toString())));
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
		List<Expression> newArgs = new ArrayList<Expression>(oldArgs.size()+1);
		for (int ii = 0; ii < oldArgs.size(); ii++) {
			// i.e. 1 == the arity slot
			if (ii == 1) {
				newArgs.add(DefaultSymbol.createSymbol(oldArgs.get(1).intValue() + 1));
			}
			else {
				newArgs.add(oldArgs.get(ii));
			}
		}

		// Change the return type to boolean.
		newArgs.add(DefaultSymbol.createSymbol(TYPE_BOOLEAN));
		return new DefaultCompoundSyntaxTree(randomVariableDecl.getFunctor(), newArgs);
	}
	
	public RewritingProcess getRewritingProcess() {
		return rewritingProcess;
	}

	/*===================================================================================
	 * PRIVATE METHODS
	 *=================================================================================*/
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
			Expression potentialExpression, Expression constraintC, Expression assumption) {
		// C' <- R_complete_simplify(C and Assumption)
		Expression cPrime;
		Expression constraintCAndAssumption = And.make(constraintC, assumption);
		cPrime = rewritingProcess.rewrite(LBPRewriter.R_complete_simplify, constraintCAndAssumption);
		
		// if C' is not false
		if (!cPrime.equals(Expressions.FALSE)) {
			// P' <- R_complete_simplify(P) under C'
			RewritingProcess processUnderAssumption  = GrinderUtil.extendContextualConstraint(cPrime, rewritingProcess);
			Expression pPrime = processUnderAssumption.rewrite(LBPRewriter.R_complete_simplify, potentialExpression);			
			// if P' is not a numeric constant
			if (!Expressions.isNumber(pPrime)) {
				// setOfConstrainedPotentialExpressions <- add (P', C')
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

		if (potential instanceof DefaultSymbol) {
			try {
				NumberFormat format = NumberFormat.getNumberInstance();
				Number number = format.parse(potential.toString());
				return DefaultSymbol.createSymbol(1 - number.doubleValue());
			}
			catch(ParseException e) {
				
			}
		}
		return new DefaultCompoundSyntaxTree(FunctorConstants.MINUS, 1, potential);
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
		String functorString = ((DefaultSymbol)functor).getValue().toString();

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
			if (isFirst) {
				sb.append(" ;");
			}
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
		private Set<Expression>           randomVariableDeclarations;
		private Map<String, Set<Integer>> functionsFound;
		private Expression                currentExpression;
		private int                       uniqueCount = 0;

		public ReplaceFunctionWithRelation(Expression currentExpression,
				Set<Expression> randomVariableDeclarations, 
				Map<String, Set<Integer>> functionsFound) {
			this.currentExpression = currentExpression;
			this.randomVariableDeclarations = randomVariableDeclarations;
			this.functionsFound = functionsFound;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			Expression result = expression;
			
			// if E is a function application with functor "=" or "!=" and arguments E1,...,Ek,
			// having an argument Ei such that isRandomVariableValue(Ei, declarations)
			int elementI = determineIfEqualityDisequalityWithEmbeddedRandomVariableValue(result, process);
			if (elementI != -1) {
				// (predicate, (T1,...,Tn) ) <- functorAndArguments(Ei)
				// functions <- add (predicate, n) to functions
				Expression functionApplicationI = result.get(elementI);
				updateFunctionsFound(functionApplicationI);
				// j is some index distinct from i
				Expression expressionJ = null;
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
				// if functor "="
				if (Equality.isEquality(result)) {
					// return predicate(T1, ..., Tn, Ej) and =(G1,...,G{k-1})
					// where G1,...G{k-1} is E1,...,Ek excluding Ei
					result = And.make(addArgToPredicate(functionApplicationI, expressionJ), Equality.make(equalityArgs.toArray()));
				}
				else { // else functor "!="
					// return not(predicate(T1, ..., Tn, Ej))
					result = Not.make(addArgToPredicate(functionApplicationI, expressionJ));
				}
			}

			// if isRandomFunctionApplication(E) of the form predicate1(E1,..., i,..., En)
			// where isRandomVariableValue(Ei, declarations) is true
			elementI = determineIfRandomFunctionApplicationWithEmbeddedRandomVariableValue(result, process);
			if (elementI != -1) {
				// (predicate2, (T1,...,Tk) ) <- functorAndArguments(Ei)
				// functions <- add (predicate2, n) to functions
				Expression functionApplicationI = result.get(elementI);
				updateFunctionsFound(functionApplicationI);
				
				// return predicate1(E1, ..., Ei-1, NewUniqueVariable, E{i+1},..., En) 
				// ...... and 
				// ...... predicate2(T1, ..., Tk, NewUniqueVariable)
				Expression newUniqueVariable = Expressions.makeUniqueVariable("X" + (uniqueCount++), currentExpression, rewritingProcess);
				List<Expression> predicate1Args = new ArrayList<Expression>(result.getArguments());
				predicate1Args.set(elementI, newUniqueVariable);
				Expression predicate1 = Expressions.make(result.getFunctor(), predicate1Args.toArray());
				
				List<Expression> predicate2Args = new ArrayList<Expression>(functionApplicationI.getArguments());
				predicate2Args.add(newUniqueVariable);
				Expression predicate2 = Expressions.make(functionApplicationI.getFunctor(), predicate2Args.toArray());
				
				result = And.make(predicate1, predicate2);
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
		
		private int determineIfRandomFunctionApplicationWithEmbeddedRandomVariableValue(Expression expression, RewritingProcess process) {
			int result = -1;
			// if isRandomFunctionApplication(E) of the form predicate1(E1,..., i,..., En)
			if (isRandomFunctionApplication(expression)) {
				for (int i = 0; i < expression.numberOfArguments(); i++) {
					// where isRandomVariableValue(Ei, declarations) is true
					Expression expressionI = expression.get(i);
					if (isRandomVariableValue(expressionI, randomVariableDeclarations)) {
						result = i;
						break;
					}
				}
			}
			
			return result;
		}
		
		private void updateFunctionsFound(Expression functionApplication) {
			String functorName = functionApplication.getFunctorOrSymbol().toString();
			
			Set<Integer> paramCount;
			paramCount = functionsFound.get(functorName);
			if (paramCount == null) {
				paramCount = new LinkedHashSet<Integer>();
				functionsFound.put(functorName, paramCount);
			}
			paramCount.add(functionApplication.getArguments().size());
		}
		
		private Expression addArgToPredicate(Expression functionApplication, Expression additionalArgument) {
			List<Expression> args = new ArrayList<Expression>();
			args.addAll(functionApplication.getArguments());
			args.add(additionalArgument);
			
			Expression result = Expressions.make(functionApplication.getFunctorOrSymbol(), args.toArray());
			
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
		private List<Expression> expandingPotentialExpressions;
		private Set<Expression> randomVariableDeclarations;

		public ReplaceQuantifierFunction(List<Expression> expandingPotentialExpressions, Set<Expression> randomVariableDeclarations) {
			this.expandingPotentialExpressions = expandingPotentialExpressions;
			this.randomVariableDeclarations = randomVariableDeclarations;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			if (expression.getArguments().size() > 0) {
				// Find all instances of the quantifiers: "for all" and "there exists".
				if (ForAll.isForAll(expression) || ThereExists.isThereExists(expression)) {
					// Create a new symbol based on the name of the quantifier expression.
					// This will be used as the name of a new random variable.
					// newSymbol <- string representation of E
					Symbol newSymbol = DefaultSymbol.createSymbol(expression.toString());
					
					// Get all the free variables in the quantifier expression to create a
					// call to our new random variable expression.
					// F <- array of free variables in E
					Set<Expression> freeVariablesF = Expressions.freeVariables(expression, rewritingProcess);
					Expression newSymbolF = Expressions.make(newSymbol, freeVariablesF);

					// Then create a new rule based on the new expression.
					Expression newPotentialExpression;
					// if Quantifier is "there exists"
					if (ThereExists.isThereExists(expression)) {
						// newPotentialExpressions <- add rule2PotentialExpression("if Phi then newSymbol(F)")
						// add "randomVariable("if Phi then newSymbol(F)", size(F), TypeF1, ..., TypeFn, Boolean) to declarations
						newPotentialExpression = translateConditionalRule(
								Expressions.make(RuleConverter.FUNCTOR_CONDITIONAL_RULE, expression.getArguments().get(0), 
										Expressions.make(RuleConverter.FUNCTOR_ATOMIC_RULE, newSymbolF, 1)));
					}
					else {
						// Quantifier is "for all"
						// newPotentialExpressions <- add rule2PotentialExpression("if not Phi then not newSymbol(F)")
						// add "randomVariable("if not Phi then not newSymbol", size(F), TypeF1, ..., TypeFn, Boolean) to declarations
						newPotentialExpression = translateConditionalRule(
								Expressions.make(RuleConverter.FUNCTOR_CONDITIONAL_RULE, 
										Not.make(expression.getArguments().get(0)), 
										Expressions.make(RuleConverter.FUNCTOR_ATOMIC_RULE, 
												Not.make(newSymbolF), 1)));
						
					}
					expandingPotentialExpressions.add(newPotentialExpression);
					Expression newRandomVariableDeclaration = createNewRandomVariableDeclaration(newSymbolF, newPotentialExpression, randomVariableDeclarations);
					if (newRandomVariableDeclaration != null) {
						randomVariableDeclarations.add(newRandomVariableDeclaration);
					}					

					// Replace the quantifier expression with the newSymbol(F) expression
					// return newSymbol( F )
					return newSymbolF;
				}
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
									queryAtomArgs.get(ii), expressionArgs.get(ii), rewritingProcess);
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
							Expression unique = Expressions.makeUniqueVariable(
									"Unique" + uniqueCount, result, 
									rewritingProcess);
							uniqueCount++;
							uniques.add(unique);
							result = result.replaceAllOccurrences(
									queryAtomArg, unique, rewritingProcess);
						}

						// Then replace the unique variables with the args from the query expression.
						for (int ii = 0; ii < queryAtomArgs.size(); ii++) {
							result = result.replaceAllOccurrences(
									uniques.get(ii), expressionArgs.get(ii), rewritingProcess);
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
