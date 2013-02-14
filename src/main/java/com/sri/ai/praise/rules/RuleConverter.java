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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.Variables;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.rules.antlr.RuleParserWrapper;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.util.base.Pair;

@Beta
public class RuleConverter {

	public static final String FUNCTOR_IF_THEN_ELSE                = IfThenElse.FUNCTOR;
	public static final String FUNCTOR_FOR_ALL                     = FunctorConstants.FOR_ALL;
	public static final String FUNCTOR_THERE_EXISTS                = FunctorConstants.THERE_EXISTS;

	public static final String FUNCTOR_RANDOM_VARIABLE_DECLARATION = RandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION;
	public static final String FUNCTOR_SORT                        = SortDeclaration.FUNCTOR_SORT_DECLARATION;

	public static final String FUNCTOR_ATOMIC_RULE                 = "atomic rule";
	public static final String FUNCTOR_CONDITIONAL_RULE            = "conditional rule";
	public static final String FUNCTOR_PROLOG_RULE                 = "prolog rule";

	public static final String FUNCTOR_MAY_BE_SAME_AS              = "may be same as";


	private RuleParserWrapper         ruleParser       = null;
//	private AntlrGrinderParserWrapper grinderParser    = new AntlrGrinderParserWrapper();
	private RewritingProcess          rewritingProcess = null;

	private ReplaceConstraintWithConstant positiveEmbeddedConstraintReplacementFunction = new ReplaceConstraintWithConstant(Expressions.TRUE);
	private ReplaceConstraintWithConstant negativeEmbeddedConstraintReplacementFunction = new ReplaceConstraintWithConstant(Expressions.FALSE);

	private interface NodeInspector {
		public boolean inspectNode(Expression parent, /*Expression child,*/ Object context);
	}

	public class RulesConversionProcess {
		public Expression                        currentExpression;
		public List<Expression>                  randomVariableDeclarations;
		public Map<String, Set<Integer>>         randomVariableIndex;
		public List<Expression>                  parfactors;
		public List<Expression>                  sorts;
		public List<Expression>                  randomVariables;

		public List<Expression>                  processedParfactors;
		public Set<Pair<Expression, Expression>> mayBeSameAsSet;
		public Map<String, Set<Integer>>         functionsFound;
		public int                               uniqueCount = 0;
		public boolean                           runAgain;
	}

	/**
	 * Replacement function for use by embedded constraint extractor.
	 * @author etsai
	 *
	 */
	private class ReplaceConstraintWithConstant implements ReplacementFunctionWithContextuallyUpdatedProcess {
		private Expression constant;

		public Expression constraint;

		public ReplaceConstraintWithConstant(Expression constant) {
			this.constant = constant;
		}

		@Override
		public Expression apply(Expression expression) {
			throw new UnsupportedOperationException("evaluate(Object expression) should not be called.");
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			if (LPIUtil.isConstraint(expression, process)) {
//				System.out.println("Found constraint: " + expression);
				constraint = expression;
				return constant;
			}
			return expression;
		}
	}



	/*===================================================================================
	 * PUBLIC METHODS
	 *=================================================================================*/
	
	public RuleConverter() {
		// Ensure these are instantiated straight away and not when first referenced.
		// This helps ensure any global dependencies are setup correctly.
		rewritingProcess = LBPFactory.newLBPProcess(DefaultSymbol.createSymbol("true"));
		ruleParser       = new RuleParserWrapper();
	}
	
	public Model parseModel (String modelString) {
		return parseModel("", "", modelString);
	}

	public Model parseModel (String name, String description, String modelString) {
		return parseModel(name, description, ruleParser.parseAll(modelString));
	}
	
	public Model parseModel (List<Expression> inputRules) {
		return parseModel("", "", inputRules);
	}

	public Model parseModel (String name, String description, List<Expression> inputRules) {
		RulesConversionProcess context = new RulesConversionProcess();
		context.parfactors                    = new ArrayList<Expression>();
		context.sorts                         = new ArrayList<Expression>();
		context.randomVariables               = new ArrayList<Expression>();
//		Map<String, Expression> randomVariableNames    = new HashMap<String, Expression>();
		context.randomVariableIndex           = new HashMap<String, Set<Integer>>();
		context.functionsFound                = new HashMap<String, Set<Integer>>();
		
		// Sort and convert the rules to their if-then-else forms.
		for (Expression rule : inputRules) {
			if (rule.getFunctor().equals(FUNCTOR_ATOMIC_RULE)) {
				context.parfactors.add(translateAtomicRule(rule));
			}
			else if (rule.getFunctor().equals(FUNCTOR_PROLOG_RULE)) {
				context.parfactors.add(translatePrologRule(rule));
			}
			else if (rule.getFunctor().equals(FUNCTOR_CONDITIONAL_RULE)) {
				context.parfactors.add(translateConditionalRule(rule));
			}
			else if (rule.getFunctor().equals(FUNCTOR_RANDOM_VARIABLE_DECLARATION)) {
				context.randomVariables.add(this.updateRandomVariableDeclaration(rule));
				String varName = rule.get(0).toString();
				if (context.randomVariableIndex.get(varName) == null) {
					Set<Integer> set = new HashSet<Integer>();
					set.add(rule.get(1).intValue());
					context.randomVariableIndex.put(varName, set);
				}
				else {
					context.randomVariableIndex.get(varName).add(rule.get(1).intValue());
				}
//				randomVariableNames.put(rule.get(0).toString(), rule);
			}
			else if (rule.getFunctor().equals(FUNCTOR_SORT)) {
				context.sorts.add(rule);
			}
		}
//		System.out.println("var names: " + randomVariableNames.toString());
//		System.out.println("var index: " + context.randomVariableIndex.toString());
//		System.out.println("parfactors: " + context.parfactors.toString());
//		System.out.println("random variables: " + context.randomVariables.toString());
//		System.out.println("sorts: " + context.sorts.toString());

		// Transform the functions.
		context.processedParfactors = new ArrayList<Expression>();
		translateFunctions(context);

		// Transform the quantifiers.
		context.parfactors = context.processedParfactors;
		context.processedParfactors = new ArrayList<Expression>();
		translateQuantifiers(context);

		// Extract the embedded constraints.
		context.parfactors = context.processedParfactors;
		context.processedParfactors = new ArrayList<Expression>();
		disembedConstraints(context);

		// Create the model object output.
		ArrayList<Expression> modelArgs = new ArrayList<Expression>();
		modelArgs.add(DefaultSymbol.createSymbol(name));
		modelArgs.add(DefaultSymbol.createSymbol(description));
		for (Expression sort : context.sorts)
			modelArgs.add(sort);

		Set<String> randomVariableNames = new HashSet<String>();
		for (Expression randomVariable : context.randomVariables) {
			modelArgs.add(randomVariable);
			randomVariableNames.add(randomVariable.get(0).toString());
		}
		modelArgs.add(Expressions.apply("parfactors", context.processedParfactors));
		Expression modelExpression = Expressions.apply("model", modelArgs);

		return new Model(modelExpression, randomVariableNames);
	}
	



	public Set<Expression> translateRules (List<Expression> rules) {
		Set<Expression> result = new HashSet<Expression>();
		for (Expression rule : rules) {
			result.add(translateRule(rule));
		}
		return result;
	}

	public Expression translateRule (Expression rule) {
		if (rule.getFunctor().equals(FUNCTOR_ATOMIC_RULE)) {
			return translateAtomicRule(rule);
		}
		else if (rule.getFunctor().equals(FUNCTOR_PROLOG_RULE)) {
			return translatePrologRule(rule);
		}
		else if (rule.getFunctor().equals(FUNCTOR_CONDITIONAL_RULE)) {
			return translateConditionalRule(rule);
		}
		return rule;
	}
	
	public Expression translateAtomicRule (Expression rule) {
		List<Expression> args = rule.getArguments();
		if (args.size() != 2)
			return null;

		return new DefaultCompoundSyntaxTree(FUNCTOR_IF_THEN_ELSE, args.get(0), 
				args.get(1), oneMinusPotential(args.get(1)));
	}
	
	public Expression translatePrologRule (Expression rule) {
		List<Expression> args = rule.getArguments();
		
		if (args.size() == 2) {
			return new DefaultCompoundSyntaxTree(FUNCTOR_IF_THEN_ELSE, args.get(1), 
					args.get(0), oneMinusPotential(args.get(0)));
		}
		else if (args.size() == 3){
			return new DefaultCompoundSyntaxTree(FUNCTOR_IF_THEN_ELSE, args.get(2), 
					new DefaultCompoundSyntaxTree(FUNCTOR_IF_THEN_ELSE, args.get(1), 
							args.get(0), oneMinusPotential(args.get(0))),
					0.5);
		}

		return null;
	}
	
	public Expression translateConditionalRule (Expression rule) {
		List<Expression> args = rule.getArguments();
		if (args.size() == 2) {
			return new DefaultCompoundSyntaxTree(FUNCTOR_IF_THEN_ELSE, args.get(0), 
					this.translateRule(args.get(1)),
					0.5);
		}
		else if (args.size() == 3) {
			return new DefaultCompoundSyntaxTree(FUNCTOR_IF_THEN_ELSE, args.get(0), 
					this.translateRule(args.get(1)),
					this.translateRule(args.get(2)));
		}
		return null;
	}


	public void translateFunctions (RulesConversionProcess context) {
		for (Expression parfactor : context.parfactors) {
			context.currentExpression = parfactor;
			context.functionsFound = new HashMap<String, Set<Integer>>();
			context.uniqueCount = 0;
//			System.out.println("Converting: " + parfactor);
			do {
				context.runAgain = false;
//				System.out.println("Iterating walkNode: " + context.currentExpression);
				walkNode(context.currentExpression, context, new NodeInspector() {
					public boolean inspectNode(Expression parent, /*Expression child,*/ Object context) {
						if (parent.getArguments().size() == 0)
							return true;
//						System.out.println(parent);
//						System.out.println("inspectNode: " + parent);
						if (parent.getFunctor().equals(FunctorConstants.EQUAL) || parent.getFunctor().equals(FunctorConstants.INEQUALITY) ||
								isRandomFunctionApplication(parent)) {
							RulesConversionProcess converterContext = (RulesConversionProcess)context;
							boolean isReplace = false;
							List<Expression> arguments = new ArrayList<Expression>(); 
							List<Expression> andArgs   = new ArrayList<Expression>();
							for (Expression argument : parent.getArguments()) {
								if (isRandomVariableValue(argument, converterContext.randomVariableIndex)) {
									converterContext.runAgain = true;
									isReplace = true;
									Expression unique = Expressions.makeUniqueVariable(
											"X" + converterContext.uniqueCount, converterContext.currentExpression, 
											rewritingProcess);
//									System.out.println("Incrementing unique count: " + converterContext.uniqueCount + ",  " + parent + " | " + argument);
									converterContext.uniqueCount++;
									arguments.add(unique);
									List<Expression> newArgumentArgs = new ArrayList<Expression>(argument.getArguments());
									newArgumentArgs.add(unique);

									String name;
									if (argument.getArguments().size() == 0)
										name = argument.toString();
									else
										name = argument.getFunctor().toString();
									andArgs.add(Expressions.make(name, newArgumentArgs));

									// Note the function name and param count so can add additional rule enforcing functional
									// relation later.
									Set<Integer> paramCount;
									paramCount = converterContext.functionsFound.get(name);
									if (paramCount == null) {
										paramCount = new HashSet<Integer>();
										converterContext.functionsFound.put(name, paramCount);
									}
									paramCount.add(argument.getArguments().size());
								}
								else {
									arguments.add(argument);
								}
							}
							if(isReplace) {
//								System.out.println("arguments: " + arguments);
//								System.out.println("andArgs:   " + andArgs);
								andArgs.add(Expressions.make(parent.getFunctor(), arguments));
								Expression replacement = And.make(andArgs);
//								System.out.println("Replace <" + parent + ">  with <" + replacement + ">");
								converterContext.currentExpression = 
										converterContext.currentExpression.replaceAllOccurrences(
												parent, replacement, rewritingProcess);
//								System.out.println("Current expression: "+ converterContext.currentExpression);
								return false;
							}

						}
						return true;
					}
				});
			} while (context.runAgain);
			
			context.processedParfactors.add(context.currentExpression);
		}

//		System.out.println("Functions found: " + context.functionsFound);
		for (String functor : context.functionsFound.keySet()) {
			Set<Integer> counts = context.functionsFound.get(functor);
			for (Integer count : counts) {
				this.createTransformedFunctionConstraints(functor, count, context.processedParfactors);
			}
		}
	}

	public boolean isRandomFunctionApplication (Expression e) {
		if (!e.getSyntacticFormType().equals("Function application")) {
//			System.out.println(e + " is not function application");
			return false;
		}

		Expression functor = e.getFunctor();
		if (functor.equals(FunctorConstants.AND) || functor.equals(FunctorConstants.OR) || 
				functor.equals(FunctorConstants.NOT) || functor.equals(FunctorConstants.EQUIVALENCE) ||
				functor.equals(FunctorConstants.IMPLICATION) || functor.equals(FUNCTOR_IF_THEN_ELSE) ||
				functor.equals(FUNCTOR_THERE_EXISTS) || functor.equals(FUNCTOR_FOR_ALL) ||
				functor.equals(FUNCTOR_MAY_BE_SAME_AS)) {
			return false;
		}

		return true;
	}

	public boolean isRandomVariableValue (Expression e, Map<String, Set<Integer>> randomVariableIndex) {
		if (isRandomFunctionApplication(e))
			return true;

		if (e.getSyntacticFormType().equals("Symbol")) {
			if (randomVariableIndex == null) {
				return false;
			}

//			Expression functor = e.getFunctor();
//			System.out.println("e: " + e);
//			System.out.println("Functor: " + functor);
			Set<Integer> paramCounts = randomVariableIndex.get(e.toString());
			if (paramCounts != null && paramCounts.contains(0)) {
				return true;
			}
		}

		return false;
	}

	public void createTransformedFunctionConstraints (String functionName, int numArgs, List<Expression> parfactors) {
		StringBuilder rule = new StringBuilder();
		int ii;
		rule.append("if ");
		rule.append(functionName);
		rule.append('(');
		for (ii = 0; ii < numArgs-1; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Y) then not ");
		rule.append(functionName);
		rule.append('(');
		for (ii = 0; ii < numArgs-1; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Z);");

//		System.out.println(rule.toString());
		parfactors.add(translateConditionalRule(ruleParser.parse(rule.toString())));

		rule = new StringBuilder();
		rule.append("there exists Y : " + functionName + "(");
		for (ii = 0; ii < numArgs-1; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Y);");

//		System.out.println(rule.toString());
		parfactors.add(translateAtomicRule(ruleParser.parse(rule.toString())));
	}

	public Expression updateRandomVariableDeclaration (Expression randomVariableDecl) {
		if (!randomVariableDecl.getFunctor().equals(FUNCTOR_RANDOM_VARIABLE_DECLARATION))
			return null;

		// If the return type is Boolean, don't update the declaration.
		List<Expression> oldArgs = randomVariableDecl.getArguments();
		if (oldArgs.get(oldArgs.size()-1).equals("Boolean"))
			return randomVariableDecl;

		List<Expression> newArgs = new ArrayList<Expression>(oldArgs.size()+1);
		for (int ii = 0; ii < oldArgs.size(); ii++) {
			if (ii == 1)
				newArgs.add(DefaultSymbol.createSymbol(oldArgs.size() - 2));
			else
				newArgs.add(oldArgs.get(ii));
		}
		return new DefaultCompoundSyntaxTree(randomVariableDecl.getFunctor(), newArgs);
	}

	public void translateQuantifiers (RulesConversionProcess context) {
		for (Expression parfactor : context.parfactors) {
			context.currentExpression = parfactor;
			context.uniqueCount = 0;
//			System.out.println("Converting: " + parfactor);
			do {
				context.runAgain = false;
//				System.out.println("Iterating walkNode: " + context.currentExpression);
				walkNode(context.currentExpression, context, new NodeInspector() {
					public boolean inspectNode(Expression parent, /*Expression child,*/ Object context) {
						if (parent.getArguments().size() > 0) {
//						System.out.println("inspectNode: " + parent);
							if (parent.getFunctor().equals(FunctorConstants.FOR_ALL) || 
									parent.getFunctor().equals(FunctorConstants.THERE_EXISTS)) {
								RulesConversionProcess converterContext = (RulesConversionProcess)context;
								converterContext.runAgain = true;
								Symbol newFunctor = DefaultSymbol.createSymbol(parent.toString());
								Set<Expression> variables = Variables.freeVariables(parent, rewritingProcess);
								Expression newExpression = Expressions.make(newFunctor, variables);
//								System.out.println("Generated expression: " + newExpression);
								if (parent.getFunctor().equals(FunctorConstants.THERE_EXISTS)) {
									converterContext.processedParfactors.add(translateConditionalRule(
											Expressions.make(RuleConverter.FUNCTOR_CONDITIONAL_RULE, parent.getArguments().get(0), 
													Expressions.make(RuleConverter.FUNCTOR_ATOMIC_RULE, newExpression, 1))));
								}
								else {
									converterContext.processedParfactors.add(translateConditionalRule(
											Expressions.make(RuleConverter.FUNCTOR_CONDITIONAL_RULE, 
													Not.make(parent.getArguments().get(0)), 
													Expressions.make(RuleConverter.FUNCTOR_ATOMIC_RULE, 
															Not.make(newExpression), 1))));
								}
//								System.out.println("Replacing: " + parent + " with " + newExpression);
//								System.out.println(converterContext.currentExpression);
								converterContext.currentExpression = 
										converterContext.currentExpression.replaceAllOccurrences(
												parent, newExpression, rewritingProcess);
//										converterContext.currentExpression.replaceAllOccurrences(
//												parent, newExpression, rewritingProcess);
//								System.out.println(converterContext.currentExpression);
								return false;
							}
						}
						return true;
					}
				});
			} while (context.runAgain);
			
			context.processedParfactors.add(context.currentExpression);
		}
		
	}

	public void disembedConstraints (RulesConversionProcess context) {
		List<Pair<Expression, Expression>> setOfConstrainedPotentialExpressions = 
				new ArrayList<Pair<Expression, Expression>>();
//		context.simplifier = new CompleteSimplify();//LBPFactory.newCompleteSimplify();

		for (Expression parfactor : context.parfactors) {
			context.currentExpression = parfactor;
			context.mayBeSameAsSet = new HashSet<Pair<Expression, Expression>>();
//			System.out.println("Searching for 'may be same as': " + parfactor);

			// Gather instances of "may be same as".
			do {
//				System.out.println("May be same as loop: begin");
				context.runAgain = false;
				walkNode(context.currentExpression, context, new NodeInspector() {
					public boolean inspectNode(Expression parent, /*Expression child,*/ Object context) {
//						System.out.println("inspecting: " + parent);
						if (parent.getArguments().size() > 0) {
							if (parent.getFunctor().equals(RuleConverter.FUNCTOR_MAY_BE_SAME_AS)) {
//								System.out.println("Found 'may be same as'");

								RulesConversionProcess converterContext = (RulesConversionProcess)context;
								converterContext.runAgain = true;

								// Add both variants of the pair.
								converterContext.mayBeSameAsSet.add(
										new Pair<Expression, Expression>(
												parent.getArguments().get(0), parent.getArguments().get(1)));
								converterContext.mayBeSameAsSet.add(
										new Pair<Expression, Expression>(
												parent.getArguments().get(1), parent.getArguments().get(0)));
//								System.out.println(converterContext.mayBeSameAsSet);

								// Replace the "may be same as" expressions with true.
								Expression newExpression = 
										converterContext.currentExpression.replaceAllOccurrences(
												parent, Expressions.TRUE, rewritingProcess);
//								System.out.println("about to run simplify: " + newExpression);
								converterContext.currentExpression = 
										rewritingProcess.rewrite(LBPRewriter.R_simplify, newExpression);
//								System.out.println("done running simplify: " + converterContext.currentExpression);
								return false;
							}
						}
						return true;
					}
				});
			} while (context.runAgain);

//			System.out.println("Completed search for may be same as expressions: " + context.mayBeSameAsSet);
//			System.out.println("Potential expression: " + context.currentExpression);

			// Get free variables and create inequality constraints on all pairs except those
			// pairs stated to be "may be same as".
			List<Expression> constraints = new ArrayList<Expression>();
			Set<Expression> variables = Variables.freeVariables(parfactor, rewritingProcess);
//			System.out.println("Free variables: " + variables);
			Expression[] variableArray = new Expression[variables.size()];
			variables.toArray(variableArray);
			for (int ii = 0; ii < variables.size() - 1; ii++) {
				for (int jj = ii+1; jj < variables.size(); jj++) {
					// Check if this pair is in the "may be same as" set.
					Expression arg1 = variableArray[ii];
					Expression arg2 = variableArray[jj];
					if (!context.mayBeSameAsSet.contains(new Pair<Expression, Expression>(arg1, arg2))) {
						// If the pair is not in the "may be same as" set, then add it to the list of constraints.
						constraints.add(Disequality.make(arg1, arg2));
					}
				}
			}

//			System.out.println("Generated constraints: " + constraints);
			setOfConstrainedPotentialExpressions.add(
					new Pair<Expression, Expression>(context.currentExpression, And.make(constraints)));
		}

		// Extract the embedded constraints from the potential expressions.
		for (int ii = 0; ii < setOfConstrainedPotentialExpressions.size(); ii++) {

			// Check if the potential expression has any more embedded constraints.
			Pair<Expression, Expression> pair = setOfConstrainedPotentialExpressions.get(ii);
//			System.out.println("Searching for embedded constraints " + ii + ": " + pair.first);
			List<Expression> result = getReplacementsIfAny(pair.first, rewritingProcess);

			// If the result is null, then were no more embedded constraints found.  If the
			// result is non-null, then we add the true and false substituted versions of the
			// potential expression to the end of the list of potential expressions to be process,
			// so that we can check if there are more embedded constraints to extract.
			if (result == null) {
				// Add the complete parfactor to the completely-processed parfactor list.
				context.processedParfactors.add(createParfactor(pair.first, pair.second));
			}
			else {
				// Add the positive case to the list of potential expressions for further processing.
				Expression constraints = pair.second;
				Expression additionalConstraint = result.get(2);
				Expression potentialExpression = result.get(0);
				addFurtherConstrainedPotentialExpression(setOfConstrainedPotentialExpressions, potentialExpression, constraints, additionalConstraint);

				// Add the negative case to the list of potential expressions for further processing.
				constraints = pair.second;
				additionalConstraint = Not.make(result.get(2));
				potentialExpression = result.get(1);
				addFurtherConstrainedPotentialExpression(setOfConstrainedPotentialExpressions, potentialExpression, constraints, additionalConstraint);
			}

		}
		
	}

	private void addFurtherConstrainedPotentialExpression(List<Pair<Expression, Expression>> setOfConstrainedPotentialExpressions, Expression potentialExpression, Expression constraints, Expression additionalConstraint) {
		Expression cPrime;
		constraints = And.make(constraints, additionalConstraint);
		cPrime = rewritingProcess.rewrite(LBPRewriter.R_simplify, constraints);
		if (!cPrime.equals(Expressions.FALSE)) {
			RewritingProcess processUnderAssumption  = GrinderUtil.extendContextualConstraint(cPrime, rewritingProcess);
			Expression pPrime = processUnderAssumption.rewrite(LBPRewriter.R_simplify, potentialExpression);
			if (!Expressions.isNumber(pPrime)) {
				setOfConstrainedPotentialExpressions.add(new Pair<Expression, Expression>(pPrime, cPrime));
			}
		}
	}

	public Expression createParfactor (Expression potentialExpression, List<Expression> constraints) {
		return createParfactor(potentialExpression, And.make(constraints));
	}

	public Expression createParfactor (Expression potentialExpression, Expression constraints) {
		Set<Expression> variableSet = Variables.freeVariables(potentialExpression, rewritingProcess);
		List<Expression> variableList = new ArrayList<Expression>();
		for (Expression variable : variableSet) {
			variableList.add(variable);
		}
		return IntensionalSet.makeMultiSetFromIndexExpressionsList(
				variableList, 
				potentialExpression, constraints);
	}


	/*===================================================================================
	 * PRIVATE METHODS
	 *=================================================================================*/
	/**
	 * Recursively walks the tree subnodes of the given expression, calling the node inspector at
	 * every subnode, including the given expression itself.
	 * @param node       The current expression/subexpression to inspect.
	 * @param context    Contextual information to pass to the node inspector.
	 * @param inspector  The node inspector to call on each subnode.
	 */
	private void walkNode (Expression node, Object context, NodeInspector inspector) {
		List<Expression> children = node.getArguments();
		boolean isContinue = inspector.inspectNode(node, /*child,*/ context);
		if (isContinue) {
			for (Expression child : children) {
				walkNode(child, context, inspector);
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
		return new DefaultCompoundSyntaxTree("-", 1, potential);
	}

	/**
	 * Returns list (Pt, Pf, Constraint) if potentialExpression (P, C) contains a constraint Constraint, and Pt and Pf are P[Constraint/true] and P[Constraint/false] respectively,
	 * or null if there is no such constraint.
	 */
	private List<Expression> getReplacementsIfAny(Expression potentialExpression, RewritingProcess process) {
		Expression pT = potentialExpression.replaceFirstOccurrence(positiveEmbeddedConstraintReplacementFunction, process);
		if (pT == potentialExpression) {
			return null;
		}

		Expression pF = potentialExpression.replaceFirstOccurrence(negativeEmbeddedConstraintReplacementFunction, process);
		List<Expression> result = new ArrayList<Expression>();
		result.add(pT);
		result.add(pF);
		result.add(negativeEmbeddedConstraintReplacementFunction.constraint);
//		System.out.println("Positive replacement: " + pT);
//		System.out.println("Negative replacement: " + pF);
//		System.out.println("Constraint: " + negativeEmbeddedConstraintReplacementFunction.constraint);
		return result;
	}

	/**
	 * Returns list of fours elements ( (P,C), Pt, Pf, Constraint) ) where (P,C) is the first constrained potential expression that has a constraint Constraint,
	 * and Pt and Pf are P[Constraint/true] and P[Constraint/false] respectively,
	 * or null if there is no such constrained potential expression.
	 */
//	private List<Object> getConstrainedPotentialExpressionReplacementsIfAny(List<Pair<Expression,Expression>> constrainedPotentialExpressions, RewritingProcess process) {
//		for (Pair<Expression, Expression> constrainedPotentialExpression : constrainedPotentialExpressions) {
//			List<Object> result= getReplacementsIfAny(constrainedPotentialExpression.first , process);
//			if (result!= null) {
//				((ArrayList<Object>)result).add(0, constrainedPotentialExpression);
//				return result;
//			}
//		}
//		return null;
//	}


}
