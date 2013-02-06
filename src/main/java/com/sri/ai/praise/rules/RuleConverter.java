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
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.Variables;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.rules.antlr.RuleParserWrapper;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;

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

	private interface NodeInspector {
		public boolean inspectNode(Expression parent, /*Expression child,*/ Object context);
	}

	public class ConverterContext {
		public Expression                 currentExpression;
		public List<Expression>           randomVariableDeclarations;
		public Map<String, Set<Integer>>  randomVariableIndex;
		public List<Expression>           parfactors;
		public List<Expression>           sorts;
		public List<Expression>           randomVariables;
		
		public List<Expression>           processedParfactors;
		public Map<String, Set<Integer>>  functionsFound;
		public int                        uniqueCount = 0;
		public boolean                    runAgain;
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
		ConverterContext context = new ConverterContext();
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
		System.out.println("var index: " + context.randomVariableIndex.toString());
		System.out.println("parfactors: " + context.parfactors.toString());
		System.out.println("random variables: " + context.randomVariables.toString());
		System.out.println("sorts: " + context.sorts.toString());

		// Transform the functions.
		context.processedParfactors = new ArrayList<Expression>();
		translateFunctions(context);

		// Transform the quantifiers.
		context.parfactors = context.processedParfactors;
		context.processedParfactors = new ArrayList<Expression>();
		translateQuantifiers(context);
		context.parfactors = context.processedParfactors;
		context.processedParfactors = new ArrayList<Expression>();
		
		



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
		modelArgs.add(Expressions.apply("parfactors", context.parfactors));
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


	public void translateFunctions (ConverterContext context) {
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
							ConverterContext converterContext = (ConverterContext)context;
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
								Expression replacement = Expressions.make(FunctorConstants.AND, andArgs);
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

	public void translateQuantifiers (ConverterContext context) {
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
								ConverterContext converterContext = (ConverterContext)context;
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
													Expressions.make(FunctorConstants.NOT, parent.getArguments().get(0)), 
													Expressions.make(RuleConverter.FUNCTOR_ATOMIC_RULE, 
															Expressions.make(FunctorConstants.NOT, newExpression), 1))));
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


	/*===================================================================================
	 * PRIVATE METHODS
	 *=================================================================================*/
	private void walkNode (Expression node, Object context, NodeInspector inspector) {
//		System.out.println("walkNode: " + node);
		List<Expression> children = node.getArguments();
		boolean isContinue = inspector.inspectNode(node, /*child,*/ context);
		if (isContinue) {
			for (Expression child : children) {
//				System.out.println("Calling walkNode: " + node + " : " + child);
				walkNode(child, context, inspector);
			}
		}
	}

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
}
