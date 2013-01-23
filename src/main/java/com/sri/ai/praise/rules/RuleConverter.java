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
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.praise.rules.antlr.RuleParserWrapper;
import com.sri.ai.praise.model.Model;

@Beta
public class RuleConverter {

	public static final String FUNCTOR_IF_THEN_ELSE                = "if . then . else .";
	public static final String FUNCTOR_FOR_ALL                     = "for all . : .";
	public static final String FUNCTOR_THERE_EXISTS                = "there exists . : .";

	public static final String FUNCTOR_RANDOM_VARIABLE_DECLARATION = "random variable";
	public static final String FUNCTOR_SORT                        = "sort";

	public static final String FUNCTOR_ATOMIC_RULE                 = "atomic rule";
	public static final String FUNCTOR_CONDITIONAL_RULE            = "conditional rule";
	public static final String FUNCTOR_PROLOG_RULE                 = "prolog rule";

	public static final String FUNCTOR_MAY_BE_SAME_AS              = "may be same as";


	private RuleParserWrapper         ruleParser       = new RuleParserWrapper();
//	private AntlrGrinderParserWrapper grinderParser    = new AntlrGrinderParserWrapper();
//	private RewritingProcess          rewritingProcess = LBPFactory.newLBPProcess(DefaultSymbol.createSymbol(""));

	private interface NodeInspector {
		public boolean inspectNode(Expression parent, Expression child, Object context);
	}

	private class TransformFunctionContext {
		@SuppressWarnings("unused")
		Expression                currentExpression;
		@SuppressWarnings("unused")
		List<Expression>          randomVariableDeclarations;
		@SuppressWarnings("unused")
		Map<String, Set<Integer>> randomVariableIndex;
	}
	

	/*===================================================================================
	 * PUBLIC METHODS
	 *=================================================================================*/
	public Model parseModel (String modelString) {
		return parseModel(ruleParser.parseAll(modelString));
	}
	

	public Model parseModel (List<Expression> inputRules) {
		List<Expression> parfactors                    = new ArrayList<Expression>();
		List<Expression> sorts                         = new ArrayList<Expression>();
		List<Expression> randomVariables               = new ArrayList<Expression>();
//		Map<String, Expression> randomVariableNames    = new HashMap<String, Expression>();
		Map<String, Set<Integer>> randomVariableIndex  = new HashMap<String, Set<Integer>>();
		
		// Sort and convert the rules to their if-then-else forms.
		for (Expression rule : inputRules) {
			if (rule.getFunctor().equals(FUNCTOR_ATOMIC_RULE)) {
				parfactors.add(translateAtomicRule(rule));
			}
			else if (rule.getFunctor().equals(FUNCTOR_PROLOG_RULE)) {
				parfactors.add(translatePrologRule(rule));
			}
			else if (rule.getFunctor().equals(FUNCTOR_CONDITIONAL_RULE)) {
				parfactors.add(translateConditionalRule(rule));
			}
			else if (rule.getFunctor().equals(FUNCTOR_RANDOM_VARIABLE_DECLARATION)) {
				randomVariables.add(this.updateRandomVariableDeclaration(rule));
				String name = rule.get(0).toString();
				if (randomVariableIndex.get(name) == null) {
					Set<Integer> set = new HashSet<Integer>();
					set.add(rule.get(1).intValue());
					randomVariableIndex.put(name, set);
				}
				else {
					randomVariableIndex.get(name).add(rule.get(1).intValue());
				}
//				randomVariableNames.put(rule.get(0).toString(), rule);
			}
			else if (rule.getFunctor().equals(FUNCTOR_SORT)) {
				sorts.add(rule);
			}
		}
//		System.out.println("var names: " + randomVariableNames.toString());
		System.out.println("var index: " + randomVariableIndex.toString());
		System.out.println("parfactors: " + parfactors.toString());
		System.out.println("random variables: " + randomVariables.toString());
		System.out.println("sorts: " + sorts.toString());

		// Transform the functions.
		TransformFunctionContext context = new TransformFunctionContext();
		context.randomVariableDeclarations = randomVariables;
		context.randomVariableIndex = randomVariableIndex;
		for (Expression parfactor : parfactors) {
			//boolean result = 
			walkNode(parfactor, context, new NodeInspector() {
				public boolean inspectNode(Expression parent, Expression child, Object context) {
					if (parent.getFunctor().equals(FunctorConstants.EQUAL) && isRandomFunctionApplication(child)) {
						// TODO: this doesn't do anything yet.
						
					}
					if (parent.getFunctor().equals(FunctorConstants.INEQUALITY) && isRandomFunctionApplication(child)) {
						// TODO: this doesn't do anything yet.
						
					}
					if (isRandomFunctionApplication(parent) && isRandomFunctionApplication(child)) {
						// TODO: this doesn't do anything yet.
						
					}
					return true;
				}
			});
		}
		



		// TODO: This is not the correct way to construct the model expression.
		Expression union = new DefaultCompoundSyntaxTree("union", parfactors);
		@SuppressWarnings("unused")
		Expression modelExpression = new DefaultCompoundSyntaxTree("model", union);

//		return new Model(modelExpression, null);  // TODO: do we need to extract the random variable names?
		return null;
	}

	public boolean isRandomFunctionApplication (Expression e) {
		if (!e.getSyntacticFormType().equals("Function application")) {
			System.out.println(e + " is not function application");
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

	public boolean isRandomVariableValue (Expression e) {
		if (isRandomFunctionApplication(e))
			return true;

		if (e.getSyntacticFormType().equals("Symbol"))
			return true;

		return false;
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

		System.out.println(rule.toString());
		parfactors.add(translateConditionalRule(ruleParser.parse(rule.toString())));

		rule = new StringBuilder();
		rule.append("there exists Y : " + functionName + "(");
		for (ii = 0; ii < numArgs-1; ii++) {
			rule.append('X');
			rule.append(ii);
			rule.append(',');
		}
		rule.append("Y);");

		System.out.println(rule.toString());
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



	/*===================================================================================
	 * PRIVATE METHODS
	 *=================================================================================*/
	private void walkNode (Expression node, Object context, NodeInspector inspector) {
		List<Expression> children = node.getArguments();
		for (Expression child : children) {
			inspector.inspectNode(node, child, context);
			walkNode(child, context, inspector);
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
