/*
 * Copyright (c) 2015, SRI International
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
package com.sri.ai.praise.model.v1.hogm.antlr;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.AND;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUIVALENCE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EXPONENTIATION;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IMPLICATION;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.INTEGER_INTERVAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.NOT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.OR;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.REAL_INTERVAL_CLOSED_CLOSED;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.REAL_INTERVAL_CLOSED_OPEN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.REAL_INTERVAL_OPEN_CLOSED;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.REAL_INTERVAL_OPEN_OPEN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TIMES;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import org.antlr.v4.runtime.ParserRuleContext;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultCountingFormula;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAll;
import com.sri.ai.grinder.sgdpllt.library.boole.Or;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExists;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpllt.library.number.Minus;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets;
import com.sri.ai.praise.model.v1.ConstantDeclaration;
import com.sri.ai.praise.model.v1.HOGMRandomVariableDeclaration;
import com.sri.ai.praise.model.v1.HOGMSortDeclaration;
import com.sri.ai.praise.model.v1.HOGModel;
import com.sri.ai.praise.model.v1.StatementInfo;
import com.sri.ai.praise.model.v1.hogm.antlr.HOGMParser.TermContext;
import com.sri.ai.util.math.Rational;

@Beta
public class HOGModelVisitor extends HOGMBaseVisitor<Expression> {
	// Note track bracketed expressions based on identity to ensure no accidental overwrite by value.
	private Map<Expression, Expression> parenthesizedExpressions = new IdentityHashMap<Expression, Expression>(); 
	//
	private List<StatementInfo> sortDeclarations           = new ArrayList<>();
	private List<StatementInfo> constantDeclarations       = new ArrayList<>();
	private List<StatementInfo> randomVariableDeclarations = new ArrayList<>();
	private List<StatementInfo> terms                      = new ArrayList<>();

	// model : statements+=statement* EOF
	@Override 
	public Expression visitModel(HOGMParser.ModelContext ctx) { 

		sortDeclarations.clear();
		constantDeclarations.clear();
		randomVariableDeclarations.clear();
		terms.clear();
		
		ctx.statements.forEach(s -> visit(s));
		
		Expression result = HOGModel.validateAndConstruct(sortDeclarations, constantDeclarations, randomVariableDeclarations, terms);

		return result;
	}
	
	// aterm : term EOF
	@Override 
	public Expression visitAterm(HOGMParser.AtermContext ctx) { 
		Expression result = visit(ctx.term());	
		return result;
	}
	
	@Override 
	public Expression visitStatement(HOGMParser.StatementContext ctx) { 
		Expression result;
		
		if (ctx.declaration() != null) {
			result = visit(ctx.declaration());
		}
		else {
			TermContext term = ctx.term();
			result = visit(term);
			terms.add(newStatementInfo(result, ctx));
		}
		
		return result;
	}
	
	// sort_decl : SORT name=sort_name (COLON size=(INTEGER | UNKNOWN) (COMMA constants+=constant_name)*)? (SEMICOLON)?
	@Override 
	public Expression visitSort_decl(HOGMParser.Sort_declContext ctx) { 
		Expression name = newSymbol(ctx.name.getText());
		Expression size = HOGMSortDeclaration.UNKNOWN_SIZE;
		if (ctx.size != null) {
			size = newSymbol(ctx.size.getText());
		}
		List<Expression> constants = new ArrayList<Expression>();
		if (ctx.constants != null) {
			ctx.constants.forEach(c -> constants.add(newSymbol(c.getText())));
		}
		
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				HOGMSortDeclaration.FUNCTOR_SORT_DECLARATION, name, size,
				ExtensionalSets.makeUniSet(constants));
		
		sortDeclarations.add(newStatementInfo(result, ctx));
		
		return result;
	}
	
	// constant_decl 
    // : CONSTANT name=constant_name COLON range=sort_name (SEMICOLON)? #propositionalConstantDeclaration
	@Override 
	public Expression visitPropositionalConstantDeclaration(HOGMParser.PropositionalConstantDeclarationContext ctx) { 
		Expression name  = newSymbol(ctx.name.getText());
		Expression arity = Expressions.ZERO;
		Expression range = visit(ctx.range );

		List<Expression> declarationArgs = new ArrayList<Expression>();
		declarationArgs.add(name);
		declarationArgs.add(arity);
		declarationArgs.add(range);

		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				ConstantDeclaration.FUNCTOR_CONSTANT_DECLARATION,
				declarationArgs.toArray());
		
		constantDeclarations.add(newStatementInfo(result, ctx));

		return result; 
	}
	
	// constant_decl 
    // | CONSTANT name=constant_name COLON parameters+=sort_name (X parameters+=sort_name)* MAPPING_RIGHT_ARROW range=sort_name (SEMICOLON)? #relationalConstantDeclaration
	@Override 
	public Expression visitRelationalConstantDeclaration(HOGMParser.RelationalConstantDeclarationContext ctx) { 
		Expression name = newSymbol(ctx.name.getText());
		List<Expression> parameters = expressionsList(ctx.parameters);
		Expression arity = Expressions.makeSymbol(parameters.size());
		Expression range = visit(ctx.range);

		List<Expression> declarationArgs = new ArrayList<Expression>();
		declarationArgs.add(name);
		declarationArgs.add(arity);
		declarationArgs.addAll(parameters);
		declarationArgs.add(range);

		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				ConstantDeclaration.FUNCTOR_CONSTANT_DECLARATION,
				declarationArgs.toArray());
		
		constantDeclarations.add(newStatementInfo(result, ctx));

		return result;
	}
	
	// random_variable_decl 
    // : RANDOM name=constant_name COLON range=sort_name (SEMICOLON)?
	@Override 
	public Expression visitPropositionalRandomVariableDeclaration(HOGMParser.PropositionalRandomVariableDeclarationContext ctx) { 
		Expression name  = newSymbol(ctx.name.getText());
		Expression arity = Expressions.ZERO;
		Expression range = visit(ctx.range);

		List<Expression> declarationArgs = new ArrayList<Expression>();
		declarationArgs.add(name);
		declarationArgs.add(arity);
		declarationArgs.add(range);

		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				HOGMRandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION,
				declarationArgs.toArray());
		
		randomVariableDeclarations.add(newStatementInfo(result, ctx));

		return result; 
	}

	// random_variable_decl 
    // | RANDOM name=constant_name COLON parameters+=sort_name (X parameters+=sort_name)* MAPPING_RIGHT_ARROW range=sort_name (SEMICOLON)?
	@Override 
	public Expression visitRelationalRandomVariableDeclaration(HOGMParser.RelationalRandomVariableDeclarationContext ctx) { 
		Expression name = newSymbol(ctx.name.getText());
		List<Expression> parameters = expressionsList(ctx.parameters);
		Expression arity = Expressions.makeSymbol(parameters.size());
		Expression range = visit(ctx.range);

		List<Expression> declarationArgs = new ArrayList<Expression>();
		declarationArgs.add(name);
		declarationArgs.add(arity);
		declarationArgs.addAll(parameters);
		declarationArgs.add(range);

		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				HOGMRandomVariableDeclaration.FUNCTOR_RANDOM_VARIABLE_DECLARATION,
				declarationArgs.toArray());
		
		randomVariableDeclarations.add(newStatementInfo(result, ctx));

		return result;
	}
	
	// term
	// : OPEN_PAREN term CLOSE_PAREN
	@Override 
	public Expression visitParentheses(HOGMParser.ParenthesesContext ctx) { 
		Expression result = visit(ctx.term());
		
		// Keep track of explicitly bracketed expressions
		// so that the are not flattened as part of the 
		// possiblyFlatten()
		// call for some expressions, e.g.: 1 + 2 + 3.
		parenthesizedExpressions.put(result, result);
		
		return result;
	}
	
	// term
	// | function_application
	// function_application
	// : functor=functor_name OPEN_PAREN ( args+=term (COMMA args+=term)* )? CLOSE_PAREN
 	@Override 
 	public Expression visitFunction_application(HOGMParser.Function_applicationContext ctx) {
 		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(visit(ctx.functor), expressions(ctx.args));
		
		return result; 
	}
 	
 	// term
 	// | VERTICAL_BAR ( indexes+=expr (',' indexes+=expr)* )? COLON body=expr VERTICAL_BAR #countingFormula
 	@Override 
 	public Expression visitCountingFormula(HOGMParser.CountingFormulaContext ctx) { 
 		Expression result = new DefaultCountingFormula(expressionsList(ctx.indexes), visit(ctx.body));
 		
 		return result;
 	}
 	
 	// term
    // | VERTICAL_BAR constant_name VERTICAL_BAR #typeCardinality
 	@Override 
 	public Expression visitTypeCardinality(HOGMParser.TypeCardinalityContext ctx) { 
 		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(CARDINALITY, newSymbol(ctx.constant_name().getText()));
 		
 		return result;
 	}
 	
 	// term
 	// | NOT term
 	@Override
	public Expression visitNot(HOGMParser.NotContext ctx) { 
 		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(NOT, visit(ctx.term()));
		return result; 
 	}
 	
 	// term
 	// | SUBTRACT term
 	@Override 
 	public Expression visitUnaryMinus(HOGMParser.UnaryMinusContext ctx) { 
		Expression argument = visit(ctx.term());
		Expression result;
		if (argument.getValue() instanceof Number) {
			result = Expressions.makeSymbol(argument.rationalValue().negate());
		}
		else {
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(MINUS, argument);
		}
		return result;
 	}
 	
 	// term
 	// |  <assoc=right> base=term EXPONENTIATION exponent=term
 	@Override 
 	public Expression visitExponentiation(HOGMParser.ExponentiationContext ctx) { 
 		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(EXPONENTIATION, visit(ctx.base), visit(ctx.exponent));
		return result;
 	}
 	
 	// term
 	// | leftop=term op=(TIMES | DIVIDE) rightop=term
 	@Override 
 	public Expression visitMultiplicationOrDivision(HOGMParser.MultiplicationOrDivisionContext ctx) { 
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result; 
 	}
 	
 	// term
 	// | leftop=term op=(PLUS | SUBTRACT) rightop=term
 	@Override 
 	public Expression visitAdditionOrSubtraction(HOGMParser.AdditionOrSubtractionContext ctx) { 
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result; 
 	}
 	
 	// term
 	// | leftop=term op=(LESS_THAN | LESS_THAN_EQUAL | EQUAL | NOT_EQUAL | GREATER_THAN_EQUAL | GREATER_THAN) rightop=term
 	@Override 
 	public Expression visitComparison(HOGMParser.ComparisonContext ctx) { 
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(ctx.op.getText(), visit(ctx.leftop), visit(ctx.rightop));
		result = possiblyFlatten(result);
		return result; 
 	}
 	
 	// term
 	// | leftconj=term AND rightconj=term
 	@Override 
 	public Expression visitConjunction(HOGMParser.ConjunctionContext ctx) { 
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(And.FUNCTOR, visit(ctx.leftconj), visit(ctx.rightconj));
		result = possiblyFlatten(result);
		return result; 
 	}
 	
 	// term
 	// | leftdisj=term OR rightdisj=term
 	@Override 
 	public Expression visitDisjunction(HOGMParser.DisjunctionContext ctx) { 
 		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(Or.FUNCTOR, visit(ctx.leftdisj), visit(ctx.rightdisj));
		result = possiblyFlatten(result);
		return result; 
 	}
 	
 	// term
 	// |<assoc=right> antecedent=term IMPLICATION consequent=term
 	@Override 
 	public Expression visitImplication(HOGMParser.ImplicationContext ctx) { 
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(IMPLICATION, visit(ctx.antecedent), visit(ctx.consequent));
		return result; 
 	}
 	
 	// term
 	// |<assoc=right> leftop=term BICONDITIONAL rightop=term
 	@Override 
 	public Expression visitBiconditional(HOGMParser.BiconditionalContext ctx) { 
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(EQUIVALENCE, visit(ctx.leftop), visit(ctx.rightop));
		return result; 
 	}
 	
 	// term
 	// | FOR ALL index=quantifier_index COLON body=term
 	@Override 
 	public Expression visitForAll(HOGMParser.ForAllContext ctx) { 
 		Expression result = ForAll.make(new ExtensionalIndexExpressionsSet(visit(ctx.index).getArguments()), visit(ctx.body));
		return result; 
 	}
 	
 	// term
 	// | THERE EXISTS index=quantifier_index COLON body=term
 	@Override 
 	public Expression visitThereExists(HOGMParser.ThereExistsContext ctx) { 
 		Expression result = ThereExists.make(new ExtensionalIndexExpressionsSet(visit(ctx.index).getArguments()), visit(ctx.body));
		return result; 
 	}
 	
 	// term
 	// | term term
 	@Override 
 	public Expression visitShorthandConditionedPotential(HOGMParser.ShorthandConditionedPotentialContext ctx) { 
 		Expression condition = visit(ctx.term(0));
 		Expression potential = visit(ctx.term(1));
 		
 		Expression result = IfThenElse.make(condition, potential, Minus.make(Expressions.ONE, potential));
 		return result;
 	}
 	
 	// term
 	// | IF condition=term THEN thenbranch=term ELSE elsebranch=term
 	@Override 
 	public Expression visitConditional(HOGMParser.ConditionalContext ctx) { 
 		Expression result = IfThenElse.make(visit(ctx.condition), visit(ctx.thenbranch), visit(ctx.elsebranch));
 		return result;
 	}
 	
 	// term
 	// | IF condition=term THEN thenbranch=term
 	@Override 
 	public Expression visitConditionalUnknownElseBranch(HOGMParser.ConditionalUnknownElseBranchContext ctx) { 
 		Expression result = IfThenElse.make(visit(ctx.condition), visit(ctx.thenbranch), Expressions.ZERO_POINT_FIVE);
 		return result;
 	}
 	
 	// quantifier_index
    // : indexes+=quantifier_index_term // (COMMA indexes+=quantifier_index_term)* Not on For All or There Exists
 	@Override 
 	public Expression visitQuantifier_index(HOGMParser.Quantifier_indexContext ctx) { 
 		Expression result = Expressions.makeTuple(expressions(ctx.indexes));
 		return result;
 	}
 	
 	// : variable=constant_name IN sort=sort_reference #quantifierIndexTermVariableInSort
 	@Override 
 	public Expression visitQuantifierIndexTermVariableInSort(HOGMParser.QuantifierIndexTermVariableInSortContext ctx) {
 		Expression variable      = newSymbol(ctx.variable.getText());
 		Expression sortReference = visit(ctx.sort);
 		
 		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(IN, variable, sortReference);
 		
 		return result; 
 	}

 	// sort_name
    // : IN_BUILT_SORT_BOOLEAN
    // | IN_BUILT_SORT_NUMBER
    // | constant_name
 	@Override 
 	public Expression visitSort_name(HOGMParser.Sort_nameContext ctx) { 
 		Expression result = newSymbol(ctx.getText());
 		
 		return result;
 	}
   
 	// sort_number_integer
    // : start=INTEGER RANGE_SEPARTOR end=INTEGER
 	@Override 
 	public Expression visitSort_integer_interval(HOGMParser.Sort_integer_intervalContext ctx) {
 		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(INTEGER_INTERVAL, newSymbol(ctx.start.getText()), newSymbol(ctx.end.getText()));
 		return result;
 	}
 	
 	// sort_real_interval_closed_closed
 	// INTERVAL_LOWER_CLOSED lower=RATIONAL SEMICOLON upper=RATIONAL INTERVAL_UPPER_CLOSED
 	@Override 
 	public Expression visitSort_real_interval(HOGMParser.Sort_real_intervalContext ctx) { 
 		String intervalType;
 		if (ctx.lower_bracket.getText().equals("[")) {
 			if (ctx.upper_bracket.getText().equals("]")) {
 				intervalType = REAL_INTERVAL_CLOSED_CLOSED;
 			}
 			else { // upper bracket is open i.e. [
 				intervalType = REAL_INTERVAL_CLOSED_OPEN;
 			}
 		}  		
 		else { // Lower bracket is open i.e. ]
 			if (ctx.upper_bracket.getText().equals("]")) {
 				intervalType = REAL_INTERVAL_OPEN_CLOSED;
 			}
 			else { // upper bracket is open i.e. [
 				intervalType = REAL_INTERVAL_OPEN_OPEN;
 			}
 		}
 		Rational lower = new Rational(ctx.lower.getText());
 		if (ctx.negate_lower != null) {
 			lower = lower.negate();
 		}
 		Rational upper = new Rational(ctx.upper.getText());
 		if (ctx.negate_upper != null) {
 			upper = upper.negate();
 		}
 		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(intervalType, Expressions.makeSymbol(lower), Expressions.makeSymbol(upper));
 		return result;
 	}
 	
 	// functor_name
    // : constant_name
 	@Override 
 	public Expression visitFunctor_name(HOGMParser.Functor_nameContext ctx) { 
 		Expression result = newSymbol(ctx.getText());
 		return result;
 	}
 	
 	// symbol
 	// : constant_name
    // | constant_number
 	@Override 
 	public Expression visitSymbol(HOGMParser.SymbolContext ctx) { 
 		Expression result = newSymbol(ctx.getText());
 		return result;
 	}
 	
 	// constant_name
    // : X
    // | CONSTANT_STR
    // | QUOTED_CONSTANT_STR
 	@Override 
 	public Expression visitConstant_name(HOGMParser.Constant_nameContext ctx) { 
 		Expression result = newSymbol(ctx.getText());
 		return result;
 	}
 	
 	// constant_number
    // : INTEGER
    // | RATIONAL
 	@Override 
 	public Expression visitConstant_number(HOGMParser.Constant_numberContext ctx) { 
 		Expression result = newSymbol(ctx.getText());
 		return result; 
 	}
 	
	//
	// PROTECTED
	//
 	protected StatementInfo newStatementInfo(Expression statement, ParserRuleContext ctx) {
 		StatementInfo result = new StatementInfo(statement, ctx.getText(), ctx.getStart().getLine(), ctx.getStart().getStartIndex(), ctx.getStop().getStopIndex());
 		return result;
 	}
 	
	protected Expression newSymbol(String text) {
		Expression result = Expressions.parseTextAndMakeSymbolOrStringLiteral(text);
		return result;
	}
	
	protected Expression[] expressions(List<? extends ParserRuleContext> exprContexts) {
		List<Expression> expressionsList = expressionsList(exprContexts);
		Expression[] result = expressionsList.toArray(new Expression[expressionsList.size()]);
		return result;
	}
	
	protected List<Expression> expressionsList(List<? extends ParserRuleContext> exprContexts) {
		List<Expression> result = new ArrayList<Expression>();
		exprContexts.forEach(exprContext -> result.add(visit(exprContext)));
		return result;
	}
	
	protected Expression possiblyFlatten(Expression expression) {
		Expression result = expression;
		
		Object functor = expression.getFunctor();
		if (functor != null) {
			if (functor.equals(TIMES) || 
			    functor.equals(PLUS)  || 
			    functor.equals(EQUAL) ||
			    functor.equals(AND)   || 
			    functor.equals(OR)) {
				List<Expression> args = new ArrayList<Expression>();
				for (Expression arg : expression.getArguments()) {
					if (arg.getFunctor() != null && functor.equals(arg.getFunctor()) && !parenthesizedExpressions.containsKey(arg)) {
						args.addAll(arg.getArguments());
					}
					else {
						args.add(arg);
					}
				}
				result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(functor, args.toArray());
			}
		}
		
		// Clear in order manage memory
		parenthesizedExpressions.clear();
		
		return result;
	}
}
