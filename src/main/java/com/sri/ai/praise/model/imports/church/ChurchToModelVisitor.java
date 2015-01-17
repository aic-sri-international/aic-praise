/*
 * Copyright (c) 2014, SRI International
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
package com.sri.ai.praise.model.imports.church;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;

import org.antlr.v4.runtime.misc.NotNull;
import org.apache.commons.lang3.StringEscapeUtils;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.imports.church.antlr.ChurchBaseVisitor;
import com.sri.ai.praise.imports.church.antlr.ChurchParser;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.rules.RuleConverter;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.CartesianProductEnumeration;
import com.sri.ai.util.math.Rational;

/**
 * Utility class for converting a parsed Church Program to a HOGMs model.
 * 
 * @author oreilly
 *
 */
@Beta
public class ChurchToModelVisitor extends ChurchBaseVisitor<Expression> {
	//
	private static final String FLIP_ID_PREFIX     = "flip";
	private static final String CHURCH_VALUES_SORT = "Values";
	private static final Map<Expression, Expression> operatorMap = Util.map(
				Expressions.makeSymbol("eq?"), Expressions.makeSymbol("="),
				Expressions.makeSymbol("eqv?"), Expressions.makeSymbol("="),
				Expressions.makeSymbol("equal?"), Expressions.makeSymbol("=")
			);
	//
	private String                   churchProgramName        = null;
	private String                   churchProgram            = null;
	private List<String>             randoms                  = new ArrayList<>();
	private Set<Expression>          knownConstants           = new LinkedHashSet<>();
	private Set<Expression>          knownRandomVariableNames = new LinkedHashSet<>();
	private List<String>             rules                    = new ArrayList<>();
	private List<Expression>         queries                  = new ArrayList<>();
	private Map<Integer, Rational>   flipIdToValue            = new LinkedHashMap<>();
	private List<Expression>         lambdaParams             = new ArrayList<>();
	private Rewriter                 rNormalize               = LBPFactory.newNormalize();
	
	public void setChurchProgramInformation(String name, String program) {
		churchProgramName = name;
		churchProgram     = program;
	}

	@Override 
	public synchronized Expression visitParse(@NotNull ChurchParser.ParseContext ctx) {		
		Expression result = null;
		
		// Clear down the working variables
		randoms.clear();
		knownConstants.clear();
		knownRandomVariableNames.clear();
		rules.clear();
		queries.clear();
		flipIdToValue.clear();
		
		visitChildren(ctx);
		
		// Construct the HOGM		
		StringBuilder hogm = new StringBuilder();
		hogm.append("\n");
		if (knownConstants.size() > 0) {
			StringJoiner knownConstantsCommaSeparatedList = new StringJoiner(", ", ", ", "");
			knownConstants.forEach(constant -> knownConstantsCommaSeparatedList.add(constant.toString()));
			hogm.append("sort " + CHURCH_VALUES_SORT + " : " + SortDeclaration.UNKNOWN_SIZE + knownConstantsCommaSeparatedList + ";\n\n");
		}
		else {
			boolean randomsReferToValuesSort = false;
			for (String randomDeclaration : randoms) {
				if (randomDeclaration.contains(CHURCH_VALUES_SORT)) {
					randomsReferToValuesSort = true;
					break;
				}
			}
			if (randomsReferToValuesSort) {
				hogm.append("sort " + CHURCH_VALUES_SORT + ";\n\n");
			}
		}
		
		for (String rv : randoms) {
			hogm.append(rv+";\n");
		}
		hogm.append("\n");
		for (String r : rules) {
			if (Expressions.ZERO_POINT_FIVE.equals(r)) {
				continue; // simplified to know nothing about the random variable so skip it
			}
			hogm.append(r+";\n");
		}

		Model m = RuleConverter.makeModel(churchProgramName, "\n"+churchProgram+"\n--->\n"+hogm.toString(), hogm.toString());
		
		result = Tuple.make(newSymbol(hogm.toString()),
							m.getModelDefinition(),
							ExtensionalSet.makeUniSet(queries));
		
		return result;
	}
	
	@Override 
	public Expression visitChurchQuery(@NotNull ChurchParser.ChurchQueryContext ctx) { 
		visit(ctx.model);
		queries.add(visit(ctx.query));
		if (ctx.condition != null) {
			visit(ctx.condition);
		}
		return null; // Required information is gathered into relevant attributes and not passed back through API
	}
	
	@Override 
	public Expression visitChurchQueryCondition(@NotNull ChurchParser.ChurchQueryConditionContext ctx) { 
		Expression condition = visit(ctx.churchEvidenceCommand());
		
		rules.add(condition.toString());
		
		return null; // Required information is gathered into relevant attributes and not passed back through API
	}
	
	@Override 
	public Expression visitSpecialUniversallyQuantifiedCommand(@NotNull ChurchParser.SpecialUniversallyQuantifiedCommandContext ctx) { 
		final Map<Expression, Expression> variableIdentifierToLogicalName = new LinkedHashMap<>();
		if (ctx.universals != null && ctx.universals.variable() != null && ctx.universals.variable().size() > 0) {
			for (int i = 0; i < ctx.universals.variable().size(); i++) {
				Expression variable = visit(ctx.universals.variable(i));
				variableIdentifierToLogicalName.put(variable, this.newLogicalVariable(variable.toString()));
			}
		}
		Expression body = visit(ctx.bodyLogic);
		
		Expression result = body;
		if (variableIdentifierToLogicalName.size() > 0) {
			result = body.replaceAllOccurrences(new AbstractReplacementFunctionWithContextuallyUpdatedProcess() {					
				@Override
				public Expression apply(Expression expression, RewritingProcess process) {
					Expression result = expression;
					if (Expressions.isSymbol(expression)) {
						Expression varName = variableIdentifierToLogicalName.get(expression);
						if (varName != null) {
							result = varName;
						}
					}
					return result;
				}
			}, LBPFactory.newLBPProcess());
		}
		
		return result;
	}

	
	@Override 
	public Expression visitDefinition(@NotNull ChurchParser.DefinitionContext ctx) {
		Expression result = visitChildren(ctx);			
		return result;
	}
	
	@Override 
	public Expression visitCommand(@NotNull ChurchParser.CommandContext ctx) {
		Expression result = visitChildren(ctx);
		return result;
	}
	
	@Override 
	public Expression visitDefineChurchMemoization(@NotNull ChurchParser.DefineChurchMemoizationContext ctx) {
		Expression name = visit(ctx.name);
		Expression body = visit(ctx.procedure);
		
		Expression result = defineInHOGM(name, lambdaParams, body);
		
		return result; 
	}
	
	@Override 
	public Expression visitDefineBinding(@NotNull ChurchParser.DefineBindingContext ctx) { 
		Expression name = visit(ctx.name);
		Expression body = visit(ctx.binding);
						
		Expression result = defineInHOGM(name, Collections.<Expression>emptyList(), body);
		
		return result;
	}
	
	@Override 
	public Expression visitDefineProcedure(@NotNull ChurchParser.DefineProcedureContext ctx) { 
		Expression name = visit(ctx.name);
		Expression body = visit(ctx.bodyLogic);
		
		List<Expression> params = new ArrayList<>();
		if (ctx.arguments != null && ctx.arguments.variable() != null && ctx.arguments.variable().size() > 0) {
			for (int i = 0; i < ctx.arguments.variable().size(); i++) {
				params.add(visit(ctx.arguments.variable(i)));
			}
		}
						
		Expression result = defineInHOGM(name, params, body);
		
		return result;
	}
	
	@Override 
	public Expression visitLambdaExpression(@NotNull ChurchParser.LambdaExpressionContext ctx) {		
		if (ctx.formals() != null) {
			visit(ctx.formals());
		}	
		Expression result = visit(ctx.body());
	
		return result;
	}
	
	@Override 
	public Expression visitConditional(@NotNull ChurchParser.ConditionalContext ctx) { 
		Expression test       = visit(ctx.test());
		Expression consequent = visit(ctx.consequent());		
		Expression alternate  = null;
		
		if (ctx.alternate() != null) {
			alternate = visit(ctx.alternate());
		}
		
		Expression result;
		if (alternate == null) {			
// TODO - correct way to handle no alternate?			
			result = IfThenElse.make(test, consequent, Expressions.FALSE);
		}
		else {
			result = IfThenElse.make(test, consequent, alternate);
		}
				
		return result;
	}
	
	@Override 
	public Expression visitFormals(@NotNull ChurchParser.FormalsContext ctx) {
// TODO - add a stack mechanism, as this won't support nested lambda expressions.
//        Though, how should we handle lamba's outside of a (mem ...) - i.e. what
//        would we name it? Likely need to create an anonymous/unique random variable?
		lambdaParams.clear();
		if (ctx.variable() != null && ctx.variable().size() > 0) {
			for (int i = 0; i < ctx.variable().size(); i++) {
				lambdaParams.add(visit(ctx.variable(i)));
			}
		}
		return Expressions.TRUE;
	}
	
	@Override 
	public Expression visitFlip(@NotNull ChurchParser.FlipContext ctx) {
		
		Expression value = Expressions.ZERO_POINT_FIVE;
		if (ctx.value != null) {
			value = visit(ctx.value);
		}
		
		// Must be a number and in the interval [0, 1]
		boolean badNumber = false;		
		if (Expressions.isNumber(value)) {
			if (value.rationalValue().compareTo(Rational.ZERO) < 0 
					|| 
				value.rationalValue().compareTo(Rational.ONE) > 0) {
				
				badNumber = true;		
			}
		}
		else {
			badNumber = true;
		}
		
		if (badNumber) {
			throw new IllegalArgumentException("flip value must be in interval [0, 1]: "+value);
		}

		Integer flipId = this.flipIdToValue.size();
		flipIdToValue.put(flipId, value.rationalValue());
		
		Expression result = newSymbol(FLIP_ID_PREFIX+flipId);
		
		return result; 
	}
	
	@Override
	public Expression visitLogicalOperatorExpression(@NotNull ChurchParser.LogicalOperatorExpressionContext ctx) { 		
		List<Expression> arguments = new ArrayList<Expression>();
		
		if (ctx.test() != null && ctx.test().size() > 0) {
			for (int i = 0; i < ctx.test().size(); i++) {
				arguments.add(visit(ctx.test(i)));			
			}
		}
		
		Expression result = null;
		
		if (ctx.AND() != null) {
			result = And.make(arguments);
		}
		else if (ctx.OR() != null) {	
			result = Or.make(arguments);		
		}
		else if (ctx.NOT() != null) {
			result = Not.make(arguments.get(0));
		}	
		return result; 
	}
	
	@Override
	public Expression visitProcedureCall(@NotNull ChurchParser.ProcedureCallContext ctx) { 
		Expression       operator = visit(ctx.operator());
		List<Expression> operands = new ArrayList<Expression>();
		
		if (ctx.operand() != null && ctx.operand().size() > 0) {
			for (int i = 0; i < ctx.operand().size(); i++) {
				operands.add(visit(ctx.operand(i)));			
			}
		}
		if (operatorMap.containsKey(operator)) {
			operator = operatorMap.get(operator);
		}
		
		Expression result = Expressions.apply(operator, operands);
		
		return result; 
	}
	
	@Override 
	public Expression visitBody(@NotNull ChurchParser.BodyContext ctx) {
// TODO - body is defined as:
// body:  (definition)* sequence;
// we need a way to handle the optional definitions and more that 1 expression in the sequence.

		Expression result = visitChildren(ctx);
		
		return result;
	}
	
	@Override 
	public Expression visitSelfEvaluating(@NotNull ChurchParser.SelfEvaluatingContext ctx) { 
		Expression result = null;
		if (ctx.CHARACTER() != null || ctx.STRING() != null) {
			result = newSymbol(ctx.getText());
		}
		else {
			result = visitChildren(ctx);
		}
		return result; 
	}
	
	@Override 
	public Expression visitSimpleDatum(@NotNull ChurchParser.SimpleDatumContext ctx) {
		Expression result = null;
		if (ctx.CHARACTER() != null || ctx.STRING() != null) {
			result = newSymbol(ctx.getText());
		}
		else {
			result = visitChildren(ctx);
		}
		return result;
	}
	
	@Override 
	public Expression visitIdentifier(@NotNull ChurchParser.IdentifierContext ctx) { 
		Expression result = newSymbol(ctx.getText());
		return result;
	}
	
	@Override 
	public Expression visitVariable(@NotNull ChurchParser.VariableContext ctx) { 
		Expression result = null;
		if (ctx.VARIABLE() != null) {
			result = newSymbol(ctx.VARIABLE().getText());
		}
		else {
			throw new UnsupportedOperationException("Church Variable Ellipsis not supported");
		}
		return result;
	}
	
	@Override 
	public Expression visitList(@NotNull ChurchParser.ListContext ctx) { 
		throw new UnsupportedOperationException("Church List to HOGM currently not supported"); 
	}
	
	@Override 
	public Expression visitVector(@NotNull ChurchParser.VectorContext ctx) { 
		throw new UnsupportedOperationException("Church Vector to HOGM currently not supported");
	}
	
	@Override 
	public Expression visitNumber(@NotNull ChurchParser.NumberContext ctx) {
		Expression result = null;
		if (ctx.NUM_10() != null) {
			result = newSymbol(ctx.getText());
		}
		else {
			throw new UnsupportedOperationException("Currently do not support numbers like: "+ctx.getText());
		}	
		return result; 
	}
	
	@Override 
	public Expression visitBool(@NotNull ChurchParser.BoolContext ctx) {
		Expression result = ctx.TRUE() != null ? Expressions.TRUE : Expressions.FALSE;	
		return result;
	}
	
	//
	// PROTECTED
	//
	protected Expression defineInHOGM(Expression name, List<Expression> params, Expression body) {
		Expression result = null;
		
		// Track the known random variable names
		knownRandomVariableNames.add(name);
		
		// Determine the correct argument names for any of the
		StringBuilder                     rArgs         = new StringBuilder();
		final Map<Expression, Expression> paramVarNames = new HashMap<Expression, Expression>();
		boolean firstArg = true;
		int cnt = 0;
		Expression randomVariable = name;
		List<Expression> rvArgs = new ArrayList<Expression>();
		for (Expression arg : params) {
			if (firstArg) {
				firstArg = false;
				rArgs.append(" ");
			}
			else {
				rArgs.append(" x ");			
			}
			// Ensure name is upper cased
			Expression logicalVariableArg = newLogicalVariable(arg.toString());
			rvArgs.add(logicalVariableArg);
			params.set(cnt, logicalVariableArg);
			paramVarNames.put(arg, params.get(cnt));
// TODO - anything better?			
			rArgs.append(CHURCH_VALUES_SORT);
			cnt++;
		}
		randoms.add("random "+name+":"+rArgs+(rArgs.length() > 0 ? " -> " : " ") + "Boolean");
		StringJoiner knownRandomVariablesHLM = new StringJoiner(";\n", "", ";");
		randoms.forEach(r -> knownRandomVariablesHLM.add(r));
		RewritingProcess processForRV = LBPFactory.newLBPProcessWithHighLevelModel("sort "+CHURCH_VALUES_SORT+";\n\n"+knownRandomVariablesHLM.toString());		
		if (rvArgs.size() > 0) {
			randomVariable = Expressions.apply(randomVariable, rvArgs);
			processForRV   = LPIUtil.extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(randomVariable, processForRV);
		}
		
		final List<List<Boolean>>       flipValues                = new ArrayList<List<Boolean>>();
		final List<Boolean>             trueFalseValues           = new ArrayList<Boolean>();
		final Map<Expression, Integer>  flipMarkerToId            = new LinkedHashMap<Expression, Integer>();
		final Map<Expression, Integer>  flipMarkerToFlipValuesIdx = new LinkedHashMap<Expression, Integer>();
		// Flips <- array of flip applications in body
		trueFalseValues.add(Boolean.FALSE);
		trueFalseValues.add(Boolean.TRUE);
		
		for (Integer flipId : flipIdToValue.keySet()) {
			Expression flipMarker = newSymbol(FLIP_ID_PREFIX+flipId);			
			if (!flipMarkerToId.containsKey(flipMarker) && Expressions.isSubExpressionOf(flipMarker, body)) {
				flipMarkerToId.put(flipMarker, flipId);
				flipMarkerToFlipValuesIdx.put(flipMarker, flipMarkerToFlipValuesIdx.size());
				flipValues.add(trueFalseValues);
			}
		}

		if (flipValues.size() == 0) {
			Expression potentialRule = createPotentialRule(randomVariable, deterministicChurch2HOGM(body, paramVarNames, processForRV), Expressions.ONE, Expressions.ZERO);
			result = rNormalize.rewrite(potentialRule, processForRV);
		}
		else {
			// H <- empty list
			List<Expression> h = new ArrayList<Expression>();
			// for all assignments of FlipsValues to Flips do
			CartesianProductEnumeration<Boolean> cpe = new CartesianProductEnumeration<Boolean>(flipValues);
			while (cpe.hasMoreElements()) {
				final List<Boolean> values = cpe.nextElement();
				
				// caseC <- subsitute FlipsValues for Flips in body
				Expression caseC = body.replaceAllOccurrences(new AbstractReplacementFunctionWithContextuallyUpdatedProcess() {					
					@Override
					public Expression apply(Expression expression, RewritingProcess process) {
						Expression result = expression;
						if (Expressions.isSymbol(expression)) {
							Integer idx = flipMarkerToFlipValuesIdx.get(expression);
							if (idx != null) {
								result = values.get(idx) ? Expressions.TRUE : Expressions.FALSE;
							}
						}
						return result;
					}
				}, LBPFactory.newLBPProcess());
				// caseH <- deterministicChurch2HOGM(caseC)
				Expression caseH = deterministicChurch2HOGM(caseC, paramVarNames, processForRV);				
				
				// Calculate q
				Rational q   = Rational.ONE;
				for (Map.Entry<Expression, Integer> flipMarkerToIdEntry : flipMarkerToId.entrySet()) {
					Rational pi = flipIdToValue.get(flipMarkerToIdEntry.getValue());
					if (!values.get(flipMarkerToFlipValuesIdx.get(flipMarkerToIdEntry.getKey()))) {
						pi = Rational.ONE.subtract(pi);
					}
					
					q  = q.multiply(pi);
				}
											
				h.add(createPotentialRule(randomVariable, caseH, Expressions.makeSymbol(q), Expressions.ZERO));				
			}
			Expression plusH = Plus.make(h);
			List<Expression> constants = new ArrayList<>(FormulaUtil.getConstants(plusH, processForRV));
			// Ensure we exclude known random variable names
			constants.removeAll(knownRandomVariableNames);
			// And also ensure we remove these known constants as well.
			constants.remove(Expressions.TRUE);
			constants.remove(Expressions.FALSE);
			if (constants.size() > 0) {
				knownConstants.addAll(constants);				
				Model model = Model.getRewritingProcessesModel(processForRV);
				Set<Expression> sortDeclarationExpressions = new LinkedHashSet<>();
				sortDeclarationExpressions.add(new SortDeclaration(Expressions.makeSymbol(CHURCH_VALUES_SORT), SortDeclaration.UNKNOWN_SIZE, 
						ExtensionalSet.makeUniSet(constants)).getSortDeclaration());
				Set<Expression> randomVariableDeclarationExpressions = new LinkedHashSet<>();
				model.getRandomVariableDeclarations().forEach(randomVariableDeclaration -> randomVariableDeclarationExpressions.add(randomVariableDeclaration.getRandomVariableDeclaration()));
				processForRV = Model.setKnownSortsAndRandomVariables(sortDeclarationExpressions, randomVariableDeclarationExpressions, processForRV);
			}
			
			result = SimplifyWithRelationsAtBottom.simplify(plusH, name, processForRV);
		}
		
		rules.add(result.toString());		
		
		return result;
	}
	
	protected Expression deterministicChurch2HOGM(Expression expr, final Map<Expression, Expression> paramVarNames, RewritingProcess processForRV) {	
		expr = expr.replaceAllOccurrences(new AbstractReplacementFunctionWithContextuallyUpdatedProcess() {					
			@Override
			public Expression apply(Expression expression, RewritingProcess process) {
				Expression result = expression;
				if (Expressions.isSymbol(expression)) {
					Expression varName = paramVarNames.get(expression);
					if (varName != null) {
						result = varName;
					}
				}
				return result;
			}
		}, LBPFactory.newLBPProcess());
		
		Expression result = rNormalize.rewrite(expr, processForRV);
		
		return result;
	}
	
	protected Expression createPotentialRule(Expression randomVariable, Expression caseH, Expression alpha, Expression beta) {		
		Expression condition = null;
		if (Expressions.TRUE.equals(caseH)) {
			condition = randomVariable;
		}
		else if (Expressions.FALSE.equals(caseH)) {
			condition = Not.make(randomVariable);
		}
		else {
			condition = Equality.make(randomVariable, caseH);
		}
		Expression result = IfThenElse.make(condition, alpha, beta);

		return result;
	}
	
	protected Expression newSymbol(String text) {
		// Remove quotes from around quoted strings
		if ((text.startsWith("'") && text.endsWith("'"))
				|| (text.startsWith("\"") && text.endsWith("\""))) {
			text = text.substring(1, text.length() - 1);
		}

		// Ensure escapes are applied.
		text = StringEscapeUtils.unescapeJava(text);

		if (!text.contains(" ")) {
			text = text.replaceAll("-", "_");
			text = ensureLegalRandomVariableName(new String(text));
		}

		Expression result = Expressions.makeSymbol(text);
		return result;
	}
	
	protected Expression newLogicalVariable(String logicalVariableName) {
		if (!logicalVariableName.substring(0, 1).toUpperCase().equals(logicalVariableName.substring(0, 1))) {
			logicalVariableName = logicalVariableName.substring(0, 1).toUpperCase() + (logicalVariableName.length() > 1 ? logicalVariableName.substring(1) : "");
		}
		Expression result = Expressions.makeSymbol(logicalVariableName);
		return result;
	}
	
	protected String ensureLegalRandomVariableName(String name) {
		String result = name.replaceAll("-", "_");
		if (name.length() > 0 && name.toUpperCase().substring(0, 1).equals(name.substring(0, 1))) {
			result = name.substring(0, 1).toLowerCase() + (name.length() > 1 ? name.substring(1) : "");
		}
		return result;
	}
}
