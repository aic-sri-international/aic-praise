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
import java.util.List;
import java.util.Map;

import org.antlr.v4.runtime.misc.NotNull;
import org.apache.commons.lang3.StringEscapeUtils;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.imports.church.antlr.ChurchBaseVisitor;
import com.sri.ai.praise.imports.church.antlr.ChurchParser;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.rules.RuleConverter;
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
	//
	private String                   churchProgramName = null;
	private String                   churchProgram     = null;
	private List<String>             randoms           = new ArrayList<String>();
	private List<String>             rules             = new ArrayList<String>();
	private List<Expression>         queries           = new ArrayList<Expression>();
	private Map<Integer, Rational>   flipIdToValue     = new LinkedHashMap<Integer, Rational>();
	private List<Expression>         lambdaParams      = new ArrayList<Expression>();
	private Rewriter                 rNormalize        = LBPFactory.newNormalize();
	
	public void setChurchProgramInformation(String name, String program) {
		churchProgramName = name;
		churchProgram     = program;
	}

	@Override 
	public synchronized Expression visitParse(@NotNull ChurchParser.ParseContext ctx) {		
		Expression result = null;
		
		// Clear down the working variables
		randoms.clear();
		rules.clear();
		queries.clear();
		flipIdToValue.clear();
		
		visitChildren(ctx);
		
		// Construct the HOGM		
		StringBuilder hogm = new StringBuilder();
		hogm.append("\n");
		hogm.append("sort "+CHURCH_VALUES_SORT+";\n\n");
		for (String rv : randoms) {
			hogm.append(rv+";\n");
		}
		hogm.append("\n");
		for (String r : rules) {		
			hogm.append(r+";\n");
		}

		Model m = RuleConverter.makeModel(churchProgramName, "\n"+churchProgram+"\n--->\n"+hogm.toString(), hogm.toString());
		
		result = Tuple.make(newSymbol(hogm.toString()),
							m.getModelDefinition(),
							ExtensionalSet.makeUniSet(queries));
		
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
		queries.add(result);
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
		Expression alternate  = visit(ctx.alternate());
		
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
		if (ctx.number() != null) {
			value = visit(ctx.number());
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
	
	public Expression visitProcedureCall(@NotNull ChurchParser.ProcedureCallContext ctx) { 
		Expression       operator = visit(ctx.operator());
		List<Expression> operands = new ArrayList<Expression>();
		
		if (ctx.operand() != null && ctx.operand().size() > 0) {
			for (int i = 0; i < ctx.operand().size(); i++) {
				operands.add(visit(ctx.operand(i)));			
			}
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
				rArgs.append(" X ");			
			}
			// Ensure name is upper cased
			String sArg = arg.toString();
			if (!sArg.substring(0, 1).toUpperCase().equals(sArg.substring(0, 1))) {
				sArg = sArg.substring(0, 1).toUpperCase() + (sArg.length() > 1 ? sArg.substring(1) : "");
			}
			rvArgs.add(newSymbol(sArg));
			params.set(cnt, newSymbol(sArg));
			paramVarNames.put(arg, params.get(cnt));
// TODO - anything better?			
			rArgs.append(CHURCH_VALUES_SORT);
			cnt++;
		}
		randoms.add("random "+name+":"+rArgs+" -> Boolean");
		RewritingProcess processForRV = LBPFactory.newLBPProcessWithHighLevelModel("random "+name+":"+rArgs+" -> Boolean;");		
		if (rvArgs.size() > 0) {
			processForRV = LPIUtil.extendContextualSymbolsWithFreeVariablesInferringDomainsFromUsageInRandomVariables(Tuple.make(rvArgs), processForRV);
			randomVariable = Expressions.apply(randomVariable, rvArgs);
		}
				
		if (flipIdToValue.size() == 0) {
			Expression potentialRule = createPotentialRule(randomVariable, deterministicChurch2HOGM(body, paramVarNames, processForRV), Expressions.ONE, Expressions.ZERO);
			result = rNormalize.rewrite(potentialRule, processForRV);
		}
		else {
			final List<List<Boolean>>       flipValues      = new ArrayList<List<Boolean>>();
			final List<Boolean>             trueFalseValues = new ArrayList<Boolean>();
			final Map<Expression, Integer>  flipMarkerToIdx = new HashMap<Expression, Integer>();
			// Flips <- array of flip applications in body
			trueFalseValues.add(Boolean.FALSE);
			trueFalseValues.add(Boolean.TRUE);
			for (Integer flipId : flipIdToValue.keySet()) {
				flipValues.add(trueFalseValues);
				flipMarkerToIdx.put(newSymbol(FLIP_ID_PREFIX+flipId), flipId);
			}
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
							Integer idx = flipMarkerToIdx.get(expression);
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
				Rational q = Rational.ONE;
				for (Map.Entry<Integer, Rational> flipEntry : flipIdToValue.entrySet()) {
					Rational pi = flipEntry.getValue();
					if (!values.get(flipEntry.getKey())) {
						pi = Rational.ONE.subtract(pi);
					}
					
					q  = q.multiply(pi);
				}
											
				h.add(createPotentialRule(randomVariable, caseH, Expressions.makeSymbol(q), Expressions.ZERO));				
			}		
			result = rNormalize.rewrite(Plus.make(h), processForRV);
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
		Expression condition = Equality.make(randomVariable, caseH);
		Expression result    = IfThenElse.make(condition, alpha, beta);

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

		text = new String(text);

		Expression result = Expressions.makeSymbol(text);
		return result;
	}
}
