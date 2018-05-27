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
package com.sri.ai.praise.core.model.classbased.core.hogm.validation;

import static com.sri.ai.expresso.helper.Expressions.makeTuple;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.getIndexToTypeMapWithDefaultNull;
import static com.sri.ai.util.Util.sameInstancesInSameIterableOrder;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.api.Tuple;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.CountingFormulaEquivalentExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSets;
import com.sri.ai.praise.core.model.classbased.core.hogm.components.HOGMConstantDeclaration;
import com.sri.ai.praise.core.model.classbased.core.hogm.components.HOGMModelConstants;
import com.sri.ai.praise.core.model.classbased.core.hogm.components.HOGMRandomVariableDeclaration;
import com.sri.ai.praise.core.model.classbased.core.hogm.components.HOGMSortDeclaration;
import com.sri.ai.praise.core.model.classbased.core.hogm.validation.HOGModelError.Type;
import com.sri.ai.util.base.Pair;

public class HOGMModelValidator {

	public enum TermCategoryType {
		BOOLEAN, NUMERIC, STRING, OTHER, INVALID
	}

	List<Expression> sortDeclarations           = new ArrayList<>();
	List<Expression> constantDeclarations       = new ArrayList<>();
	List<Expression> randomVariableDeclarations = new ArrayList<>();
	List<Expression> conditionedPotentials      = new ArrayList<>();
	//
	List<HOGModelError> errors = new ArrayList<>();
	//
	Set<String> booleanTypeFunctors = new LinkedHashSet<>(HOGMModelConstants.KNOWN_BOOLEAN_FUNCTORS);
	Set<String> numericTypeFunctors = new LinkedHashSet<>(HOGMModelConstants.KNOWN_NUMERIC_FUNCTORS);
	Set<String> stringTypeFunctors  = new LinkedHashSet<>();
	Set<String> otherTypeFunctors   = new LinkedHashSet<>();
	//
	Set<Expression>                                sortConstants = new LinkedHashSet<>();
	Map<Expression, HOGMSortDeclaration>           sorts         = new LinkedHashMap<>();
	Map<Expression, HOGMConstantDeclaration>           constants     = new LinkedHashMap<>();
	Map<Expression, HOGMRandomVariableDeclaration> randoms       = new LinkedHashMap<>();
	List<Expression> conditioned = new ArrayList<>();
	
	public HOGMModelValidator(List<StatementInfo> sortStatements, List<StatementInfo> constantStatements, List<StatementInfo> randomVariableStatements, List<StatementInfo> termStatements) {
		validateSortStatements(sortStatements);
		validateConstantStatements(constantStatements);
		validateRandomVariableStatements(randomVariableStatements);
		
		validateTermStatements(termStatements);	
		
		// Map validated values
		sortDeclarations.addAll(sorts.values().stream().map(sd -> sd.getSortDeclaration()).collect(Collectors.toList()));
		constantDeclarations.addAll(constants.values().stream().map(cd -> cd.getConstantDeclaration()).collect(Collectors.toList()));
		randomVariableDeclarations.addAll(randoms.values().stream().map(rd -> rd.getRandomVariableDeclaration()).collect(Collectors.toList()));
		conditionedPotentials.addAll(conditioned);

		if (!isValid()) {
			throw new HOGModelException("Invalid model", errors);
		}
	}
	
	boolean isValid() {
		return errors.size() == 0;
	}
	
	public Tuple getHOGMModelComponents() {
		Tuple result = 
				makeTuple(
						makeTuple(sortDeclarations), 
						makeTuple(constantDeclarations),
						makeTuple(randomVariableDeclarations), 
						makeTuple(conditionedPotentials));
		return result;
	}
	
	void validateSortStatements(List<StatementInfo> sortStatements) {
		// Pre-associated with the Boolean Sort
		sortConstants.add(Expressions.FALSE);
		sortConstants.add(Expressions.TRUE);
		
		sortStatements.forEach(sortStatement -> {
			if (Expressions.isStringLiteral(sortStatement.statement.get(0))) {
				newError(Type.SORT_NAME_CANNOT_BE_A_STRING_LITERAL, "", sortStatement);
			}
			else if (HOGMSortDeclaration.isNameOfInBuilt(sortStatement.statement.get(0))) {
				newError(Type.SORT_NAME_PREDEFINED, "", sortStatement);
			}
			else if (!validSortConstants(sortStatement)) {
				// method will have added errors specific to each constant
				// do nothing here as can't actually construct a sort declaration with invalid constants.
			}
			else if (!HOGMSortDeclaration.isSortDeclaration(sortStatement.statement)) {
				newError(Type.SORT_DECLARATION_IS_NOT_LEGAL, "", sortStatement);
			}
			else {
				HOGMSortDeclaration sortDeclaration = HOGMSortDeclaration.makeSortDeclaration(sortStatement.statement);
				if (sorts.containsKey(sortDeclaration.getName())) {
					newError(Type.SORT_NAME_NOT_UNIQUE, sortDeclaration.getName(), sortStatement);
				}
				else if (HOGMModelConstants.KNOWN_FUNCTORS.contains(functorName(sortDeclaration.getName()))) {
					newError(Type.SORT_NAME_SAME_AS_IN_BUILT_FUNCTOR, sortDeclaration.getName(), sortStatement);
				}
				else if (ForAll.LABEL.equals(functorName(sortDeclaration.getName())) || ThereExists.LABEL.equals(functorName(sortDeclaration.getName()))) {
					newError(Type.SORT_NAME_SAME_AS_QUANTIFIER, sortDeclaration.getName(), sortStatement);
				}
				else {
					sorts.put(sortDeclaration.getName(), sortDeclaration);
					sortDeclaration.getAssignedConstants().forEach(constant -> {
						if (sortConstants.contains(constant)) {
							newError(Type.SORT_CONSTANT_NAME_NOT_UNIQUE, constant, sortStatement);
						}
						sortConstants.add(constant);
					});
				}
			}
		});
	}
	
	boolean validSortConstants(StatementInfo sortStatement) {
		boolean result = true;	
		for (Expression constant : ExtensionalSets.getElements(sortStatement.statement.get(2))) {
			if (Expressions.isStringLiteral(constant)) {
				result = false;
				newError(Type.SORT_CONSTANT_NAME_CANNOT_BE_A_STRING_LITERAL, ""+constant, sortStatement);
			}
		}
		return result;
	}
	
	void validateConstantStatements(List<StatementInfo> constantStatements) {				
		constantStatements.forEach(constantStatement -> {
			if (Expressions.isStringLiteral(constantStatement.statement.get(0))) {
				newError(Type.CONSTANT_NAME_CANNOT_BE_A_STRING_LITERAL, "", constantStatement);
			}
			else if (!HOGMConstantDeclaration.isConstantDeclaration(constantStatement.statement)) {
				newError(Type.CONSTANT_DECLARATION_IS_NOT_LEGAL, "", constantStatement);
			}
			else {
				HOGMConstantDeclaration constantDeclaration = HOGMConstantDeclaration.makeConstantDeclaration(constantStatement.statement);
				if (constants.containsKey(constantDeclaration.getName())) {
					newError(Type.CONSTANT_NAME_NOT_UNIQUE, constantDeclaration.getName(), constantStatement);
				}
				else if (sortConstants.contains(constantDeclaration.getName())) {
					newError(Type.CONSTANT_NAME_SAME_AS_UNIQUE_CONSTANT, constantDeclaration.getName(), constantStatement);
				}
				else if (getSort(constantDeclaration.getName()) != null) {
					newError(Type.CONSTANT_NAME_SAME_AS_SORT, constantDeclaration.getName(), constantStatement);
				}
				else if (HOGMModelConstants.KNOWN_FUNCTORS.contains(functorName(constantDeclaration.getName()))) {
					newError(Type.CONSTANT_NAME_SAME_AS_IN_BUILT_FUNCTOR, constantDeclaration.getName(), constantStatement);
				}
				else if (ForAll.LABEL.equals(functorName(constantDeclaration.getName())) || ThereExists.LABEL.equals(functorName(constantDeclaration.getName()))) {
					newError(Type.CONSTANT_NAME_SAME_AS_QUANTIFIER, constantDeclaration.getName(), constantStatement);
				}
				else {
					constants.put(constantDeclaration.getName(), constantDeclaration);
					constantDeclaration.getParameterSorts().forEach(parameterSortName -> {
						if (getSort(parameterSortName) == null) {
							newError(Type.CONSTANT_SORT_ARGUMENT_NOT_DECLARED, parameterSortName, constantStatement);
						}
					});
					if (getSort(constantDeclaration.getRangeSort()) == null) {
						newError(Type.CONSTANT_SORT_ARGUMENT_NOT_DECLARED, constantDeclaration.getRangeSort(), constantStatement);
					}	
					trackCategoryType(constantDeclaration.getName(), constantDeclaration.getRangeSort());
				}
			}
		});
	}
	
	void validateRandomVariableStatements(List<StatementInfo> randomVariableStatements) {
		randomVariableStatements.forEach(rvStatement -> {
			if (Expressions.isStringLiteral(rvStatement.statement.get(0))) {
				newError(Type.RANDOM_VARIABLE_NAME_CANNOT_BE_A_STRING_LITERAL, "", rvStatement);
			}
			else if (!HOGMRandomVariableDeclaration.isRandomVariableDeclaration(rvStatement.statement)) {
				newError(Type.RANDOM_VARIABLE_IS_NOT_LEGAL, "", rvStatement);
			}
			else {
				HOGMRandomVariableDeclaration rvDeclaration = HOGMRandomVariableDeclaration.makeRandomVariableDeclaration(rvStatement.statement);
				if (randoms.containsKey(rvDeclaration.getName())) {
					newError(Type.RANDOM_VARIABLE_NAME_NOT_UNIQUE, rvDeclaration.getName(), rvStatement);
				}
				else if (sortConstants.contains(rvDeclaration.getName())) {
					newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_UNIQUE_CONSTANT, rvDeclaration.getName(), rvStatement);
				} 
				else if (getSort(rvDeclaration.getName()) != null) {
					newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_SORT, rvDeclaration.getName(), rvStatement);
				}
				else if (constants.containsKey(rvDeclaration.getName())) {
					newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_CONSTANT, rvDeclaration.getName(), rvStatement);
				}
				else if (HOGMModelConstants.KNOWN_FUNCTORS.contains(functorName(rvDeclaration.getName()))) {
					newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_IN_BUILT_FUNCTOR, rvDeclaration.getName(), rvStatement);
				}
				else if (ForAll.LABEL.equals(functorName(rvDeclaration.getName())) || ThereExists.LABEL.equals(functorName(rvDeclaration.getName()))) {
					newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_QUANTIFIER, rvDeclaration.getName(), rvStatement);
				}
				else {
					randoms.put(rvDeclaration.getName(), rvDeclaration);
					rvDeclaration.getParameterSorts().forEach(parameterSortName -> {
						if (getSort(parameterSortName) == null) {
							newError(Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED, parameterSortName, rvStatement);
						}
					});
					if (getSort(rvDeclaration.getRangeSort()) == null) {
						newError(Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED, rvDeclaration.getRangeSort(), rvStatement);
					}	
					trackCategoryType(rvDeclaration.getName(), rvDeclaration.getRangeSort());
				}
			}
		});
	}
	
	void trackCategoryType(Expression functorName, Expression sortName) {
		// Track the category type of the functor
		if (sortName.equals(HOGMSortDeclaration.IN_BUILT_BOOLEAN.getName())) {
			booleanTypeFunctors.add(functorName(functorName));
		}
		else if (HOGMSortDeclaration.isNameOfInBuiltNumberType(sortName)) {
			numericTypeFunctors.add(functorName(functorName));
		}
		else if (sortName.equals(HOGMSortDeclaration.IN_BUILT_STRING.getName())) {
			stringTypeFunctors.add(functorName(functorName));
		}
		else {
			otherTypeFunctors.add(functorName(functorName));
		}
	}
	
	void validateTermStatements(List<StatementInfo> termStatements) {
		termStatements.forEach(termStatement -> {
			termStatement = updateQuantifierExpressionsAsNeeded(termStatement);
			
			// Ensure all functors are known and have correct arity
			validateFunctorsAndArguments(termStatement);
			
			Expression statement = termStatement.statement;
			// Determine type
			HOGMModelValidator.TermCategoryType termType = determineTermCategoryType(statement);
			if (IfThenElse.isIfThenElse(statement)) {
				Expression conditionedPotential = statement;
				if (termType != TermCategoryType.NUMERIC) {
					conditionedPotential = attemptMakeRule(statement);
				}
				
				if (conditionedPotential == null) {
					newError(Type.TERM_CONDITONAL_STATEMENT_MUST_BE_OF_TYPE_NUMERIC, "", termStatement);
				}
				else {
					conditioned.add(conditionedPotential);
				}
			} // A numeric rule (e.g. 0.1)
			else if (termType == TermCategoryType.NUMERIC) {
				conditioned.add(statement);
			} 
			else if (termType != TermCategoryType.BOOLEAN) {
				newError(Type.TERM_NON_CONDITIONAL_STATEMENT_MUST_BE_OF_TYPE_BOOLEAN, "", termStatement);
			}
			else {
				conditioned.add(IfThenElse.make(statement, Expressions.ONE, Expressions.ZERO));
			}
		});
	}
	
	StatementInfo updateQuantifierExpressionsAsNeeded(StatementInfo termStatement) {
		
		Expression updatedQuantifiers = updateQuantifiers(termStatement.statement, constants);
		
		StatementInfo result;
		if (updatedQuantifiers == termStatement.statement) {
			result = termStatement;
		}
		else {
			result = new StatementInfo(updatedQuantifiers, termStatement.sourceText, termStatement.line, termStatement.startIndex, termStatement.endIndex);
		}
		
		return result;
	}
	
	Expression updateQuantifiers(Expression expr, Map<Expression, HOGMConstantDeclaration> currentScope) {
		Expression result = expr;
		if (isQuantifiedExpression(expr)) {				
			Expression bodyExpression = getQuantifiedExpressionBody(expr);
			if (bodyExpression != null) {
				Map<Expression, HOGMConstantDeclaration> quantifierScope = new LinkedHashMap<>(currentScope);
				quantifierScope.putAll(getQuantifiedExpressionScope(expr));
				Expression updatedBodyExpression = updateQuantifiers(bodyExpression, quantifierScope);
				
				HOGMModelValidator.TermCategoryType updatedBodyTermCategoryType = determineTermCategoryType(updatedBodyExpression);
				if (updatedBodyTermCategoryType == TermCategoryType.NUMERIC) {
					Expression intensionalMultiSet = IntensionalSet.intensionalMultiSet(((QuantifiedExpression)expr).getIndexExpressions(), updatedBodyExpression, Expressions.TRUE);
					result = Expressions.apply(FunctorConstants.PRODUCT, intensionalMultiSet);
				}					
				else if (bodyExpression != updatedBodyExpression) 
				{	
					if (ForAll.isForAll(expr)) {
						result = ForAll.make(ForAll.getIndexExpression(expr), updatedBodyExpression);
					}
					else { // Otherwise is existential
						result = ThereExists.make(ThereExists.getIndexExpression(expr), updatedBodyExpression);
					}
				}
			}
		}
		else if (Expressions.isFunctionApplicationWithArguments(expr)) {
			List<Expression> updatedArgs = expr.getArguments().stream()
					.map(arg -> updateQuantifiers(arg, currentScope))
					.collect(Collectors.toList());
			
			if (!sameInstancesInSameIterableOrder(expr.getArguments(), updatedArgs)) {
				result = Expressions.apply(expr.getFunctor(), updatedArgs);
			}
			
			HOGMModelValidator.TermCategoryType resultTermCategoryType = determineTermCategoryType(result);
			if (resultTermCategoryType == TermCategoryType.INVALID) {
				if (IfThenElse.isIfThenElse(result)) {
					Expression asRule = attemptMakeRule(result);
					if (asRule != null) {
						result = asRule;
					}
				}
			}
		}
		
		return result;
	}
	
	HOGMModelValidator.TermCategoryType determineTermCategoryType(Expression expr) {
		HOGMModelValidator.TermCategoryType result = TermCategoryType.INVALID;
		
		HOGMSortDeclaration sortType = determineSortType(expr);
		if (sortType != null) {
			if (sortType == HOGMSortDeclaration.IN_BUILT_BOOLEAN) {
				result = TermCategoryType.BOOLEAN;
			}
			else if (sortType == HOGMSortDeclaration.IN_BUILT_INTEGER || sortType == HOGMSortDeclaration.IN_BUILT_REAL) {
				result = TermCategoryType.NUMERIC;
			}
			else if (sortType == HOGMSortDeclaration.IN_BUILT_STRING) {
				result = TermCategoryType.STRING;
			}
			else  {
				result = TermCategoryType.OTHER;
			}
		}
		
		return result;
	}
	
	HOGMSortDeclaration determineSortType(Expression expr) {
		HOGMSortDeclaration result = determineSortType(expr, constants);
		return result;
	}
	
	HOGMSortDeclaration determineSortType(Expression expr, Map<Expression, HOGMConstantDeclaration> scopedConstants) {
		HOGMSortDeclaration result = null;
		
		if (IfThenElse.isIfThenElse(expr)) {
			HOGMSortDeclaration condition  = determineSortType(IfThenElse.condition(expr), scopedConstants);
			HOGMSortDeclaration thenBranch = determineSortType(IfThenElse.thenBranch(expr), scopedConstants);
			HOGMSortDeclaration elseBranch = determineSortType(IfThenElse.elseBranch(expr), scopedConstants);
			// Ensure legal 
			if (condition == HOGMSortDeclaration.IN_BUILT_BOOLEAN) {
				if (thenBranch == elseBranch) {
					result = thenBranch; // same as elseBranch
				}
				else if ((thenBranch == HOGMSortDeclaration.IN_BUILT_INTEGER || thenBranch == HOGMSortDeclaration.IN_BUILT_REAL) &&
						 (elseBranch == HOGMSortDeclaration.IN_BUILT_INTEGER || elseBranch == HOGMSortDeclaration.IN_BUILT_REAL)) {
					result = HOGMSortDeclaration.IN_BUILT_REAL; // subsumes Integer branch
				}
				else {
					result = null; // Don't know
				}
			}
			else {
				result = null; // Don't know
			}
		}
		else {
			Expression functor = expr.getFunctor();
			if (functor == null) {
				if (Expressions.FALSE.equals(expr) || Expressions.TRUE.equals(expr)) {
					result = HOGMSortDeclaration.IN_BUILT_BOOLEAN;
				}
				else if (ForAll.isForAll(expr) || ThereExists.isThereExists(expr)) { // NOTE: quantifiers are not functions
					result = HOGMSortDeclaration.IN_BUILT_BOOLEAN;
				}
				else if (CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression(expr)) { // NOTE: counting formulas are not functions which is a subset of counting formula equivalent expressions
					result = HOGMSortDeclaration.IN_BUILT_INTEGER;
				}
				else if (Sets.isIntensionalMultiSet(expr)) {
					Map<Expression, HOGMConstantDeclaration> intensionalMultiSetScope = new LinkedHashMap<>(scopedConstants);
					intensionalMultiSetScope.putAll(getQuantifiedExpressionScope(expr));
					result = determineSortType(((IntensionalSet)expr).getHead(), intensionalMultiSetScope);
				}
				else if (Expressions.isNumber(expr)) {
					if (expr.rationalValue().isInteger()) {
						result = HOGMSortDeclaration.IN_BUILT_INTEGER;
					}
					else {
						result = HOGMSortDeclaration.IN_BUILT_REAL;
					}
				}
				else if (Expressions.isStringLiteral(expr)) {
					result = HOGMSortDeclaration.IN_BUILT_STRING;
				}
				else {
					if (isDeclaredConstantFunctor(expr.getFunctorOrSymbol(), scopedConstants)) {
						HOGMConstantDeclaration constantDeclaration = scopedConstants.get(expr.getFunctorOrSymbol());
						result = getSort(constantDeclaration.getRangeSort());
					}
					else if (isDeclaredRandomFunctor(expr.getFunctorOrSymbol())) {
						HOGMRandomVariableDeclaration rvDeclaration = randoms.get(expr.getFunctorOrSymbol());
						result = getSort(rvDeclaration.getRangeSort());
					}
					else if ((result = getSort(expr)) != null) {
						// has been assigned.
					}
					else if (sortConstants.contains(expr)) {
						for (HOGMSortDeclaration sort : sorts.values()) {
							// Have mapped the unique constant sort.
							if (ExtensionalSets.getElements(sort.getConstants()).contains(expr)) {
								result = sort;
								break;
							}
						}
					}
					else {
						// Don't know
					}
				}
			}
			else {
				String functorName = functorName(functor);
				if (booleanTypeFunctors.contains(functorName)) {
					result = HOGMSortDeclaration.IN_BUILT_BOOLEAN;
				}
				else if (numericTypeFunctors.contains(functorName)) {
					result = HOGMSortDeclaration.IN_BUILT_INTEGER;
					if (!FunctorConstants.CARDINALITY.equals(functorName)) {
						if (FunctorConstants.PRODUCT.equals(functorName)) {
							result = determineSortType(expr.get(0), scopedConstants);
						}
						else {
							for (Expression arg : expr.getArguments()) {
								if (Expressions.isNumber(arg)) {
									if (!arg.rationalValue().isInteger()) {
										result = HOGMSortDeclaration.IN_BUILT_REAL;
										break;
									}
								}
								else {
									result = determineSortType(arg, scopedConstants);
									if (result == HOGMSortDeclaration.IN_BUILT_REAL) {
										break; 
									}
									else if (result != HOGMSortDeclaration.IN_BUILT_INTEGER) {
										// Something wrong as the argument sort is not numeric
										result = null;
										break;
									}
								}
							}
						}
					}
				}
				else if (isDeclaredConstantFunctor(functor, scopedConstants)) {
					HOGMConstantDeclaration constantDeclaration = scopedConstants.get(functor);
					result = getSort(constantDeclaration.getRangeSort());
				}
				else if (isDeclaredRandomFunctor(functor)) {
					HOGMRandomVariableDeclaration rvDeclaration = randoms.get(functor);
					result = getSort(rvDeclaration.getRangeSort());
				}
				else {
					result = null; // Don't know
				}
			}
		}
		
		return result;
	}
	
	private Expression attemptMakeRule(Expression conditional) {
		Expression result = null;
		
		HOGMModelValidator.TermCategoryType conditionType = determineTermCategoryType(IfThenElse.condition(conditional));
		if (conditionType == TermCategoryType.BOOLEAN) {
			Expression thenBranch;
			if ((thenBranch = attemptMakeNumeric(IfThenElse.thenBranch(conditional))) != null) {
				Expression elseBranch;
				if ((elseBranch = attemptMakeNumeric(IfThenElse.elseBranch(conditional))) != null) {
					result = IfThenElse.make(IfThenElse.condition(conditional), thenBranch, elseBranch);
				}
			}
			
		}
		
		return result;
	}
	
	private Expression attemptMakeNumeric(Expression expr) {
		Expression result = null;
		
		HOGMModelValidator.TermCategoryType exprType = determineTermCategoryType(expr);
		if (exprType == TermCategoryType.NUMERIC) {
			result = expr; // Already numeric
		}
		else if (IfThenElse.isIfThenElse(expr)) {
			result = attemptMakeRule(expr);
		}
		else if (exprType == TermCategoryType.BOOLEAN) {
			result = IfThenElse.make(expr, Expressions.ONE, Expressions.ZERO);
		}
		
		return result;
	}
	
	void validateFunctorsAndArguments(StatementInfo termStatement) {
		// Ensure constant functions have the correct arity and their arguments are of the correct type
		List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> constantFunctionsWithScope = getConstantFunctionsWithScope(termStatement.statement);
		constantFunctionsWithScope.forEach(constantFunctionAndScope -> {
			Expression                           constantFunction = constantFunctionAndScope.first;
			Map<Expression, HOGMConstantDeclaration> scopedConstants  = constantFunctionAndScope.second;
			if (!isValidDeclaredConstantFunctorArity(constantFunction, scopedConstants)) {
				newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, constantFunction, termStatement);
			}
			else {
				HOGMConstantDeclaration constantDeclaration = scopedConstants.get(constantFunction.getFunctorOrSymbol());
				for (int i = 0; i < constantFunction.numberOfArguments(); i++) {
					Expression arg = constantFunction.get(i);
					if (isUnknownConstant(arg, scopedConstants)) {								
						newError(Type.TERM_CONSTANT_NOT_DEFINED, arg, termStatement);
					}
					else {
						if (getSort(constantDeclaration.getParameterSorts().get(i)) != determineSortType(arg, scopedConstants)) {
							newError(Type.CONSTANT_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
						}
					}
				}
			}
		});
					
		// Ensure random functions have the correct arity and their arguments are of the correct type
		List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> randomFunctionsWithScope = getRandomFunctionsWithScope(termStatement.statement);
		randomFunctionsWithScope.forEach(randomFunctionAndScope -> {
			Expression                           randomFunction  = randomFunctionAndScope.first;
			Map<Expression, HOGMConstantDeclaration> scopedConstants = randomFunctionAndScope.second;
			if (!isValidDeclaredRandomFunctorArity(randomFunction)) {
				newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, randomFunction, termStatement);
			}
			else {
				HOGMRandomVariableDeclaration rvDeclaration = randoms.get(randomFunction.getFunctorOrSymbol());
				for (int i = 0; i < randomFunction.numberOfArguments(); i++) {
					Expression arg = randomFunction.get(i);
					if (isUnknownConstant(arg, scopedConstants)) {							
						newError(Type.TERM_CONSTANT_NOT_DEFINED, arg, termStatement);
					}
					else {
						if (getSort(rvDeclaration.getParameterSorts().get(i)) != determineSortType(arg, scopedConstants)) {
							newError(Type.RANDOM_VARIABLE_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
						}
					}
				}
			}
		});
		
					
		// All of these should belong to the known set of functors			
		List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> nonConstantAndRandomFunctionsWithScope = getNonConstantRandomFunctionsWithScope(termStatement.statement);
		
		nonConstantAndRandomFunctionsWithScope.forEach(x -> checkNonConstantAndRandomFunctionWithScope(x, termStatement));
		
		checkQuantifiedExpressions(termStatement);			
	}

	private void checkNonConstantAndRandomFunctionWithScope(Pair<Expression, Map<Expression, HOGMConstantDeclaration>> nonConstantAndRandomFunctionWithScope, StatementInfo termStatement) {
		Expression                           nonConstantAndRandomFunction = nonConstantAndRandomFunctionWithScope.first;
		Map<Expression, HOGMConstantDeclaration> scopedConstants              = nonConstantAndRandomFunctionWithScope.second;
		
		if (isKnownFunctor(nonConstantAndRandomFunction.getFunctor())) {
			checkDeclaredNonConstantAndRandomFunction(termStatement, nonConstantAndRandomFunction, scopedConstants);
		}
		else {					
			newError(Type.TERM_TYPE_OF_FUNCTOR_NOT_DECLARED, nonConstantAndRandomFunction.getFunctor(), termStatement);
		}
	}

	private void checkDeclaredNonConstantAndRandomFunction(StatementInfo termStatement, Expression nonConstantAndRandomFunction, Map<Expression, HOGMConstantDeclaration> scopedConstants) {
		String functorName = functorName(nonConstantAndRandomFunction.getFunctor());
		if (!isValidKnownFunctorArity(nonConstantAndRandomFunction)) {
			newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, "'"+nonConstantAndRandomFunction+"'", termStatement);
		}
		else {
			checkNonConstantAndRandomFunctionWithCorrectArity(termStatement, nonConstantAndRandomFunction, scopedConstants, functorName);
		}
	}

	private void checkNonConstantAndRandomFunctionWithCorrectArity(StatementInfo termStatement, Expression nonConstantAndRandomFunction, Map<Expression, HOGMConstantDeclaration> scopedConstants, String functorName) {
		Set<HOGMSortDeclaration> argSorts = new HashSet<>();
		for (int i = 0; i < nonConstantAndRandomFunction.numberOfArguments(); i++) {
			Expression arg = nonConstantAndRandomFunction.get(i);
			checkAndCollectArgSorts(arg, i, termStatement, nonConstantAndRandomFunction, scopedConstants, functorName, argSorts);
		}
		checkThatTypesOfArgumentsAllMatch(argSorts, termStatement, nonConstantAndRandomFunction);
	}

	private void checkAndCollectArgSorts(Expression arg, int i, StatementInfo termStatement, Expression nonConstantAndRandomFunction, Map<Expression, HOGMConstantDeclaration> scopedConstants, String functorName, Set<HOGMSortDeclaration> argSorts) {
		if (isUndefinedConstantTerm(arg, scopedConstants)) {									
			newError(Type.TERM_CONSTANT_NOT_DEFINED, arg, termStatement);
		}
		else {
			checkThatAllArgumentsAreOfTheSameType(arg, termStatement, nonConstantAndRandomFunction, scopedConstants, functorName, argSorts, i);
		}
	}

	private void checkThatAllArgumentsAreOfTheSameType(Expression arg, StatementInfo termStatement, Expression nonConstantAndRandomFunction, Map<Expression, HOGMConstantDeclaration> scopedConstants, String functorName, Set<HOGMSortDeclaration> argSorts, int i) {
		HOGMSortDeclaration sortType = determineSortType(arg, scopedConstants);
		if (sortType == null) {
			newError(Type.TERM_SORT_CANNOT_BE_DETERMINED, arg, termStatement);
		}
		if (IfThenElse.isIfThenElse(nonConstantAndRandomFunction)) {
			checkSortAgreementForIfThenElse(termStatement, argSorts, i, arg, sortType);
		}
		else if (isEqualityOrDisequality(nonConstantAndRandomFunction)) {
			checkSortAgreementForEqualityOrDisequalityFunctor(argSorts, sortType);
		}
		else if (isNumericComparisonFunctor(functorName)) {
			checkSortAgreementForNumericComparisonFunctor(termStatement, arg, sortType);	
		}
		else if (booleanTypeFunctors.contains(functorName)) {
			checkSortAgreementForBooleanFunctor(termStatement, arg, sortType);
		}
		else if (numericTypeFunctors.contains(functorName)) {
			checkSortAgreementForNumericFunctor(termStatement, functorName, arg, sortType);
		}
		else {
			newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
		}
	}

	private boolean isUndefinedConstantTerm(Expression arg, Map<Expression, HOGMConstantDeclaration> scopedConstants) {
		return !isDeclaredConstantFunctor(arg.getFunctorOrSymbol(), scopedConstants) &&
			!isDeclaredRandomFunctor(arg.getFunctorOrSymbol()) &&
			isUnknownConstant(arg, scopedConstants);
	}

	private void checkSortAgreementForIfThenElse(StatementInfo termStatement, Set<HOGMSortDeclaration> argSorts, int i, Expression arg, HOGMSortDeclaration sortType) {
		if (i == 0) {
			checkSortAgreementForBooleanFunctor(termStatement, arg, sortType);
		}
		else {
			// otherwise the branches sorts must match
			argSorts.add(sortType);
		}
	}

	private boolean isEqualityOrDisequality(Expression nonConstantAndRandomFunction) {
		return Equality.isEquality(nonConstantAndRandomFunction) || Disequality.isDisequality(nonConstantAndRandomFunction);
	}

	private void checkSortAgreementForEqualityOrDisequalityFunctor(Set<HOGMSortDeclaration> argSorts, HOGMSortDeclaration sortType) {
		// For equalities the types must match up
		if (sortType != null) {
			argSorts.add(sortType);
		}
	}

	private void checkSortAgreementForNumericComparisonFunctor(StatementInfo termStatement, Expression arg, HOGMSortDeclaration sortType) {
		if (sortType != HOGMSortDeclaration.IN_BUILT_INTEGER && sortType != HOGMSortDeclaration.IN_BUILT_REAL) {
			newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
		}
	}

	private boolean isNumericComparisonFunctor(String functorName) {
		return functorName.equals(FunctorConstants.LESS_THAN) ||
				 functorName.equals(FunctorConstants.LESS_THAN_OR_EQUAL_TO) ||
				 functorName.equals(FunctorConstants.GREATER_THAN) ||
				 functorName.equals(FunctorConstants.GREATER_THAN_OR_EQUAL_TO);
	}

	private void checkSortAgreementForBooleanFunctor(StatementInfo termStatement, Expression arg, HOGMSortDeclaration sortType) {
		if (sortType != HOGMSortDeclaration.IN_BUILT_BOOLEAN) {
			newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
		}
	}

	private void checkSortAgreementForNumericFunctor(StatementInfo termStatement, String functorName, Expression arg, HOGMSortDeclaration sortType) {
		boolean sortTypeIsNonNumeric = sortType != HOGMSortDeclaration.IN_BUILT_INTEGER && sortType != HOGMSortDeclaration.IN_BUILT_REAL;
		boolean functionIsNotCardinality = !functorName.equals(FunctorConstants.CARDINALITY);
		if (sortTypeIsNonNumeric && functionIsNotCardinality) {
			newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
		}
	}

	private void checkThatTypesOfArgumentsAllMatch(Set<HOGMSortDeclaration> argSorts, StatementInfo termStatement, Expression nonConstantAndRandomFunction) {
		if (IfThenElse.isIfThenElse(nonConstantAndRandomFunction) || Equality.isEquality(nonConstantAndRandomFunction) || Disequality.isDisequality(nonConstantAndRandomFunction)) {
			boolean thereAreMultipleSortsAndNotAllAreNumeric = argSorts.size() != 1 && !argSorts.equals(HOGMSortDeclaration.IN_BUILT_NUMERIC_SORTS);
			if (thereAreMultipleSortsAndNotAllAreNumeric) {	
				if (isNotAConditionalRule(nonConstantAndRandomFunction)) {
					newError(Type.TERM_ARGUMENTS_MUST_ALL_BE_OF_THE_SAME_COMPATIBLE_TYPE, nonConstantAndRandomFunction.toString() + " which has argument types " + argSorts.toString(), termStatement);
				}
			}
		}
	}

	private boolean isNotAConditionalRule(Expression nonConstantAndRandomFunction) {
		boolean isAConditionalRule = isIfThenElse(nonConstantAndRandomFunction) && attemptMakeRule(nonConstantAndRandomFunction) != null;
		boolean result = !isAConditionalRule;
		return result;
	}

	private void checkQuantifiedExpressions(StatementInfo termStatement) {
		List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> quantifiedExpressionsWithScope = getQuantifiedExpressionsWithScope(termStatement.statement);
		quantifiedExpressionsWithScope.forEach(qAndS -> checkQuantifiedExpressionWithScope(termStatement, qAndS));
	}

	private void checkQuantifiedExpressionWithScope(StatementInfo termStatement, Pair<Expression, Map<Expression, HOGMConstantDeclaration>> quantifiedExpressionWithScope) {
		QuantifiedExpression quantifiedExpression = (QuantifiedExpression) quantifiedExpressionWithScope.first;
		Map<Expression, HOGMConstantDeclaration> scopedConstants = quantifiedExpressionWithScope.second;
		checkIfIndexSortsAreDefined(termStatement, quantifiedExpression);
		checkIfBodyIsBoolean(termStatement, quantifiedExpression, scopedConstants);
	}

	private void checkIfIndexSortsAreDefined(StatementInfo termStatement, QuantifiedExpression quantifiedExpression) {
		Map<Expression, Expression> indexToTypeMap = getIndexToTypeMap(quantifiedExpression);
		indexToTypeMap.forEach((name, type) -> checkIfIndexSortIsDefined(termStatement, name, type));
	}

	private Map<Expression, Expression> getIndexToTypeMap(QuantifiedExpression quantifiedExpression) {
		IndexExpressionsSet indexExpressionSet = quantifiedExpression.getIndexExpressions();
		Map<Expression, Expression> indexToTypeMap = getIndexToTypeMapWithDefaultNull(indexExpressionSet);
		return indexToTypeMap;
	}

	private void checkIfIndexSortIsDefined(StatementInfo termStatement, Expression name, Expression type) {
		HOGMSortDeclaration localSort = getSort(type);
		if (localSort == null) {
			newError(Type.TERM_SORT_CANNOT_BE_DETERMINED, name, termStatement);
		}
	}

	private void checkIfBodyIsBoolean(StatementInfo termStatement, QuantifiedExpression quantifiedExpression, Map<Expression, HOGMConstantDeclaration> scopedConstants) {
		Expression body = getQuantifiedExpressionBody(quantifiedExpression);
		if (body != null && determineSortType(body, scopedConstants) != HOGMSortDeclaration.IN_BUILT_BOOLEAN) {
			newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, body, termStatement);
		}
	}
	
	List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> getRandomFunctionsWithScope(Expression expr) {
		List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> result = new ArrayList<>();
		getRandomFunctions(expr, result, constants);
		return result;
	}
	
	void getRandomFunctions(Expression expr, List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> randomFunctionsWithScope, Map<Expression, HOGMConstantDeclaration> currentScope) {
		if (this.isDeclaredRandomFunctor(expr.getFunctorOrSymbol())) {
			randomFunctionsWithScope.add(new Pair<>(expr, currentScope));
		}
		
		if (Expressions.isFunctionApplicationWithArguments(expr)) {
			expr.getArguments().forEach(arg -> getRandomFunctions(arg, randomFunctionsWithScope, currentScope));
		}
		else if (isQuantifiedExpression(expr)) {
			Map<Expression, HOGMConstantDeclaration> quantifierScope = new LinkedHashMap<>(currentScope);
			quantifierScope.putAll(getQuantifiedExpressionScope(expr));
			
			Expression bodyExpression = getQuantifiedExpressionBody(expr);
			if (bodyExpression != null) {
				getRandomFunctions(bodyExpression, randomFunctionsWithScope, quantifierScope);
			}
		}
	}
	
	List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> getConstantFunctionsWithScope(Expression expr) {
		List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> result = new ArrayList<>();
		getConstantFunctions(expr, result, constants);
		return result;
	}
	
	void getConstantFunctions(Expression expr, List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> constantFunctionsWithScope, Map<Expression, HOGMConstantDeclaration> currentScope) {
		if (this.isDeclaredConstantFunctor(expr.getFunctorOrSymbol(), currentScope)) {
			constantFunctionsWithScope.add(new Pair<>(expr, currentScope));
		}
		
		if (Expressions.isFunctionApplicationWithArguments(expr)) {
			expr.getArguments().forEach(arg -> getConstantFunctions(arg, constantFunctionsWithScope, currentScope));
		}
		else if (isQuantifiedExpression(expr)) {
			Map<Expression, HOGMConstantDeclaration> quantifierScope = new LinkedHashMap<>(currentScope);
			quantifierScope.putAll(getQuantifiedExpressionScope(expr));
			
			Expression bodyExpression = getQuantifiedExpressionBody(expr);
			if (bodyExpression != null) {
				getConstantFunctions(bodyExpression, constantFunctionsWithScope, quantifierScope);
			}
		}
	}

	List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> getNonConstantRandomFunctionsWithScope(Expression expr) {
		List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> result = new ArrayList<>();
		getNonConstantRandomFunctions(expr, result, constants);
		return result;
	}
	
	void getNonConstantRandomFunctions(Expression expr, List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> nonConstantRandomFunctionsWithScope, Map<Expression, HOGMConstantDeclaration> currentScope) {
		if (Expressions.isFunctionApplicationWithArguments(expr) &&
			!isDeclaredConstantFunctor(expr.getFunctorOrSymbol(), currentScope) &&
			!isDeclaredRandomFunctor(expr.getFunctorOrSymbol())) {
			nonConstantRandomFunctionsWithScope.add(new Pair<>(expr, currentScope));
		}
		
		if (Expressions.isFunctionApplicationWithArguments(expr)) {
			expr.getArguments().forEach(arg -> getNonConstantRandomFunctions(arg, nonConstantRandomFunctionsWithScope, currentScope));
		}
		else if (isQuantifiedExpression(expr)) {
			Map<Expression, HOGMConstantDeclaration> quantifierScope = new LinkedHashMap<>(currentScope);
			quantifierScope.putAll(getQuantifiedExpressionScope(expr));
			
			Expression bodyExpression = getQuantifiedExpressionBody(expr);
			if (bodyExpression != null) {
				getNonConstantRandomFunctions(bodyExpression, nonConstantRandomFunctionsWithScope, quantifierScope);
			}
		}
	}
	
	List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> getQuantifiedExpressionsWithScope(Expression expr) {
		List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> result = new ArrayList<>();
		getQuantifiers(expr, result, constants);
		return result;
	}
	
	void getQuantifiers(Expression expr, List<Pair<Expression, Map<Expression, HOGMConstantDeclaration>>> quantifiersWithScope, Map<Expression, HOGMConstantDeclaration> currentScope) {
		if (isQuantifiedExpression(expr)) {
			Map<Expression, HOGMConstantDeclaration> quantifierScope = new LinkedHashMap<>(currentScope);
			quantifierScope.putAll(getQuantifiedExpressionScope(expr));
			
			quantifiersWithScope.add(new Pair<>(expr, quantifierScope));
			
			Expression bodyExpression = getQuantifiedExpressionBody(expr);
			if (bodyExpression != null) {
				getQuantifiers(bodyExpression, quantifiersWithScope, quantifierScope);
			}
		}
		
		if (Expressions.isFunctionApplicationWithArguments(expr)) {
			expr.getArguments().forEach(arg -> getQuantifiers(arg, quantifiersWithScope, currentScope));
		}
	}
	
	public boolean isQuantifiedExpression(Expression expression) {
		boolean result = false;
		
		if (expression instanceof QuantifiedExpression) {
			result = true;
		}
		
		return result;
	}
	
	public Map<Expression, HOGMConstantDeclaration> getQuantifiedExpressionScope(Expression quantifiedExpression) {
		Map<Expression, HOGMConstantDeclaration> result = new LinkedHashMap<>();
		
		IndexExpressionsSet indexExpressionSet = ((QuantifiedExpression)quantifiedExpression).getIndexExpressions();
		IndexExpressions.getIndexToTypeMapWithDefaultNull(indexExpressionSet).forEach((name, type) -> {
			HOGMSortDeclaration localSort = getSort(type);
			if (localSort != null) {
				result.put(name, new HOGMConstantDeclaration(name, Expressions.ZERO, localSort.getName()));
			}
		});
		
		return result;
	}
	
	public Expression getQuantifiedExpressionBody(Expression quantifiedExpression) {
		Expression result = null;
		
		if (quantifiedExpression instanceof QuantifiedExpressionWithABody) {
			result = ((QuantifiedExpressionWithABody)quantifiedExpression).getBody();
		}
		
		return result;
	}
	
	boolean isUnknownConstant(Expression expr, Map<Expression, HOGMConstantDeclaration> scopedConstants) {
		boolean result = Expressions.isSymbol(expr) &&
						!Expressions.isNumber(expr) &&
						!isQuantifiedExpression(expr) &&
						!scopedConstants.containsKey(expr) &&
						!randoms.containsKey(expr) &&
						!sorts.containsKey(expr) &&
						!sortConstants.contains(expr) &&
						!Expressions.isStringLiteral(expr);
		return result;
	}
	
	boolean isKnownFunctor(Expression functorName) {
		boolean result = false;
		if (HOGMModelConstants.KNOWN_FUNCTORS.contains(functorName(functorName))) {
			result = true;
		}
		return result;
	}
	
	boolean isDeclaredConstantFunctor(Expression functorName, Map<Expression, HOGMConstantDeclaration> scopedConstants) {
		boolean result = false;
		if (scopedConstants.containsKey(functorName)) {
			result = true;
		}
		return result;
	}
	
	boolean isDeclaredRandomFunctor(Expression functorName) {
		boolean result = false;
		if (randoms.containsKey(functorName)) {
			result = true;
		}
		return result;
	}
	
	boolean isValidDeclaredConstantFunctorArity(Expression expr, Map<Expression, HOGMConstantDeclaration> scopedConstants) {
		boolean result = true;
		HOGMConstantDeclaration constantDeclaration = scopedConstants.get(expr.getFunctorOrSymbol());
		if (constantDeclaration.getArity().intValue() != expr.numberOfArguments()) {
			result = false;
		}
		return result;
	}
	
	boolean isValidKnownFunctorArity(Expression expr) {
		boolean result = true;
		String functorName = functorName(expr.getFunctor());

		if (HOGMModelConstants.KNOWN_ARITY_1_FUNCTORS.contains(functorName)
				&& expr.numberOfArguments() != 1) {
			result = false;
		}
		else if (HOGMModelConstants.KNOWN_ARITY_2_FUNCTORS.contains(functorName)
				&& expr.numberOfArguments() != 2) {
			result = false;
		}
		else if (HOGMModelConstants.KNOWN_ARITY_3_FUNCTORS.contains(functorName)
				&& expr.numberOfArguments() != 3) {
			result = false;
		}
		else if (HOGMModelConstants.KNOWN_ARITY_1_OR_2_FUNCTORS.contains(functorName)
				&& !(expr.numberOfArguments() == 1 || expr.numberOfArguments() == 2)) {
			result = false;
		}
		else if (HOGMModelConstants.KNOWN_ARITY_0_OR_MORE_FUNCTORS.contains(functorName) 
				&& expr.numberOfArguments() < 0) { // Should never happen
			result = false;
		}
		else if (HOGMModelConstants.KNOWN_ARITY_GREATER_THAN_1_FUNCTORS.contains(functorName) 
				&& expr.numberOfArguments() < 2) {
			result = false;
		}

		return result;
	}
	
	boolean isValidDeclaredRandomFunctorArity(Expression expr) {
		boolean result = true;
		HOGMRandomVariableDeclaration rvDeclaration = randoms.get(expr.getFunctorOrSymbol());
		if (rvDeclaration.getArity().intValue() != expr.numberOfArguments()) {
			result = false;
		}
		return result;
	}
	
	HOGMSortDeclaration getSort(Expression sortName) {
		HOGMSortDeclaration result = null;
		
		if (HOGMSortDeclaration.isIntegerIntervalReference(sortName)) {
			result = HOGMSortDeclaration.IN_BUILT_INTEGER;
		}
		else if (HOGMSortDeclaration.isRealIntervalReference(sortName)) {
			result = HOGMSortDeclaration.IN_BUILT_REAL;
		}
		else {
			for (HOGMSortDeclaration inbuilt : HOGMSortDeclaration.IN_BUILT_SORTS) {
				if (inbuilt.getName().equals(sortName)) {
					result = inbuilt;
					break;
				}
			}
			if (result == null) {
				result = sorts.get(sortName);
			}
		}
		
		return result;
	}
	
	String functorName(Expression exprFunctorName) {
		String result = exprFunctorName.getValue().toString();
		return result;
	}
	
	void newError(Type errorType, Object msg, StatementInfo statement) {
		errors.add(new HOGModelError(errorType, msg.toString(), statement));
	}
}