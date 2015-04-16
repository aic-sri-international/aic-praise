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
package com.sri.ai.praise.sgsolver.model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.model.ConstantDeclaration;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.sgsolver.model.HOGModelError.Type;
import com.sri.ai.util.base.Pair;

@Beta
public class HOGModel {
	public enum TermCategoryType {
		BOOLEAN, NUMERIC, OTHER, INVALID
	}

	public static Expression validateAndConstruct(List<StatementInfo> sortDecs, List<StatementInfo> constantDecs, List<StatementInfo> randomVarDecs, List<StatementInfo> terms) {
		HOGMModelValidator validator = new HOGMModelValidator(sortDecs, constantDecs, randomVarDecs, terms);
		
		if (!validator.isValid()) {
			throw new HOGModelException("Invalid model", validator.errors);
		}
		
		Expression result = Tuple.make(
				Tuple.make(validator.sortDeclarations), 
				Tuple.make(validator.constantDeclarations),
				Tuple.make(validator.randomVariableDeclarations), 
				Tuple.make(validator.conditionedPotentials));
		
		return result;
	}
	
	private static class HOGMModelValidator {
		List<Expression> sortDeclarations           = new ArrayList<>();
		List<Expression> constantDeclarations       = new ArrayList<>();
		List<Expression> randomVariableDeclarations = new ArrayList<>();
		List<Expression> conditionedPotentials      = new ArrayList<>();
		//
		List<HOGModelError> errors = new ArrayList<>();
		//
		Set<String> booleanTypeFunctors = new LinkedHashSet<>(HOGMModelConstants.KNOWN_BOOLEAN_FUNCTORS);
		Set<String> numericTypeFunctors = new LinkedHashSet<>(HOGMModelConstants.KNOWN_NUMERIC_FUNCTORS);
		Set<String> otherTypeFunctors   = new LinkedHashSet<>();
		//
		Set<Expression>                            sortConstants = new LinkedHashSet<>();
		Map<Expression, SortDeclaration>           sorts         = new LinkedHashMap<>();
		Map<Expression, ConstantDeclaration>       constants     = new LinkedHashMap<>();
		Map<Expression, RandomVariableDeclaration> randoms       = new LinkedHashMap<>();
		List<Expression> conditioned = new ArrayList<>();
		
		HOGMModelValidator(List<StatementInfo> sortStatements, List<StatementInfo> constantStatements, List<StatementInfo> randomVariableStatements, List<StatementInfo> termStatements) {
			validateSortStatements(sortStatements);
			validateConstantStatements(constantStatements);
			validateRandomVariableStatements(randomVariableStatements);
			
			validateTermStatements(termStatements);	
			
			// Map validated values
			sortDeclarations.addAll(sorts.values().stream().map(sd -> sd.getSortDeclaration()).collect(Collectors.toList()));
			constantDeclarations.addAll(constants.values().stream().map(cd -> cd.getConstantDeclaration()).collect(Collectors.toList()));
			randomVariableDeclarations.addAll(randoms.values().stream().map(rd -> rd.getRandomVariableDeclaration()).collect(Collectors.toList()));
			conditionedPotentials.addAll(conditioned);
		}
		
		boolean isValid() {
			return errors.size() == 0;
		}
		
		void validateSortStatements(List<StatementInfo> sortStatements) {
			// Pre-associated with the Boolean Sort
			sortConstants.add(Expressions.FALSE);
			sortConstants.add(Expressions.TRUE);
			
			sortStatements.forEach(sortStatement -> {
				if (SortDeclaration.isNameOfInBuilt(sortStatement.statement.get(0))) {
					newError(Type.SORT_NAME_PREDEFINED, "", sortStatement);
				}
				else if (!SortDeclaration.isSortDeclaration(sortStatement.statement)) {
					newError(Type.SORT_DECLARATION_IS_NOT_LEGAL, "", sortStatement);
				}
				else {
					SortDeclaration sortDeclaration = SortDeclaration.makeSortDeclaration(sortStatement.statement);
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
		
		void validateConstantStatements(List<StatementInfo> constantStatements) {				
			constantStatements.forEach(constantStatement -> {
				if (!ConstantDeclaration.isConstantDeclaration(constantStatement.statement)) {
					newError(Type.CONSTANT_DECLARATION_IS_NOT_LEGAL, "", constantStatement);
				}
				else {
					ConstantDeclaration constantDeclaration = ConstantDeclaration.makeConstantDeclaration(constantStatement.statement);
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
				if (!RandomVariableDeclaration.isRandomVariableDeclaration(rvStatement.statement)) {
					newError(Type.RANDOM_VARIABLE_IS_NOT_LEGAL, "", rvStatement);
				}
				else {
					RandomVariableDeclaration rvDeclaration = RandomVariableDeclaration.makeRandomVariableDeclaration(rvStatement.statement);
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
			if (sortName.equals(SortDeclaration.IN_BUILT_BOOLEAN.getName())) {
				booleanTypeFunctors.add(functorName(functorName));
			}
			else if (sortName.equals(SortDeclaration.IN_BUILT_NUMBER.getName())) {
				numericTypeFunctors.add(functorName(functorName));
			}
			else {
				otherTypeFunctors.add(functorName(functorName));
			}
		}
		
		void validateTermStatements(List<StatementInfo> termStatements) {
			termStatements.forEach(termStatement -> {
				// Ensure all functors are known and have correct arity
				validateFunctorsAndArguments(termStatement);
				
				Expression statement = termStatement.statement;
				// Determine type
				TermCategoryType termType = determineTermCategoryType(statement);
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
				}
				else if (termType != TermCategoryType.BOOLEAN) {
					newError(Type.TERM_NON_CONDITIONAL_STATEMENT_MUST_BE_OF_TYPE_BOOLEAN, "", termStatement);
				}
				else {
					conditioned.add(IfThenElse.make(statement, Expressions.ONE, Expressions.ZERO));
				}
			});
		}
		
		TermCategoryType determineTermCategoryType(Expression expr) {
			TermCategoryType result = TermCategoryType.INVALID;
			
			SortDeclaration sortType = determineSortType(expr);
			if (sortType != null) {
				if (sortType == SortDeclaration.IN_BUILT_BOOLEAN) {
					result = TermCategoryType.BOOLEAN;
				}
				else if (sortType == SortDeclaration.IN_BUILT_NUMBER) {
					result = TermCategoryType.NUMERIC;
				}
				else  {
					result = TermCategoryType.OTHER;
				}
			}
			
			return result;
		}
		
		SortDeclaration determineSortType(Expression expr) {
			SortDeclaration result = determineSortType(expr, constants);
			return result;
		}
		
		SortDeclaration determineSortType(Expression expr, Map<Expression, ConstantDeclaration> scopedConstants) {
			SortDeclaration result = null;
			
			if (IfThenElse.isIfThenElse(expr)) {
				SortDeclaration condition  = determineSortType(IfThenElse.condition(expr), scopedConstants);
				SortDeclaration thenBranch = determineSortType(IfThenElse.thenBranch(expr), scopedConstants);
				SortDeclaration elseBranch = determineSortType(IfThenElse.elseBranch(expr), scopedConstants);
				// Ensure legal 
				if (condition == SortDeclaration.IN_BUILT_BOOLEAN && thenBranch == elseBranch) {
					result = thenBranch; // same as elseBranch
				}
				else {
					result = null; // Don't know
				}
			}
			else {
				Expression functor = expr.getFunctor();
				if (functor == null) {
					if (Expressions.FALSE.equals(expr) || Expressions.TRUE.equals(expr)) {
						result = SortDeclaration.IN_BUILT_BOOLEAN;
					}
					else if (ForAll.isForAll(expr) || ThereExists.isThereExists(expr)) { // NOTE: quantifiers are not functions
						result = SortDeclaration.IN_BUILT_BOOLEAN;
					}
					else if (Expressions.isNumber(expr)) {
						result = SortDeclaration.IN_BUILT_NUMBER;
					}
					else {
						if (isDeclaredConstantFunctor(expr.getFunctorOrSymbol(), scopedConstants)) {
							ConstantDeclaration constantDeclaration = scopedConstants.get(expr.getFunctorOrSymbol());
							result = getSort(constantDeclaration.getRangeSort());
						}
						else if (isDeclaredRandomFunctor(expr.getFunctorOrSymbol())) {
							RandomVariableDeclaration rvDeclaration = randoms.get(expr.getFunctorOrSymbol());
							result = getSort(rvDeclaration.getRangeSort());
						}
						else if (sortConstants.contains(expr)){
							for (SortDeclaration sort : sorts.values()) {
								// Have mapped the unique constant sort.
								if (ExtensionalSet.getElements(sort.getConstants()).contains(expr)) {
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
						result = SortDeclaration.IN_BUILT_BOOLEAN;
					}
					else if (numericTypeFunctors.contains(functorName)) {
						result = SortDeclaration.IN_BUILT_NUMBER;
					}
					else if (isDeclaredConstantFunctor(functor, scopedConstants)) {
						ConstantDeclaration constantDeclaration = scopedConstants.get(functor);
						result = getSort(constantDeclaration.getRangeSort());
					}
					else if (isDeclaredRandomFunctor(functor)) {
						RandomVariableDeclaration rvDeclaration = randoms.get(functor);
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
			
			TermCategoryType conditionType = determineTermCategoryType(IfThenElse.condition(conditional));
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
			
			TermCategoryType exprType = determineTermCategoryType(expr);
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
			List<Pair<Expression, Map<Expression, ConstantDeclaration>>> constantFunctionsWithScope = getConstantFunctionsWithScope(termStatement.statement);
			constantFunctionsWithScope.forEach(constantFunctionAndScope -> {
				Expression                           constantFunction = constantFunctionAndScope.first;
				Map<Expression, ConstantDeclaration> scopedConstants  = constantFunctionAndScope.second;
				if (!isValidDeclaredConstantFunctorArity(constantFunction, scopedConstants)) {
					newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, constantFunction, termStatement);
				}
				else {
					ConstantDeclaration constantDeclaration = scopedConstants.get(constantFunction.getFunctorOrSymbol());
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
			List<Pair<Expression, Map<Expression, ConstantDeclaration>>> randomFunctionsWithScope = getRandomFunctionsWithScope(termStatement.statement);
			randomFunctionsWithScope.forEach(randomFunctionAndScope -> {
				Expression                           randomFunction  = randomFunctionAndScope.first;
				Map<Expression, ConstantDeclaration> scopedConstants = randomFunctionAndScope.second;
				if (!isValidDeclaredRandomFunctorArity(randomFunction)) {
					newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, randomFunction, termStatement);
				}
				else {
					RandomVariableDeclaration rvDeclaration = randoms.get(randomFunction.getFunctorOrSymbol());
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
			List<Pair<Expression, Map<Expression, ConstantDeclaration>>> nonConstantAndRandomFunctionsWithScope = getNonConstantRandomFunctionsWithScope(termStatement.statement);
			
			nonConstantAndRandomFunctionsWithScope.forEach(nonConstantAndRandomFunctionWithScope -> {
				Expression                           nonConstantAndRandomFunction = nonConstantAndRandomFunctionWithScope.first;
				Map<Expression, ConstantDeclaration> scopedConstants              = nonConstantAndRandomFunctionWithScope.second;
				
				if (isKnownFunctor(nonConstantAndRandomFunction.getFunctor())) {
					String functorName = functorName(nonConstantAndRandomFunction.getFunctor());
					if (!isValidKnownFunctorArity(nonConstantAndRandomFunction)) {
						newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, "'"+nonConstantAndRandomFunction+"'", termStatement);
					}
					else {
						Set<SortDeclaration> argSorts = new HashSet<>();
						for (int i = 0; i < nonConstantAndRandomFunction.numberOfArguments(); i++) {
							Expression arg = nonConstantAndRandomFunction.get(i);
							if (!isDeclaredConstantFunctor(arg.getFunctorOrSymbol(), scopedConstants) &&
								!isDeclaredRandomFunctor(arg.getFunctorOrSymbol()) &&
								isUnknownConstant(arg, scopedConstants)) {									
								newError(Type.TERM_CONSTANT_NOT_DEFINED, arg, termStatement);
							}
							else {
								// All arguments must be of the same type
								SortDeclaration sortType = determineSortType(arg, scopedConstants);
								if (sortType == null) {
									newError(Type.TERM_SORT_CANNOT_BE_DETERMINED, arg, termStatement);
								}
								if (IfThenElse.isIfThenElse(nonConstantAndRandomFunction)) {
									if (i == 0) {
										// The conditional must be boolean
										if (sortType != SortDeclaration.IN_BUILT_BOOLEAN) {
											newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
										}
									}
									else {
										// otherwise the branches sorts must match
										argSorts.add(sortType);
									}
							    }    // For equalities the types must match up
								else if (Equality.isEquality(nonConstantAndRandomFunction) || Disequality.isDisequality(nonConstantAndRandomFunction)) {
									if (sortType != null) {
										argSorts.add(sortType);
									}
								}
								else {
									if (booleanTypeFunctors.contains(functorName)) {
										if (sortType != SortDeclaration.IN_BUILT_BOOLEAN) {
											newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
										}
									}
									else if (numericTypeFunctors.contains(functorName)) {
										if (sortType != SortDeclaration.IN_BUILT_NUMBER) {
											newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
										}
									}
									else {
										newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, arg, termStatement);
									}
								}
							}
						}
						
						// The type of arguments must all match in these instances
						if (IfThenElse.isIfThenElse(nonConstantAndRandomFunction) || Equality.isEquality(nonConstantAndRandomFunction) || Disequality.isDisequality(nonConstantAndRandomFunction)) {
							if (argSorts.size() != 1) {							
								newError(Type.TERM_ARGUMENTS_MUST_ALL_BE_OF_THE_SAME_TYPE, nonConstantAndRandomFunction, termStatement);
							}
						}
					}
				}
				else {					
					newError(Type.TERM_TYPE_OF_FUNCTOR_NOT_DECLARED, nonConstantAndRandomFunction.getFunctor(), termStatement);
				}
			});
			
			// NOTE: quantifiers are not functions so need to be handled separately
			List<Pair<Expression, Map<Expression, ConstantDeclaration>>> quantifiersWithScope = getQuantifiersWithScope(termStatement.statement);
			quantifiersWithScope.forEach(quantifierWithScope -> {
				Expression                           quantifier      = quantifierWithScope.first;
				Map<Expression, ConstantDeclaration> scopedConstants = quantifierWithScope.second;
				
				Expression indexExpression = ForAll.isForAll(quantifier) ? ForAll.getIndexExpression(quantifier) : ThereExists.getIndexExpression(quantifier);
				Pair<Expression, Expression> indexAndType = IndexExpressions.getIndexAndDomain(indexExpression);
				if (getSort(indexAndType.second) == null) {
					newError(Type.TERM_SORT_CANNOT_BE_DETERMINED, indexAndType.second, termStatement);
				}
				
				Expression body = ForAll.isForAll(quantifier) ? ForAll.getBody(quantifier) : ThereExists.getBody(quantifier);
				if (determineSortType(body, scopedConstants) != SortDeclaration.IN_BUILT_BOOLEAN) {
					newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRECT_TYPE, body, termStatement);
				}
			});			
		}
		
		List<Pair<Expression, Map<Expression, ConstantDeclaration>>> getRandomFunctionsWithScope(Expression expr) {
			List<Pair<Expression, Map<Expression, ConstantDeclaration>>> result = new ArrayList<>();
			getRandomFunctions(expr, result, constants);
			return result;
		}
		
		void getRandomFunctions(Expression expr, List<Pair<Expression, Map<Expression, ConstantDeclaration>>> randomFunctionsWithScope, Map<Expression, ConstantDeclaration> currentScope) {
			if (this.isDeclaredRandomFunctor(expr.getFunctorOrSymbol())) {
				randomFunctionsWithScope.add(new Pair<>(expr, currentScope));
			}
			
			if (Expressions.isFunctionApplicationWithArguments(expr)) {
				expr.getArguments().forEach(arg -> getRandomFunctions(arg, randomFunctionsWithScope, currentScope));
			}
			else if (ForAll.isForAll(expr) || ThereExists.isThereExists(expr)) {
				Map<Expression, ConstantDeclaration> quantifierScope = new LinkedHashMap<>(currentScope);
				
				Expression indexExpression = ForAll.isForAll(expr) ? ForAll.getIndexExpression(expr) : ThereExists.getIndexExpression(expr);
				Pair<Expression, Expression> indexAndType = IndexExpressions.getIndexAndDomain(indexExpression);
				SortDeclaration localSort = getSort(indexAndType.second);
				if (localSort != null) {
					quantifierScope.put(indexAndType.first, new ConstantDeclaration(indexAndType.first, Expressions.ZERO, localSort.getName()));
				}
				Expression bodyExpression  = ForAll.isForAll(expr) ? ForAll.getBody(expr) : ThereExists.getBody(expr);
				getRandomFunctions(bodyExpression, randomFunctionsWithScope, quantifierScope);
			}
		}
		
		List<Pair<Expression, Map<Expression, ConstantDeclaration>>> getConstantFunctionsWithScope(Expression expr) {
			List<Pair<Expression, Map<Expression, ConstantDeclaration>>> result = new ArrayList<>();
			getConstantFunctions(expr, result, constants);
			return result;
		}
		
		void getConstantFunctions(Expression expr, List<Pair<Expression, Map<Expression, ConstantDeclaration>>> constantFunctionsWithScope, Map<Expression, ConstantDeclaration> currentScope) {
			if (this.isDeclaredConstantFunctor(expr.getFunctorOrSymbol(), currentScope)) {
				constantFunctionsWithScope.add(new Pair<>(expr, currentScope));
			}
			
			if (Expressions.isFunctionApplicationWithArguments(expr)) {
				expr.getArguments().forEach(arg -> getConstantFunctions(arg, constantFunctionsWithScope, currentScope));
			}
			else if (ForAll.isForAll(expr) || ThereExists.isThereExists(expr)) {
				Map<Expression, ConstantDeclaration> quantifierScope = new LinkedHashMap<>(currentScope);
				
				Expression indexExpression = ForAll.isForAll(expr) ? ForAll.getIndexExpression(expr) : ThereExists.getIndexExpression(expr);
				Pair<Expression, Expression> indexAndType = IndexExpressions.getIndexAndDomain(indexExpression);
				SortDeclaration localSort = getSort(indexAndType.second);
				if (localSort != null) {
					quantifierScope.put(indexAndType.first, new ConstantDeclaration(indexAndType.first, Expressions.ZERO, localSort.getName()));
				}
				Expression bodyExpression  = ForAll.isForAll(expr) ? ForAll.getBody(expr) : ThereExists.getBody(expr);
				getConstantFunctions(bodyExpression, constantFunctionsWithScope, quantifierScope);
			}
		}

		List<Pair<Expression, Map<Expression, ConstantDeclaration>>> getNonConstantRandomFunctionsWithScope(Expression expr) {
			List<Pair<Expression, Map<Expression, ConstantDeclaration>>> result = new ArrayList<>();
			getNonConstantRandomFunctions(expr, result, constants);
			return result;
		}
		
		void getNonConstantRandomFunctions(Expression expr, List<Pair<Expression, Map<Expression, ConstantDeclaration>>> nonConstantRandomFunctionsWithScope, Map<Expression, ConstantDeclaration> currentScope) {
			if (Expressions.isFunctionApplicationWithArguments(expr) &&
				!isDeclaredConstantFunctor(expr.getFunctorOrSymbol(), currentScope) &&
				!isDeclaredRandomFunctor(expr.getFunctorOrSymbol())) {
				nonConstantRandomFunctionsWithScope.add(new Pair<>(expr, currentScope));
			}
			
			if (Expressions.isFunctionApplicationWithArguments(expr)) {
				expr.getArguments().forEach(arg -> getNonConstantRandomFunctions(arg, nonConstantRandomFunctionsWithScope, currentScope));
			}
			else if (ForAll.isForAll(expr) || ThereExists.isThereExists(expr)) {
				Map<Expression, ConstantDeclaration> quantifierScope = new LinkedHashMap<>(currentScope);
				
				Expression indexExpression = ForAll.isForAll(expr) ? ForAll.getIndexExpression(expr) : ThereExists.getIndexExpression(expr);
				Pair<Expression, Expression> indexAndType = IndexExpressions.getIndexAndDomain(indexExpression);
				SortDeclaration localSort = getSort(indexAndType.second);
				if (localSort != null) {
					quantifierScope.put(indexAndType.first, new ConstantDeclaration(indexAndType.first, Expressions.ZERO, localSort.getName()));
				}
				Expression bodyExpression  = ForAll.isForAll(expr) ? ForAll.getBody(expr) : ThereExists.getBody(expr);
				getNonConstantRandomFunctions(bodyExpression, nonConstantRandomFunctionsWithScope, quantifierScope);
			}
		}
		
		List<Pair<Expression, Map<Expression, ConstantDeclaration>>> getQuantifiersWithScope(Expression expr) {
			List<Pair<Expression, Map<Expression, ConstantDeclaration>>> result = new ArrayList<>();
			getQuantifiers(expr, result, constants);
			return result;
		}
		
		void getQuantifiers(Expression expr, List<Pair<Expression, Map<Expression, ConstantDeclaration>>> quantifiersWithScope, Map<Expression, ConstantDeclaration> currentScope) {
			if (ForAll.isForAll(expr) || ThereExists.isThereExists(expr)) {
				Map<Expression, ConstantDeclaration> quantifierScope = new LinkedHashMap<>(currentScope);
				
				Expression indexExpression = ForAll.isForAll(expr) ? ForAll.getIndexExpression(expr) : ThereExists.getIndexExpression(expr);
				Pair<Expression, Expression> indexAndType = IndexExpressions.getIndexAndDomain(indexExpression);
				SortDeclaration localSort = getSort(indexAndType.second);
				if (localSort != null) {
					quantifierScope.put(indexAndType.first, new ConstantDeclaration(indexAndType.first, Expressions.ZERO, localSort.getName()));
				}
				
				quantifiersWithScope.add(new Pair<>(expr, quantifierScope));
				
				Expression bodyExpression  = ForAll.isForAll(expr) ? ForAll.getBody(expr) : ThereExists.getBody(expr);	
				getQuantifiers(bodyExpression, quantifiersWithScope, quantifierScope);
			}
			
			if (Expressions.isFunctionApplicationWithArguments(expr)) {
				expr.getArguments().forEach(arg -> getQuantifiers(arg, quantifiersWithScope, currentScope));
			}

		}
		
		boolean isUnknownConstant(Expression expr, Map<Expression, ConstantDeclaration> scopedConstants) {
			boolean result = Expressions.isSymbol(expr) &&
							!Expressions.isNumber(expr) &&
							!ForAll.isForAll(expr) &&
							!ThereExists.isThereExists(expr) &&
							!scopedConstants.containsKey(expr) &&
							!randoms.containsKey(expr) &&
							!sortConstants.contains(expr);
			return result;
		}
		
		boolean isKnownFunctor(Expression functorName) {
			boolean result = false;
			if (HOGMModelConstants.KNOWN_FUNCTORS.contains(functorName(functorName))) {
				result = true;
			}
			return result;
		}
		
		boolean isDeclaredConstantFunctor(Expression functorName, Map<Expression, ConstantDeclaration> scopedConstants) {
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
		
		boolean isValidDeclaredConstantFunctorArity(Expression expr, Map<Expression, ConstantDeclaration> scopedConstants) {
			boolean result = true;
			ConstantDeclaration constantDeclaration = scopedConstants.get(expr.getFunctorOrSymbol());
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
			RandomVariableDeclaration rvDeclaration = randoms.get(expr.getFunctorOrSymbol());
			if (rvDeclaration.getArity().intValue() != expr.numberOfArguments()) {
				result = false;
			}
			return result;
		}
		
		SortDeclaration getSort(Expression sortName) {
			SortDeclaration result = null;
			
			if (SortDeclaration.IN_BUILT_BOOLEAN.getName().equals(sortName)) {
				result = SortDeclaration.IN_BUILT_BOOLEAN;
			}
			else if (SortDeclaration.IN_BUILT_NUMBER.getName().equals(sortName)) {
				result = SortDeclaration.IN_BUILT_NUMBER;
			}
			else {
				result = sorts.get(sortName);
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
}