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
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.sgsolver.model.HOGModelError.Type;

@Beta
public class HOGModel {
	public enum TermCategoryType {
		BOOLEAN, NUMERIC, OTHER, INVALID
	}

	public static Expression validateAndConstruct(List<StatementInfo> sortDecs, List<StatementInfo> randomVarDecs, List<StatementInfo> terms) {
		HOGMModelValidator validator = new HOGMModelValidator(sortDecs, randomVarDecs, terms);
		
		if (!validator.isValid()) {
			throw new HOGModelException("Invalid model", validator.errors);
		}
		
		Expression result = Tuple.make(
				Tuple.make(validator.sortDeclarations), 
				Tuple.make(validator.randomVariableDeclarations), 
				Tuple.make(validator.conditionedPotentials));
		
		return result;
	}
	
	private static class HOGMModelValidator {
		List<Expression> sortDeclarations           = new ArrayList<>();
		List<Expression> randomVariableDeclarations = new ArrayList<>();
		List<Expression> conditionedPotentials      = new ArrayList<>();
		//
		List<HOGModelError> errors = new ArrayList<>();
		//
		Set<String> booleanTypeFunctors = new LinkedHashSet<>(HOGMModelConstants.KNOWN_BOOLEAN_FUNCTORS);
		Set<String> numericTypeFunctors = new LinkedHashSet<>(HOGMModelConstants.KNOWN_NUMERIC_FUNCTORS);
		Set<String> otherTypeFunctors   = new LinkedHashSet<>();
		//
		Predicate<Expression> isPrologConstant = new PrologConstantPredicate();
		//
		Set<Expression>                            sortConstants = new LinkedHashSet<>();
		Map<Expression, SortDeclaration>           sorts         = new LinkedHashMap<>();
		Map<Expression, RandomVariableDeclaration> randoms       = new LinkedHashMap<>();
		List<Expression> conditioned = new ArrayList<>();
		
		HOGMModelValidator(List<StatementInfo> sortStatements, List<StatementInfo> randomVariableStatements, List<StatementInfo> termStatements) {
			validateSortStatements(sortStatements);
			validateRandomVariableStatements(randomVariableStatements);
			
			associateUnassignedConstantsToSorts(termStatements);
			
			validateTermStatements(termStatements);	
			
			// Map validated values
			sortDeclarations.addAll(sorts.values().stream().map(sd -> sd.getSortDeclaration()).collect(Collectors.toList()));
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
					else {
						sorts.put(sortDeclaration.getName(), sortDeclaration);
					}
					sortDeclaration.getAssignedConstants().forEach(constant -> {
						if (sortConstants.contains(constant)) {
							newError(Type.CONSTANT_NAME_NOT_UNIQUE, constant, sortStatement);
						}
						sortConstants.add(constant);
					});
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
						newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_CONSTANT, rvDeclaration.getName(), rvStatement);
					}
					else {
						randoms.put(rvDeclaration.getName(), rvDeclaration);
						rvDeclaration.getParameterSorts().forEach(parameterSortName -> {
							if (!sorts.containsKey(parameterSortName) && !SortDeclaration.isNameOfInBuilt(parameterSortName)) {
								newError(Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED, parameterSortName, rvStatement);
							}
						});
						if (!sorts.containsKey(rvDeclaration.getRangeSort()) && !SortDeclaration.isNameOfInBuilt(rvDeclaration.getRangeSort())) {
							newError(Type.RANDOM_VARIABLE_SORT_ARGUMENT_NOT_DECLARED, rvDeclaration.getRangeSort(), rvStatement);
						}
						if (HOGMModelConstants.KNOWN_FUNCTORS.contains(functorName(rvDeclaration.getName()))) {
							newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_IN_BUILT_FUNCTOR, rvDeclaration.getName(), rvStatement);
						}
						if (ForAll.LABEL.equals(functorName(rvDeclaration.getName())) || ThereExists.LABEL.equals(functorName(rvDeclaration.getName()))) {
							newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_QUANTIFIER, rvDeclaration.getName(), rvStatement);
						}
						// Track the category type of the random variable
						if (rvDeclaration.getRangeSort().equals(SortDeclaration.IN_BUILT_BOOLEAN.getName())) {
							booleanTypeFunctors.add(functorName(rvDeclaration.getName()));
						}
						else if (rvDeclaration.getRangeSort().equals(SortDeclaration.IN_BUILT_NUMBER.getName())) {
							numericTypeFunctors.add(functorName(rvDeclaration.getName()));
						}
						else {
							otherTypeFunctors.add(functorName(rvDeclaration.getName()));
						}
					}
				}
			});
		}
		
		void associateUnassignedConstantsToSorts(List<StatementInfo> termStatements) {		
			termStatements.forEach(termStatement -> {
				// arguments to = and !=
				Set<Expression> equalities = Expressions.getSubExpressionsSatisfying(termStatement.statement, expr -> Equality.isEquality(expr) || Disequality.isDisequality(expr));
				equalities.forEach(equality -> {
					List<RandomVariableDeclaration> randomVars       = new ArrayList<>();
					Set<Expression>  unknownConstants = new LinkedHashSet<>();
					equality.getArguments().forEach(arg -> {
						RandomVariableDeclaration rvDeclaration = randoms.get(arg.getFunctorOrSymbol());
						if (rvDeclaration != null) {
							randomVars.add(rvDeclaration);
						}
						else if (isUnknownConstant(arg)) {
							unknownConstants.add(arg);
						}
					});
					
					if (randomVars.size() == 1 && unknownConstants.size() > 0) {
						updateSort(sorts.get(randomVars.get(0).getRangeSort()), unknownConstants);
					}
				});
				
				// random variable arguments
				Set<Expression> randomVars = Expressions.getSubExpressionsSatisfying(termStatement.statement, expr -> Expressions.isFunctionApplicationWithArguments(expr) && isDeclaredRandomFunctor(expr.getFunctor()));
				randomVars.forEach(randomVar -> {
					RandomVariableDeclaration rvDeclaration = randoms.get(randomVar.getFunctor());
					if (rvDeclaration.getArityValue() == randomVar.numberOfArguments()) {
						for (int i = 0; i < randomVar.numberOfArguments(); i++) {
							Expression arg = randomVar.get(i);
							if (isUnknownConstant(arg)) {
								Expression argSort = rvDeclaration.getParameterSorts().get(i);
								updateSort(sorts.get(argSort), Collections.singleton(arg));
							}
						}
					}
				});
			});		
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
			SortDeclaration result = null;
			
			if (IfThenElse.isIfThenElse(expr)) {
				SortDeclaration condition  = determineSortType(IfThenElse.condition(expr));
				SortDeclaration thenBranch = determineSortType(IfThenElse.thenBranch(expr));
				SortDeclaration elseBranch = determineSortType(IfThenElse.elseBranch(expr));
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
					else if (isPrologConstant.apply(expr)) {
						RandomVariableDeclaration rvDeclaration = randoms.get(expr);
						if (rvDeclaration != null) {
							result = getSort(rvDeclaration.getRangeSort());
						}
						else if (sortConstants.contains(expr)){
							for (SortDeclaration sort : sorts.values()) {
								// Have mapped the constant sort.
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
					else {
						result = null; // Don't know
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
			// Ensure random functions with arity > 0 have the correct arity and their arguments are of the correct type
			Set<Expression> randomFunctions = new LinkedHashSet<>();
			getRandomFunctions(termStatement.statement, randomFunctions);
			randomFunctions.forEach(randomFunction -> {
				if (!isValidDeclaredRandomFunctorArity(randomFunction)) {
					newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, randomFunction, termStatement);
				}
				else {
					RandomVariableDeclaration rvDeclaration = randoms.get(randomFunction.getFunctorOrSymbol());
					for (int i = 0; i < randomFunction.numberOfArguments(); i++) {
						Expression arg = randomFunction.get(i);
						if (isVariable(arg)) {
							continue; // takes on type of the sort at this position
						}
						if (isUnknownConstant(arg)) {
							newError(Type.TERM_CONSTANT_NOT_DEFINED, arg, termStatement);
						}
						else {
							if (getSort(rvDeclaration.getParameterSorts().get(i)) != determineSortType(arg)) {
								newError(Type.RANDOM_VARIABLE_ARGUMENT_IS_OF_THE_INCORRENT_TYPE, arg, termStatement);
							}
						}
					}
				}
			});
						
			// All of these should belong to the known set of functors
			Set<Expression> nonRandomFunctions = Expressions.getSubExpressionsSatisfying(termStatement.statement, expr -> 
					Expressions.isFunctionApplicationWithArguments(expr) && 
					!isDeclaredRandomFunctor(expr.getFunctorOrSymbol()));
			
			nonRandomFunctions.forEach(nonRandomFunction -> {
				if (isKnownFunctor(nonRandomFunction.getFunctor())) {
					String functorName = functorName(nonRandomFunction.getFunctor());
					if (!isValidKnownFunctorArity(nonRandomFunction)) {
						newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, "'"+nonRandomFunction+"'", termStatement);
					}
					else {
						Set<SortDeclaration> argSorts = new HashSet<>();
						for (int i = 0; i < nonRandomFunction.numberOfArguments(); i++) {
							Expression arg = nonRandomFunction.get(i);
							if (isVariable(arg)) {
								continue; // Takes on the type of its position
							}
							if (!isDeclaredRandomFunctor(arg.getFunctorOrSymbol()) && isUnknownConstant(arg)) {
								newError(Type.TERM_CONSTANT_NOT_DEFINED, arg, termStatement);
							}
							else {
								// All arguments must be of the same type
								SortDeclaration sortType = determineSortType(arg);
								if (sortType == null) {
									newError(Type.TERM_SORT_CANNOT_BE_DETERMINED, arg, termStatement);
								}
								if (IfThenElse.isIfThenElse(nonRandomFunction)) {
									if (i == 0) {
										// The conditional must be boolean
										if (sortType != SortDeclaration.IN_BUILT_BOOLEAN) {
											newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRENT_TYPE, arg, termStatement);
										}
									}
									else {
										// otherwise the branches sorts must match
										argSorts.add(sortType);
									}
							    }    // For equalities the types must match up
								else if (Equality.isEquality(nonRandomFunction) || Disequality.isDisequality(nonRandomFunction)) {
									if (sortType != null) {
										argSorts.add(sortType);
									}
								}
								else {
									if (booleanTypeFunctors.contains(functorName)) {
										if (sortType != SortDeclaration.IN_BUILT_BOOLEAN) {
											newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRENT_TYPE, arg, termStatement);
										}
									}
									else if (numericTypeFunctors.contains(functorName)) {
										if (sortType != SortDeclaration.IN_BUILT_NUMBER) {
											newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRENT_TYPE, arg, termStatement);
										}
									}
									else {
										newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRENT_TYPE, arg, termStatement);
									}
								}
							}
						}
						
						// The type of arguments must all match in these instances
						if (IfThenElse.isIfThenElse(nonRandomFunction) || Equality.isEquality(nonRandomFunction) || Disequality.isDisequality(nonRandomFunction)) {
							if (argSorts.size() != 1) {							
								newError(Type.TERM_ARGUMENTS_MUST_ALL_BE_OF_THE_SAME_TYPE, nonRandomFunction, termStatement);
							}
						}
					}
				}
				else {					
					newError(Type.TERM_TYPE_OF_FUNCTOR_NOT_DECLARED, nonRandomFunction.getFunctor(), termStatement);
				}
			});
			
			// NOTE: quantifiers are not functions so need to be handled separately
			Set<Expression> quantifiers = Expressions.getSubExpressionsSatisfying(termStatement.statement, expr -> ForAll.isForAll(expr) || ThereExists.isThereExists(expr));
			quantifiers.forEach(quantifier -> {
// TODO - validate index				
				Expression body = ForAll.isForAll(quantifier) ? ForAll.getBody(quantifier) : ThereExists.getBody(quantifier);
				if (determineSortType(body) != SortDeclaration.IN_BUILT_BOOLEAN) {
					newError(Type.TERM_ARGUMENT_IS_OF_THE_INCORRENT_TYPE, body, termStatement);
				}
			});			
		}
		
		void getRandomFunctions(Expression expr, Set<Expression> randomFunctions) {
			if (this.isDeclaredRandomFunctor(expr.getFunctorOrSymbol())) {
				randomFunctions.add(expr);
			}
			
			if (Expressions.isFunctionApplicationWithArguments(expr)) {
				expr.getArguments().forEach(arg -> getRandomFunctions(arg, randomFunctions));
			}
			else if (ForAll.isForAll(expr)) {
				getRandomFunctions(ForAll.getBody(expr), randomFunctions);
			}
			else if (ThereExists.isThereExists(expr)) {
				getRandomFunctions(ThereExists.getBody(expr), randomFunctions);
			}
		}
		
		boolean isVariable(Expression expr) {
			boolean result = Expressions.isSymbol(expr) && !isPrologConstant.apply(expr);
			return result;
		}
		
		boolean isUnknownConstant(Expression expr) {
			boolean result = Expressions.isSymbol(expr) && 
							!Expressions.isNumber(expr) && 
							!isVariable(expr) && 
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
		
		boolean isDeclaredRandomFunctor(Expression functorName) {
			boolean result = false;
			if (randoms.containsKey(functorName)) {
				result = true;
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
		
		void updateSort(SortDeclaration sort, Set<Expression> unknownConstants) {
			if (sort != null) {
				List<Expression> constants = new ArrayList<>(ExtensionalSet.getElements(sort.getConstants()));
				// Ensure we can extend the constants associated with the sort
				if (SortDeclaration.UNKNOWN_SIZE.equals(sort.getSize()) ||
					(constants.size() + unknownConstants.size()) < sort.getSize().intValue()) {
					constants.addAll(unknownConstants);
					sorts.put(sort.getName(), new SortDeclaration(sort.getName(), sort.getSize(), ExtensionalSet.makeUniSet(constants)));
					sortConstants.addAll(unknownConstants);
				}
			}
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