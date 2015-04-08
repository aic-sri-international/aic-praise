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
import java.util.Iterator;
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
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.praise.model.SortDeclaration;
import com.sri.ai.praise.sgsolver.model.HOGModelError.Type;

@Beta
public class HOGModel {
	
	public final static Set<String> KNOWN_BOOLEAN_FUNCTORS;
	static {
		Set<String> knownBooleanFunctors = new LinkedHashSet<String>();
		knownBooleanFunctors.add(FunctorConstants.NOT);
		knownBooleanFunctors.add(FunctorConstants.AND);
		knownBooleanFunctors.add(FunctorConstants.OR);
		knownBooleanFunctors.add(ForAll.LABEL);
		knownBooleanFunctors.add(ThereExists.LABEL);
		knownBooleanFunctors.add(FunctorConstants.IMPLICATION);
		knownBooleanFunctors.add(FunctorConstants.EQUIVALENCE);
		knownBooleanFunctors.add(FunctorConstants.LESS_THAN);
		knownBooleanFunctors.add(FunctorConstants.LESS_THAN_OR_EQUAL_TO);
		knownBooleanFunctors.add(FunctorConstants.EQUALITY);
		knownBooleanFunctors.add(FunctorConstants.DISEQUALITY);
		knownBooleanFunctors.add(FunctorConstants.GREATER_THAN_OR_EQUAL_TO);
		knownBooleanFunctors.add(FunctorConstants.GREATER_THAN);
		
		KNOWN_BOOLEAN_FUNCTORS = Collections.unmodifiableSet(knownBooleanFunctors);
	}
	
	public final static Set<String> KNOWN_NUMERIC_FUNCTORS;
	static {
		Set<String> knownNumericFunctors = new LinkedHashSet<String>();
		knownNumericFunctors.add(FunctorConstants.EXPONENTIATION);
		knownNumericFunctors.add(FunctorConstants.DIVISION);
		knownNumericFunctors.add(FunctorConstants.TIMES);
		knownNumericFunctors.add(FunctorConstants.PLUS);
		knownNumericFunctors.add(FunctorConstants.MINUS);
		
		KNOWN_NUMERIC_FUNCTORS = Collections.unmodifiableSet(knownNumericFunctors);
	}
	
	public final static Set<String> KNOWN_UNDETERMINED_FUNCTORS;
	static {
		Set<String> undetermined = new LinkedHashSet<>();
		undetermined.add(FunctorConstants.IF_THEN_ELSE);
		
		KNOWN_UNDETERMINED_FUNCTORS = Collections.unmodifiableSet(undetermined);
	}
	
	public final static Set<String> KNOWN_FUNCTORS;
	static {
		Set<String> knownFunctors = new LinkedHashSet<>();
		knownFunctors.addAll(KNOWN_BOOLEAN_FUNCTORS);
		knownFunctors.addAll(KNOWN_NUMERIC_FUNCTORS);
		knownFunctors.addAll(KNOWN_UNDETERMINED_FUNCTORS);
		
		KNOWN_FUNCTORS = Collections.unmodifiableSet(knownFunctors);
	}
	
	public final static Set<String> KNOWN_ARITY_1_FUNCTORS;
	static {
		Set<String> arity1 = new LinkedHashSet<>();
		arity1.add(FunctorConstants.NOT);
		
		KNOWN_ARITY_1_FUNCTORS = Collections.unmodifiableSet(arity1);
	}
	
	public final static Set<String> KNOWN_ARITY_2_FUNCTORS;
	static {
		Set<String> arity2 = new LinkedHashSet<>();
		arity2.add(ForAll.LABEL);
		arity2.add(ThereExists.LABEL);
		arity2.add(FunctorConstants.IMPLICATION);
		arity2.add(FunctorConstants.EQUIVALENCE);
		arity2.add(FunctorConstants.LESS_THAN);
		arity2.add(FunctorConstants.LESS_THAN_OR_EQUAL_TO);
		arity2.add(FunctorConstants.DISEQUALITY);
		arity2.add(FunctorConstants.GREATER_THAN_OR_EQUAL_TO);
		arity2.add(FunctorConstants.GREATER_THAN);
		arity2.add(FunctorConstants.EXPONENTIATION);
		arity2.add(FunctorConstants.DIVISION);
		
		KNOWN_ARITY_2_FUNCTORS = Collections.unmodifiableSet(arity2);
	}
	
	public final static Set<String> KNOWN_ARITY_3_FUNCTORS;
	static {
		Set<String> arity3 = new LinkedHashSet<>();
		arity3.add(FunctorConstants.IF_THEN_ELSE);
		
		KNOWN_ARITY_3_FUNCTORS = Collections.unmodifiableSet(arity3);
	}
	
	public final static Set<String> KNOWN_ARITY_1_OR_2_FUNCTORS;
	static {
		Set<String> arity1or2 = new LinkedHashSet<>();
		// i.e. unary minus or normal subtraction
		arity1or2.add(FunctorConstants.MINUS); 
		KNOWN_ARITY_1_OR_2_FUNCTORS = Collections.unmodifiableSet(arity1or2);
	}
	
	public final static Set<String> KNOWN_ARITY_0_OR_MORE_FUNCTORS;
	static {
		Set<String> arity0Plus = new LinkedHashSet<>();
		arity0Plus.add(FunctorConstants.AND);
		arity0Plus.add(FunctorConstants.OR);
		arity0Plus.add(FunctorConstants.EQUALITY);	
		
		KNOWN_ARITY_0_OR_MORE_FUNCTORS = Collections.unmodifiableSet(arity0Plus);
	}
	
	public final static Set<String> KNOWN_ARITY_GREATER_THAN_1_FUNCTORS;
	static {
		Set<String> arity2Plus = new LinkedHashSet<>();
		
		arity2Plus.add(FunctorConstants.TIMES);
		arity2Plus.add(FunctorConstants.PLUS);		
		
		KNOWN_ARITY_GREATER_THAN_1_FUNCTORS = Collections.unmodifiableSet(arity2Plus);
	}
	
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
		Set<String> booleanTypeFunctors = new LinkedHashSet<>(KNOWN_BOOLEAN_FUNCTORS);
		Set<String> numericTypeFunctors = new LinkedHashSet<>(KNOWN_NUMERIC_FUNCTORS);
		Set<String> otherTypeFunctors   = new LinkedHashSet<>();
		//
		Predicate<Expression> isPrologConstant = new PrologConstantPredicate();
		//
		Set<Expression> sortConstants = new LinkedHashSet<>();
		Map<Expression, SortDeclaration> sorts = new LinkedHashMap<>();
		Map<Expression, RandomVariableDeclaration> randoms = new LinkedHashMap<>();
		List<Expression> conditioned = new ArrayList<>();
		
		HOGMModelValidator(List<StatementInfo> sortStatements, List<StatementInfo> randomVariableStatements, List<StatementInfo> termStatements) {
			validateSortStatements(sortStatements);
			validateRandomVariableStatements(randomVariableStatements);
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
				if (!SortDeclaration.isSortDeclaration(sortStatement.statement)) {
					newError(Type.SORT_DECLARATION_IS_NOT_LEGAL, "", sortStatement);
				}
				else if (SortDeclaration.isInBuilt(sortStatement.statement)) {
					newError(Type.SORT_NAME_PREDEFINED, "", sortStatement);
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
						if (KNOWN_FUNCTORS.contains(rvDeclaration.getName().toString())) {
							newError(Type.RANDOM_VARIABLE_NAME_SAME_AS_IN_BUILT_FUNCTOR, rvDeclaration.getName(), rvStatement);
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
		
		void validateTermStatements(List<StatementInfo> termStatements) {
			termStatements.forEach(termStatement -> {
				// Ensure all functors are known and have correct arity
				validateFunctors(termStatement);
				
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
			
			if (IfThenElse.isIfThenElse(expr)) {
				TermCategoryType condition  = determineTermCategoryType(IfThenElse.condition(expr));
				TermCategoryType thenBranch = determineTermCategoryType(IfThenElse.thenBranch(expr));
				TermCategoryType elseBranch = determineTermCategoryType(IfThenElse.elseBranch(expr));
				// Ensure legal 
				if (condition == TermCategoryType.BOOLEAN && thenBranch == elseBranch) {
					result = thenBranch; // same as elseBranch
				}
			}
			else {
				Expression functor = expr.getFunctor();
				if (functor == null) {
					if (Expressions.FALSE.equals(expr) || Expressions.TRUE.equals(expr)) {
						result = TermCategoryType.BOOLEAN;
					}
					else if (Expressions.isNumber(expr)) {
						result = TermCategoryType.NUMERIC;
					}
					else if (isPrologConstant.apply(expr)) {
						RandomVariableDeclaration rvDeclaration = randoms.get(expr);
						if (rvDeclaration != null) {
							if (SortDeclaration.IN_BUILT_BOOLEAN.getName().equals(rvDeclaration.getRangeSort())) {
								result = TermCategoryType.BOOLEAN;
							}
							else if (SortDeclaration.IN_BUILT_NUMBER.getName().equals(rvDeclaration.getRangeSort())) {
								result = TermCategoryType.NUMERIC;
							}
							else {
								result = TermCategoryType.OTHER;
							}
						}
						else {
							result = TermCategoryType.OTHER;
						}
					}
					else {
						result = TermCategoryType.OTHER;
					}
				}
				else {
					String functorName = functorName(functor);
					if (booleanTypeFunctors.contains(functorName)) {
						result = TermCategoryType.BOOLEAN;
					}
					else if (numericTypeFunctors.contains(functorName)) {
						result = TermCategoryType.NUMERIC;
					}
					else if (otherTypeFunctors.contains(functorName)) {
						result = TermCategoryType.OTHER;
					}
				}
			}
			
			return result;
		}
		
		private Expression attemptMakeRule(Expression conditional) {
			Expression result = null;
			
			TermCategoryType conditionType  = determineTermCategoryType(IfThenElse.condition(conditional));
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
		
		void validateFunctors(StatementInfo termStatement) {
			Iterator<Expression> subExpressionsIterator =  new SubExpressionsDepthFirstIterator(termStatement.statement);
		
			subExpressionsIterator.forEachRemaining(expr -> {
				if (expr.getFunctor() != null) {
					if (isKnownFunctor(expr.getFunctor())) {
						if (!isValidKnownFunctorArity(expr)) {
							newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, "'"+expr+"'", termStatement);
						}
// TODO - validate arguments are of correct types						
					}
					else if (isDeclaredRandomFunctor(expr.getFunctor())) {
						if (!isValidDeclaredRandomFunctorArity(expr)) {
							newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, "'"+expr+"'", termStatement);
						}
// TODO - validate arguments are of correct type						
					}
					else {					
						newError(Type.TERM_TYPE_OF_FUNCTOR_NOT_DECLARED, "'"+expr.getFunctor()+"'", termStatement);
					}
				}
				else if (isPrologConstant.apply(expr)) {				
					if (isDeclaredRandomFunctor(expr)) {
// TODO - this won't work when visiting the functor of a random variable with arity > 0.						
//						if (!isValidDeclaredRandomFunctorArity(expr)) {
//							newError(Type.TERM_ARITY_OF_FUNCTOR_DOES_NOT_MATCH_DECLARATION, "'"+expr+"'", termStatement);
//						}
					}
					else if (this.isKnownFunctor(expr) || Expressions.isNumber(expr)) {
						// ignore
					}
					else if (!sortConstants.contains(expr)) {
						newError(Type.TERM_CONSTANT_NOT_DEFINED, "'"+expr+"'", termStatement);
					}
				}
			});
		}
		
		boolean isKnownFunctor(Expression functorName) {
			boolean result = false;
			if (KNOWN_FUNCTORS.contains(functorName(functorName))) {
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

			if (KNOWN_ARITY_1_FUNCTORS.contains(functorName)
					&& expr.numberOfArguments() != 1) {
				result = false;
			}
			else if (KNOWN_ARITY_2_FUNCTORS.contains(functorName)
					&& expr.numberOfArguments() != 2) {
				result = false;
			}
			else if (KNOWN_ARITY_3_FUNCTORS.contains(functorName)
					&& expr.numberOfArguments() != 3) {
				result = false;
			}
			else if (KNOWN_ARITY_1_OR_2_FUNCTORS.contains(functorName)
					&& !(expr.numberOfArguments() == 1 || expr.numberOfArguments() == 2)) {
				result = false;
			}
			else if (KNOWN_ARITY_0_OR_MORE_FUNCTORS.contains(functorName) 
					&& expr.numberOfArguments() < 0) { // Should never happen
				result = false;
			}
			else if (KNOWN_ARITY_GREATER_THAN_1_FUNCTORS.contains(functorName) 
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
		
		String functorName(Expression exprFunctorName) {
			String result = exprFunctorName.getValue().toString();
			return result;
		}
		
		void newError(Type errorType, Object msg, StatementInfo statement) {
			errors.add(new HOGModelError(errorType, msg.toString(), statement));
		}
	}
}