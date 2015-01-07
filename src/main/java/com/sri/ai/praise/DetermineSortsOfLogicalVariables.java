package com.sri.ai.praise;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.RandomVariableDeclaration;

/**
 * A collection of methods for obtaining the types of logical variables based on their index expressions
 * in intensional sets and usage in random variable value expressions.
 * 
 * @author braz
 *
 */
public class DetermineSortsOfLogicalVariables {

	/**
	 * Determines a map from an intensional set's indices to their types, obtaining this information from both index expressions and,
	 * when those do not contain the types, from usage as random variable value expressions arguments (using the random variable declaration from a given Model).
	 * Throws an error if types as defined by the index expressions or contextual symbol types (from the process) conflict with random variable value expression argument usage. 
	 */
	public static Map<Expression, Expression> getIndicesTypeMapFromIntensionalSetIndexExpressionsAndUsageInRandomVariables(Expression intensionalSet, RewritingProcess process) {
		Map<Expression, Expression> result = new LinkedHashMap<Expression, Expression>(IndexExpressions.getIndexToTypeMapWithDefaultNull(intensionalSet));
		RewritingProcess subProcess = GrinderUtil.extendContextualSymbols(result, process);
		result = extendFreeSymbolsAndTypesFromUsageInRandomVariables(result, ((IntensionalSet) intensionalSet).getHead(), null, subProcess);
		return result;
	}

	/**
	 * gets a map with free variables and their types in the given expression,
	 * using their usage in the expression as arguments of random variables to determine type.
	 * Uses the given random variable declarations (in expression form) for that purpose, or if this
	 * is null, the random variable declarations present in the model.
	 */
	public static Map<Expression, Expression> getFreeSymbolsAndTypesFromUsageInRandomVariables(Expression expression, Set<Expression> randomVariableDeclarationExpressions, RewritingProcess process) {
		Map<Expression, Expression> result = extendFreeSymbolsAndTypesFromUsageInRandomVariables(new LinkedHashMap<Expression, Expression>(), expression, randomVariableDeclarationExpressions, process);
		return result;
	}

	/**
	 * Extends a given map with free variables and their types in the given expression,
	 * using their usage in the expression as arguments of random variables to determine type.
	 * Uses the given random variable declarations (in expression form) for that purpose, or if this
	 * is null, the random variable declarations present in the model.
	 */
	public static Map<Expression, Expression> extendFreeSymbolsAndTypesFromUsageInRandomVariables(Map<Expression, Expression> freeSymbolsAndTypes, Expression expression, Set<Expression> randomVariableDeclarationExpressions, RewritingProcess process) {
		try {
			Collection<RandomVariableDeclaration> randomVariableDeclarations = null;
			if (randomVariableDeclarationExpressions == null) {
				Model model = Model.getRewritingProcessesModel(process);
				if (model != null) {
					randomVariableDeclarations = model.getRandomVariableDeclarations();
				}
			}
			else {
				randomVariableDeclarations = getRandomVariableDeclarationObjectsFromExpressions(randomVariableDeclarationExpressions);
			}
			process.putGlobalObject(GrinderUtil.DO_NOT_REQUIRE_ADDED_CONTEXTUAL_CONSTRAINT_FREE_SYMBOLS_TO_BE_IN_CONTEXTUAL_VARIABLES, true);
			expression.replaceAllOccurrences(new CollectFreeSymbolsAndTypesFromUsageInRandomVariables(freeSymbolsAndTypes, randomVariableDeclarations), process);
			process.removeGlobalObject(GrinderUtil.DO_NOT_REQUIRE_ADDED_CONTEXTUAL_CONSTRAINT_FREE_SYMBOLS_TO_BE_IN_CONTEXTUAL_VARIABLES);
		}
		catch (Error e) {
			// no model in process, do nothing.
		}
		return freeSymbolsAndTypes;
	}

	private static Collection<RandomVariableDeclaration> getRandomVariableDeclarationObjectsFromExpressions(Set<Expression> randomVariableDeclarationExpressions) {
		Set<RandomVariableDeclaration> randomVariableDeclarations = new LinkedHashSet<RandomVariableDeclaration>();
		for (Expression randomVariableDeclarationExpression : randomVariableDeclarationExpressions) {
			RandomVariableDeclaration randomVariableDeclaration =
					RandomVariableDeclaration
					.makeRandomVariableDeclaration(randomVariableDeclarationExpression);
			randomVariableDeclarations.add(randomVariableDeclaration);
		}
		return randomVariableDeclarations;
	}

	public static class CollectFreeSymbolsAndTypesFromUsageInRandomVariables extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		private Map<Expression, Expression> freeSymbolsAndTypes;
		private Collection<RandomVariableDeclaration> randomVariableDeclarations;
		
		public CollectFreeSymbolsAndTypesFromUsageInRandomVariables(Map<Expression, Expression> freeSymbolsAndTypes, Collection<RandomVariableDeclaration> randomVariableDeclarations) {
			super();
			this.freeSymbolsAndTypes    = freeSymbolsAndTypes;
			this.randomVariableDeclarations = randomVariableDeclarations;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			if (isLogicalVariable(expression, process) && ! process.getContextualSymbols().contains(expression) && ! freeSymbolsAndTypes.containsKey(expression)) {
				// it may be that a logical variable is used without a random variable, or with a random variable without a declaration, so we need to include them without a type.
				freeSymbolsAndTypes.put(expression, null);
			}
			else {
				if (BracketedExpressionSubExpressionsProvider.isRandomVariable(expression, process)) {
					// we must use the expression with the random variable value expressions
					expression = ((BracketedExpression) expression).getInnerExpression();
				}
				
				if (LPIUtil.isRandomVariableValueExpression(expression, process)) {
					RandomVariableDeclaration declaration = LPIUtil.getRandomVariableDeclaration(expression, randomVariableDeclarations);
					Map<Expression, Expression> sortsForThisSubExpression = getLogicalVariableArgumentsDomains(expression, declaration, process);
					for (Map.Entry<Expression, Expression> entry : sortsForThisSubExpression.entrySet()) {
						Expression previousDomainIfAny = freeSymbolsAndTypes.get(entry.getKey());
						if (previousDomainIfAny != null) {
							if (entry.getValue() == null) {
								entry.setValue(previousDomainIfAny);
							}
							else if ( ! previousDomainIfAny.equals(entry.getValue())) {
								throw new Error(
										"Conflicting type informaton. Logical variable " + entry.getKey() + " has been used as " + previousDomainIfAny +
										" somewhere else, but used as " + entry.getValue() + " in expression " + expression);
							}
						}
						else {
							previousDomainIfAny = process.getContextualSymbolType(entry.getKey());
							if (previousDomainIfAny != null) {
								if (entry.getValue() == null) {
									entry.setValue(previousDomainIfAny);
								}
								else if ( ! previousDomainIfAny.equals(entry.getValue())) {
									throw new Error(
											"Conflicting type informaton. Logical variable " + entry.getKey() + " is registered as " + previousDomainIfAny +
											" in context, but used as " + entry.getValue() + " in expression " + expression);
								}
							}
							else if ( ! process.getContextualSymbols().contains(entry.getKey())) { // if it is in the context, it is not a free variable.
								freeSymbolsAndTypes.put(entry.getKey(), entry.getValue());
							}
						}
					}
				}
			}
			return expression; // not used as a replacement function, so it always returns the input
		}
	}

	public static Map<Expression, Expression>
	getLogicalVariableArgumentsDomains(Expression expression, RandomVariableDeclaration declaration, RewritingProcess process) {
		Map<Expression, Expression> result = new LinkedHashMap<Expression, Expression>();
		List<Expression> sorts = declaration != null? declaration.getParameterSorts() : null;
		for (int i = 0; i != expression.numberOfArguments(); i++) {
			Expression argument = expression.get(i);
			if (isLogicalVariable(argument, process)) {
				Expression sort = sorts != null? sorts.get(i) : null;
				result.put(argument, sort);
			}
		}
		return result;
	}

	private static boolean isLogicalVariable(Expression expression, RewritingProcess process) {
		boolean result = expression.getSyntacticFormType().equals("Symbol") && ! process.isUniquelyNamedConstant(expression);
		return result;
	}
}
