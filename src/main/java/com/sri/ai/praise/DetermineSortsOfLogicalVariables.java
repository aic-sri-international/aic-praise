package com.sri.ai.praise;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.RandomVariableDeclaration;

/**
 * A collection of methods for obtaining the domains of logical variables based on their index expressions
 * in intensional sets and usage in random variable value expressions.
 * 
 * @author braz
 *
 */
public class DetermineSortsOfLogicalVariables {

	/**
	 * Determines a map from an intensional set's indices to their domains, obtaining this information from both index expressions and,
	 * when those do not contain the domains, from usage as random variable value expressions arguments (using the random variable declaration from a given Model).
	 * Throws an error if domains as defined by the index expressions or contextual variable domains (from the process) conflict with random variable value expression argument usage. 
	 */
	public static Map<Expression, Expression> getIndicesDomainMapFromIntensionalSetIndexExpressionsAndUsageInRandomVariables(Expression intensionalSet, RewritingProcess process) {
		Map<Expression, Expression> result = new HashMap<Expression, Expression>(IntensionalSet.getIndexToDomainMapWithDefaultNull(intensionalSet));
		RewritingProcess subProcess = GrinderUtil.extendContextualVariables(result, process);
		result = extendFreeVariablesAndDomainsFromUsageInRandomVariables(result, IntensionalSet.getHead(intensionalSet), subProcess);
		return result;
	}

	public static Map<Expression, Expression> getFreeVariablesAndDomainsFromUsageInRandomVariables(Expression expression, RewritingProcess process) {
		Map<Expression, Expression> result = extendFreeVariablesAndDomainsFromUsageInRandomVariables(new HashMap<Expression, Expression>(), expression, process);
		return result;
	}

	public static Map<Expression, Expression> extendFreeVariablesAndDomainsFromUsageInRandomVariables(Map<Expression, Expression> freeVariablesAndDomains, Expression expression, RewritingProcess process) {
		try {
			Model model = Model.getRewritingProcessesModel(process);
			process.putGlobalObject(GrinderUtil.DO_NOT_REQUIRE_ADDED_CONTEXTUAL_CONSTRAINT_FREE_VARIABLES_TO_BE_IN_CONTEXTUAL_VARIABLES, true);
			expression.replaceAllOccurrences(new CollectFreeVariablesAndDomainsFromUsageInRandomVariables(freeVariablesAndDomains, model), process);
			process.removeGlobalObject(GrinderUtil.DO_NOT_REQUIRE_ADDED_CONTEXTUAL_CONSTRAINT_FREE_VARIABLES_TO_BE_IN_CONTEXTUAL_VARIABLES);
		}
		catch (Error e) {
			// no model in process, do nothing.
		}
		return freeVariablesAndDomains;
	}

	public static class CollectFreeVariablesAndDomainsFromUsageInRandomVariables implements ReplacementFunctionWithContextuallyUpdatedProcess {
		private Map<Expression, Expression> freeVariablesAndDomains;
		private Model model;
		
		public CollectFreeVariablesAndDomainsFromUsageInRandomVariables(Map<Expression, Expression> freeVariablesAndDomains, Model model) {
			super();
			this.freeVariablesAndDomains = freeVariablesAndDomains;
			this.model = model;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			if (isLogicalVariable(expression, process) && ! process.getContextualVariables().contains(expression) && ! freeVariablesAndDomains.containsKey(expression)) {
				// it may be that a logical variable is used without a random variable, or with a random variable without a declaration, so we need to include them without a domain.
				freeVariablesAndDomains.put(expression, null);
			}
			else {
				if (BracketedExpressionSubExpressionsProvider.isRandomVariable(expression, process)) {
					// we must use the expression with the random variable value expressions
					expression = BracketedExpressionSubExpressionsProvider.getRandomVariableValueExpression(expression);
				}
				
				if (LPIUtil.isRandomVariableValueExpression(expression, process)) {
					RandomVariableDeclaration declaration = model.getRandomVariableDeclaration(expression);
					Map<Expression, Expression> sortsForThisSubExpression = getLogicalVariableArgumentsDomains(expression, declaration, process);
					for (Map.Entry<Expression, Expression> entry : sortsForThisSubExpression.entrySet()) {
						Expression previousDomainIfAny = freeVariablesAndDomains.get(entry.getKey());
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
							previousDomainIfAny = process.getContextualVariableDomain(entry.getKey());
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
							else if ( ! process.getContextualVariables().contains(entry.getKey())) { // if it is in the context, it is not a free variable.
								freeVariablesAndDomains.put(entry.getKey(), entry.getValue());
							}
						}
					}
				}
			}
			return expression; // not used as a replacement function, so it always returns the input
		}
	
		@Override
		public Expression apply(Expression expression) {
			throw new UnsupportedOperationException("CollectFreeVariablesAndDomainsFromUsageInRandomVariables.evaluate(Expression) should not be invoked.");
		}
	}

	public static Map<Expression, Expression>
	getLogicalVariableArgumentsDomains(Expression expression, RandomVariableDeclaration declaration, RewritingProcess process) {
		Map<Expression, Expression> result = new HashMap<Expression, Expression>();
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
		boolean result = expression.getSyntacticFormType().equals("Symbol") && ! process.isConstant(expression);
		return result;
	}
}
