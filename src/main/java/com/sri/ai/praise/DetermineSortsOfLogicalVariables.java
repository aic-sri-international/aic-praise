package com.sri.ai.praise;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.RandomVariableDeclaration;
import com.sri.ai.util.collect.StackedHashMap;
import com.sri.ai.util.collect.StackedMap;

/**
 * A collection of methods for obtaining the domains of logical variables based on their index expressions in intensional sets and usage in random variable value expressions.
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
	public static Map<Expression, Expression> getIndicesDomainMapFromIntensionalSetIndexExpressionsAndUsageInRandomVariables(Expression intensionalSet, Model model, RewritingProcess process) {
		StackedMap<Expression, Expression> newContextualVariablesDomains = new StackedHashMap<Expression, Expression>(process.getContextualVariablesDomains());
		newContextualVariablesDomains.putAll(IntensionalSet.getIndexToDomainMap(intensionalSet));
		newContextualVariablesDomains = (StackedMap<Expression, Expression>)
				completeWithDomainsFromRandomVariableUsageInAllSubExpressions(
						newContextualVariablesDomains,
						Tuple.make(IntensionalSet.getHead(intensionalSet), IntensionalSet.getCondition(intensionalSet)),
						model,
						process);
		Map<Expression, Expression> result = newContextualVariablesDomains.getTop(); // extract only newly-defined indices
		return result;
	}

	/**
	 * Extends a given map of logical variable domains with domain information from usage of logical variables as arguments of a random variable value expressions in a given expression.
	 * If the logical variable already has a domain defined in the map, throws an error if the usage suggests a different domain.
	 */
	public static
	Map<Expression, Expression>
	completeWithDomainsFromRandomVariableUsageInAllSubExpressions(
			Map<Expression, Expression> domainMap, Expression expression, Model model, RewritingProcess process) {
		
		SubExpressionsDepthFirstIterator iterator = new SubExpressionsDepthFirstIterator(expression);
		while (iterator.hasNext()) {
			Expression subExpression = iterator.next();
			RandomVariableDeclaration declaration = model.getRandomVariableDeclaration(subExpression);
			if (declaration != null) {
				completeWithDomainsFromRandomVariableUsage(domainMap, subExpression, declaration, process);
			}
		}
		return domainMap;
	}

	/**
	 * Extends a given map of logical variable domains with domain information from usage of logical variables as arguments of a random variable value expression,
	 * the declaration of which is also provided.
	 * If the logical variable already has a domain defined in the map, throws an error if the usage suggests a different domain.
	 */
	public static void completeWithDomainsFromRandomVariableUsage(Map<Expression, Expression> domainMap, Expression expression, RandomVariableDeclaration declaration, RewritingProcess process) {
		Map<Expression, Expression> sortsForThisSubExpression = getLogicalVariableArgumentsDomains(expression, declaration, process);
		for (Map.Entry<Expression, Expression> entry : sortsForThisSubExpression.entrySet()) {
			Expression previousDomainIfAny = domainMap.get(entry.getKey());
			if (previousDomainIfAny != null && ! previousDomainIfAny.equals(entry.getValue())) {
				throw new Error(
						"Conflicting type informaton. " + entry.getKey() + " is registered as " + previousDomainIfAny +
						" but used as " + entry.getValue() + " in expression " + expression);
			}
			else {
				domainMap.put(entry.getKey(), entry.getValue());
			}
		}
	}

	private static Map<Expression, Expression>
	getLogicalVariableArgumentsDomains(Expression expression, RandomVariableDeclaration declaration, RewritingProcess process) {
		Map<Expression, Expression> result = new HashMap<Expression, Expression>();
		List<Expression> sorts = declaration.getParameterSorts();
		for (int i = 0; i != declaration.getArityValue(); i++) {
			Expression argument = expression.get(i);
			if (isLogicalVariable(argument, process)) {
				Expression sort = sorts.get(i);
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
