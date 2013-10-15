package com.sri.ai.praise;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.RandomVariableDeclaration;

/**
 * Receives an expression without scoping expressions,
 * and returns a map from its logical variables and their sorts,
 * according to random variable declarations in a given Model.
 * All logical variables are assumed to appear at least once as an argument for a random variable.
 * 
 * @author braz
 *
 */
public class DetermineSortsOfLogicalVariables {

	public static Map<Expression, Expression>
	completeWithSortsFromRandomVariableUsageInAllSubExpressions(
			Map<Expression, Expression> freeVariableToDomainMap, Expression expression, Model model, RewritingProcess process) {
		
		SubExpressionsDepthFirstIterator iterator = new SubExpressionsDepthFirstIterator(expression);
		while (iterator.hasNext()) {
			Expression subExpression = iterator.next();
			RandomVariableDeclaration declaration = model.getRandomVariableDeclaration(subExpression);
			if (declaration != null) {
				completeWithSortsFromRandomVariableUsage(freeVariableToDomainMap, subExpression, declaration, process);
			}
		}
		return freeVariableToDomainMap;
	}

	private static void completeWithSortsFromRandomVariableUsage(Map<Expression, Expression> freeVariableToDomainMap, Expression expression, RandomVariableDeclaration declaration, RewritingProcess process) {
		Map<Expression, Expression> sortsForThisSubExpression = getLogicalVariableArgumentsSorts(expression, declaration, process);
		for (Map.Entry<Expression, Expression> entry : sortsForThisSubExpression.entrySet()) {
			Expression previousDomainIfAny = freeVariableToDomainMap.get(entry.getKey());
			if (previousDomainIfAny != null && ! previousDomainIfAny.equals(entry.getValue())) {
				throw new Error(
						"Conflicting type informaton. " + entry.getKey() + " is registered as " + previousDomainIfAny +
						" but used as " + entry.getValue() + " in expression " + expression);
			}
			else {
				freeVariableToDomainMap.put(entry.getKey(), entry.getValue());
			}
		}
	}

	private static Map<Expression, Expression>
	getLogicalVariableArgumentsSorts(Expression expression, RandomVariableDeclaration declaration, RewritingProcess process) {
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
	 
	private static Map<Expression, Expression> getSortsOfIndices(Expression intensionalSet, Model model, RewritingProcess process) {
		Map<Expression, Expression> indexToDomainMap = IntensionalSet.getIndexToDomainMap(intensionalSet);
		indexToDomainMap = completeWithSortsFromRandomVariableUsageInAllSubExpressions(
				indexToDomainMap,
				Tuple.make(IntensionalSet.getHead(intensionalSet), IntensionalSet.getCondition(intensionalSet)),
				model,
				process);
		return indexToDomainMap;
	}
}
