package com.sri.ai.praise;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
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

	public static Map<Expression, Expression> compute(Expression expression, Model model, RewritingProcess process) {
		Map<Expression, Expression> result = new HashMap<Expression, Expression>();
		SubExpressionsDepthFirstIterator iterator = new SubExpressionsDepthFirstIterator(expression);
		while (iterator.hasNext()) {
			Expression subExpression = iterator.next();
			RandomVariableDeclaration declaration;
			if (subExpression.getSyntacticFormType().equals("Function application") &&
					(declaration =
					model.getRandomVariableDeclaration(subExpression.getFunctor().toString(), subExpression.numberOfArguments())
							) != null) {
				Map<Expression, Expression> sortsForThisSubExpression = getLogicalVariableArgumentsSorts(subExpression, declaration, process);
				result.putAll(sortsForThisSubExpression);
			}
		}
		return result;
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
}
