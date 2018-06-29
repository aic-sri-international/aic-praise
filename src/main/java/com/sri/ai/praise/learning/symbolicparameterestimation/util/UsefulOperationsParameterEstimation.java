package com.sri.ai.praise.learning.symbolicparameterestimation.util;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.freeVariables;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIAL;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;

/**
 * Useful Operations that can be used for Parameter Estimation.
 * 
 * @author Sarah Perrin
 *
 */

public class UsefulOperationsParameterEstimation {
	
	public static List<Expression> findParameters(ExpressionBasedModel expressionBasedModel) {
		List<Expression> result = new LinkedList<Expression>();
		for (String parameterString : expressionBasedModel.getMapFromNonUniquelyNamedConstantNameToTypeName().keySet()) {
			Expression parameter = parse(parameterString);
			if(!result.contains(parameter)) {
				result.add(parameter);
			}
		}
		return result;
	}
	
	/**
	 * Sigmoid trick to have the parameters and result between 0 and 1.
	 *
	 */
	public static Expression applySigmoidTrick(Expression marginal, Context context) {
		Set<Expression> variablesInExpression = freeVariables(marginal, context);
		for (Expression arg : variablesInExpression) {
			Expression argChanged = apply(DIVISION, 1, apply(PLUS, 1, apply(EXPONENTIAL, apply(MINUS, arg))));
			marginal = marginal.replaceAllOccurrences(arg, argChanged, context);
		}
		return marginal;
	}

}
