package com.sri.ai.praise.core.representation.translation.rodrigoframework.fromexpressionstosamplingfactors;

import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.expresso.helper.Expressions.isSymbol;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.util.Util.notNullAndEquals;

import java.util.LinkedList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

/**
 * Extracts and represents the histogram implicitly specified in an expression of the form
 * <pre>
 * if <variable> < <v_1> then <p_1> else
 * if <variable> < <v_2> then <p_2> else
 * ...
 * if <variable> < <v_n> then <p_n> else
 * 0
 * </pre>
 * where <code><</code> may also be <code><=</code>.
 * <p>
 * The class is an extension of a list of pairs of expressions
 * representing <code>v1</code> and <code>p_1</code>.
 * <p>
 * It also represents the variable as a property.
 * <p>
 * The method {@link #getHistogramOrNull(Expression)}
 * analyzes an expression and returns the histogram
 * if it conforms to the pattern above, or null otherwise.
 * 
 * @author braz
 *
 */
public class Histogram extends LinkedList<Pair<Expression, Expression>> {

	private static final long serialVersionUID = 1L;
	
	private Expression variable;

	public Histogram(Expression variable) {
		this.variable = variable;
	}

	public Expression getVariable() {
		return variable;
	}
	
	static Histogram getHistogramOrNull(Expression expression) {
		Expression histogramVariable = getHistogramVariable(expression);
		if (histogramVariable == null) {
			return null;
		}
		Histogram histogram = new Histogram(histogramVariable);
		histogram = collectHistogram(expression, histogram);
		return histogram;
	}

	private static Histogram collectHistogram(Expression expression, Histogram histogram) {

		Expression current = expression;
		while (notNullAndEquals(getHistogramVariable(current), histogram.getVariable())) {
			Pair<Expression, Expression> valueAndProbability = getValueAndProbability(current);
			if (valueAndProbability == null) {
				return null;
			}
			histogram.add(valueAndProbability);
			current = IfThenElse.elseBranch(current);
		}
		
		if (current.equals(ZERO)) {
			return histogram;
		}
		else {
			return null;
		}
	}

	private static Expression getHistogramVariable(Expression expression) {
		Expression variable = 
				getHistogramFirstClauseInformation(
						expression, 
						(condition, thenClause) -> condition.get(0));
		return variable;
	}

	private static Pair<Expression, Expression> getValueAndProbability(Expression expression) {
		Pair<Expression, Expression> result = 
				getHistogramFirstClauseInformation(
						expression, 
						(condition, thenClause) -> Pair.make(condition.get(1), thenClause));
		return result;
	}

	private static <T> 
	T 
	getHistogramFirstClauseInformation(
			Expression expression, 
			BinaryFunction<Expression, Expression, T> fromConditionAndThenBranchToInformation) {
		
		if (isIfThenElse(expression)) {
			Expression condition = IfThenElse.condition(expression);
			Expression thenBranch = IfThenElse.thenBranch(expression);
			if (Expressions.isApplicationOfLessThanOrLessThanOrEqualTo(condition) && isNumber(thenBranch)) {
				if (isSymbol(condition.get(0)) && isNumber(condition.get(1))) {
					return fromConditionAndThenBranchToInformation.apply(condition, thenBranch);
				}
			}
		}
		return null;
	}

}