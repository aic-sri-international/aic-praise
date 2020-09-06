package com.sri.ai.praise.core.representation.translation;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeOfExpression;
import static com.sri.ai.grinder.library.set.Sets.isIntensionalSet;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

public class NonBooleanFactorError extends AssertionError {

	private static final long serialVersionUID = 1L;

	private Expression factor;
	private String reason;

	public NonBooleanFactorError(Expression factor, String reason) {
		this.factor = factor;
		this.reason = reason;
	}

	public Expression getFactor() {
		return factor;
	}

	public String getReason() {
		return reason;
	}

	@Override
	public String getMessage() {
		return reason.toString();
	}

	/**
	 * Returns "" if expression is a valid factor (a numeric expression)
	 * or the reason it is not otherwise.
	 * @param expression
	 * @param context
	 * @return
	 */
	public static String expressionIsFactor(Expression expression, Registry context) {
		String result;
		if (IfThenElse.isIfThenElse(expression)) {
			Expression condition = IfThenElse.condition(expression);
			Expression thenBranch = IfThenElse.thenBranch(expression);
			Expression elseBranch = IfThenElse.elseBranch(expression);
			if ( ! isBooleanTyped(condition, context)) {
				result = condition + " must be boolean, but is " + GrinderUtil.getTypeOfExpression(condition, context);
			}
			else {
				if ((thenBranch.equals(ONE) && elseBranch.equals(ZERO)) || (thenBranch.equals(ZERO) && elseBranch.equals(ONE))) {
					result = "";
				}
				else {

					String reason1 = expressionIsFactor(thenBranch, context);
					String reason2 = expressionIsFactor(elseBranch, context);
					if ((reason1 + reason2).equals("")) {
						result = "";
					}
					else {
						result = join(" and ", list(reason1, reason2));
					}
				}
			}
		}
		else if (isProductOfSet(expression)) {
			IntensionalSet intensionalSet = (IntensionalSet) expression.get(0);
			Expression body = intensionalSet.getHead();
			Registry contextWithSetIndices = context.extendWith(intensionalSet.getIndexExpressions());
			result = expressionIsFactor(body, contextWithSetIndices);
		}
		else if (isNumber(expression)) {
			result = "";
		}
		else {
			result = expression + " should be a boolean-typed expression";
			// actually, it should have been either an if then else with constant numeric leaves, or a constant numeric leaf
			// However, if we get at this point, it's likely expression was meant to be a boolean B that would be expanded into if B then 1 else 0,
			// so saying it should be a boolean expression will be more understandable and useful.
		}
		return result;
	}

	public static boolean isNumericTyped(Expression expression, Registry context) {
		Type type = getTypeOfExpression(expression, context);
		return isNumericType(type);
	}

	public static boolean isNumericType(Type type) {
		return 
				type instanceof IntegerExpressoType 
				|| 
				type instanceof RealExpressoType 
				|| 
				type instanceof RealInterval 
				|| 
				type instanceof IntegerInterval;
	}

	public static boolean isBooleanTyped(Expression expression, Registry context) {
		Type type = getTypeOfExpression(expression, context);
		myAssert(type != null, () -> "Could not determine the type of: " + expression); 
		String typeName = type.getName();
		boolean result = typeName.equals("Boolean");
		return result;
	}

	public static boolean isProductOfSet(Expression expression) {
		boolean result = 
				expression.hasFunctor("product") 
				&& 
				isIntensionalSet(expression.get(0));
		return result;
	}

	public static void assertExpressionIsValidFactor(Expression expression, Registry context) {
		String reasonsItIsNot = expressionIsFactor(expression, context);
		if (reasonsItIsNot.length() > 0) {
			throw new NonBooleanFactorError(expression, reasonsItIsNot);
		}
	}
}