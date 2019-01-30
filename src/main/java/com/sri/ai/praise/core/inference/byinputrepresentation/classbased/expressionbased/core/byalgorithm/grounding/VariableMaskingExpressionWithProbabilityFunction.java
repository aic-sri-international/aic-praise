package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.grounding;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;

import java.util.ArrayList;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionWithProbabilityFunction;
import com.sri.ai.util.function.api.functions.Function;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.functions.VariablesMaskingFunction;

public class VariableMaskingExpressionWithProbabilityFunction extends AbstractExpressionWrapper implements ExpressionWithProbabilityFunction {

	public static VariableMaskingExpressionWithProbabilityFunction mask(
			ExpressionWithProbabilityFunction base,
			String oldVariableName, 
			String newVariableName) {
		
		return new VariableMaskingExpressionWithProbabilityFunction(base, oldVariableName, newVariableName);
	}
	
	public static VariableMaskingExpressionWithProbabilityFunction mask(
			ExpressionWithProbabilityFunction base,
			Expression oldVariable, 
			Expression newVariable) {
		
		return new VariableMaskingExpressionWithProbabilityFunction(base, oldVariable.toString(), newVariable.toString());
	}
	
	private static final long serialVersionUID = 1L;
	
	private ExpressionWithProbabilityFunction base;
	private String oldVariableName;
	private String newVariableName;
	
	public VariableMaskingExpressionWithProbabilityFunction(
			ExpressionWithProbabilityFunction base,
			String oldVariableName, 
			String newVariableName) {
		
		this.base = base;
		this.oldVariableName = oldVariableName;
		this.newVariableName = newVariableName;
	}

	@Override
	public Function getDiscretizedConditionalProbabilityDistributionFunction() {
		Function baseFunction = base.getDiscretizedConditionalProbabilityDistributionFunction();
		Variable oldVariable = getOldVariable(baseFunction);
		Variable newVariable = oldVariable.copyWithNewName(newVariableName);
		return new VariablesMaskingFunction(baseFunction, oldVariable, newVariable);
	}

	@Override
	public int getDiscretizedConditionalProbabilityDistributionFunctionQueryIndex() {
		return base.getDiscretizedConditionalProbabilityDistributionFunctionQueryIndex();
	}

	private Variable getOldVariable(Function baseFunction) {
		ArrayList<? extends Variable> oldVariables = baseFunction.getSetOfInputVariables().getVariables();
		Variable oldVariable = getFirstSatisfyingPredicateOrNull(oldVariables, v -> v.getName().equals(oldVariableName));
		return oldVariable;
	}

	@Override
	protected Expression computeInnerExpression() {
		Expression result = base.replaceAllOccurrences(parse(oldVariableName), parse(newVariableName), new TrueContext());
		return result;
	}
}