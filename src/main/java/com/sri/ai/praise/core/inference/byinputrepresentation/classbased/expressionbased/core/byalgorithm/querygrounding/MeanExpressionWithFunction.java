package com.sri.ai.praise.core.inference.byinputrepresentation.classbased.expressionbased.core.byalgorithm.querygrounding;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionWithProbabilityFunction;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.FromFunctionToProbabilityExpression;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.functions.MeanFunction;

public class MeanExpressionWithFunction extends AbstractExpressionWrapper implements ExpressionWithProbabilityFunction {

	private static final long serialVersionUID = 1L;

	private ExpressionWithProbabilityFunction base;

	public MeanExpressionWithFunction(ExpressionWithProbabilityFunction base) {
		this.base = base;
	}

	private MeanFunction meanFunction;
	
	@Override
	public com.sri.ai.util.function.api.functions.Function getDiscretizedConditionalProbabilityDistributionFunction() {
		if (meanFunction == null) {
			com.sri.ai.util.function.api.functions.Function baseFunction = 
					base.getDiscretizedConditionalProbabilityDistributionFunction();
			int baseQueryIndex = base.getDiscretizedConditionalProbabilityDistributionFunctionQueryIndex();
			SetOfVariables baseVariables = baseFunction.getSetOfInputVariables();
			Variable baseQuery = baseVariables.get(baseQueryIndex);
			meanFunction = new MeanFunction(baseQuery, baseFunction);
		}
		return meanFunction;
	}

	@Override
	public int getDiscretizedConditionalProbabilityDistributionFunctionQueryIndex() {
		return -1;
	}

	@Override
	protected Expression computeInnerExpression() {
		FromFunctionToProbabilityExpression expressionMaker = 
				new FromFunctionToProbabilityExpression(
						getDiscretizedConditionalProbabilityDistributionFunction(),
						getDiscretizedConditionalProbabilityDistributionFunctionQueryIndex());
		Expression result = expressionMaker.getFactorExpression();
		return result;
	}
}
