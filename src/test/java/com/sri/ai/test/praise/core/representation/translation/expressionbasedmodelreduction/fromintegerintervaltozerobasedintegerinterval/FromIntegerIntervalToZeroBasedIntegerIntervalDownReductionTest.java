package com.sri.ai.test.praise.core.representation.translation.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval.FromIntegerIntervalToZeroBasedIntegerIntervalDownReduction;
import org.junit.jupiter.api.Test;

import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.println;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FromIntegerIntervalToZeroBasedIntegerIntervalDownReductionTest {

	@Test
	void test() {

		ExpressionBasedModel model = new HOGMExpressionBasedModel(
				"random i: 10..20;" +
				"random j: 0..5;" +
				"if i + j = 10 and i < 20 and j < 3 then 0.1 else 0.9;"
		);

		String expected = "if ((i + 10) + j = 10) and (i + 10 < 20) and (j < 3) then 0.1 else 0.9";

		FromIntegerIntervalToZeroBasedIntegerIntervalDownReduction translator = new FromIntegerIntervalToZeroBasedIntegerIntervalDownReduction(model);
		String actual = translator.getTranslation().getFactors().get(0).toString();

		println("           Original factor: " + model.getFactors().get(0));
		println("Expected translated factor: " + expected);
		println("  Actual translated factor: " + actual);
		println();
		println("Entire model:\n" + translator.getTranslation());

		assertEquals(expected, actual);

		var integerVariables =
				collectToList(
						model.getRandomVariables(),
						v -> model.getContext().getTypeOfRegisteredSymbol(v) instanceof IntegerInterval);

		for (Expression variable: integerVariables) {
			assertVariableIsDefinedAsIntegerTyped(variable, translator);
		}
	}

	private void assertVariableIsDefinedAsIntegerTyped(
			Expression variable,
			FromIntegerIntervalToZeroBasedIntegerIntervalDownReduction translator) {

		Context context = translator.getTranslation().getContext();
		Type type = context.getTypeOfRegisteredSymbol(variable);
		assertTrue(type instanceof IntegerInterval);
		var integerInterval = (IntegerInterval) type;
		assertEquals(Expressions.ZERO, integerInterval.getNonStrictLowerBound());
	}

}
