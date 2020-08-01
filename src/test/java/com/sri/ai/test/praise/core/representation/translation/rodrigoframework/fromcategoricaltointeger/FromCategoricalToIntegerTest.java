package com.sri.ai.test.praise.core.representation.translation.rodrigoframework.fromcategoricaltointeger;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.fromcategoricaltointeger.FromCategoricalToInteger;
import org.junit.jupiter.api.Test;

import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.println;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FromCategoricalToIntegerTest {

	@Test
	void test() {

		ExpressionBasedModel model = new HOGMExpressionBasedModel(
				"sort People: 10, tom, mary, john;" +
				"sort Weekdays: 7, sunday, monday, tuesday, wednesday, thursday, friday, saturday;" +
				"random p: Boolean;" +
				"random boss: People;" +
				"random day: Weekdays;" +
				"if p and p = false and boss = mary and wednesday = day and " +
						"false and true or (not p) and true then 0.1 else 0.9;"
		);

		// TODO: using string because (p = 1) = false gets parsed as p = 1 = false; need to fix that.
		String expected =
				"if (p = 1) and ((p = 1) = false) and (boss = 1) and (3 = day) and " +
				"false and true or not (p = 1) and true then 0.1 else 0.9";

		FromCategoricalToInteger translator = new FromCategoricalToInteger(model);
		String actual = translator.getTranslation().getFactors().get(0).toString();

		println("           Original factor: " + model.getFactors().get(0));
		println("Expected translated factor: " + expected);
		println("  Actual translated factor: " + actual);
		println();
		println("Entire model:\n" + translator.getTranslation());

		assertEquals(expected, actual);

		var categoricalVariables =
				collectToList(
						model.getRandomVariables(),
						v -> model.getContext().getTypeOfRegisteredSymbol(v) instanceof Categorical);

		for (Expression variable: categoricalVariables) {
			assertVariableIsDefinedAsIntegerTyped(variable, translator);
		}
	}

	private void assertVariableIsDefinedAsIntegerTyped(Expression variable, FromCategoricalToInteger translator) {
		Context context = translator.getTranslation().getContext();
		Type type = context.getTypeOfRegisteredSymbol(variable);
		assertTrue(type instanceof IntegerInterval);
	}

}
