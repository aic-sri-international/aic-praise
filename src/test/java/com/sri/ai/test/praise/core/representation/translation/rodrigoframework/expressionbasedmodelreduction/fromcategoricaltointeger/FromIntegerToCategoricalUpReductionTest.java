package com.sri.ai.test.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromcategoricaltointeger;

import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromcategoricaltointeger.FromCategoricalToIntegerDownReduction;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromcategoricaltointeger.FromIntegerToCategoricalUpReduction;
import org.junit.jupiter.api.Test;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.collectToList;
import static com.sri.ai.util.Util.println;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FromIntegerToCategoricalUpReductionTest {

	@Test
	void test() {

		String integerExpressionString;
		String expectedExpressionString;
		String expectedSimplifiedString;
		ExpressionBasedModel model;

		model = new HOGMExpressionBasedModel(
				"sort People: 10, tom, mary, john;" +
						"sort Weekdays: 7, sunday, monday, tuesday, wednesday, thursday, friday, saturday;" +
						"random p: Boolean;" +
						"random boss: People;" +
						"random day: Weekdays;" +
						"if p and p = false and boss = mary and wednesday = day and " +
						"false and true or (not p) and true then 0.1 else 0.9;"
		);

		// TODO: using string because (p = 1) = false gets parsed as p = 1 = false; need to fix that.
		integerExpressionString =
				"if (p = 1) and ((p = 1) = false) and (boss = 1) and (3 = day) and " +
						"false and true or ((p = 1) = false) and true then 0.1 else 0.9";
		expectedExpressionString =
				"if p and (p = false) and (boss = mary) and (wednesday = day) and " +
						"false and true or (p = false) and true then 0.1 else 0.9";
		expectedSimplifiedString = "if not p then 0.1 else 0.9";
		runTest(integerExpressionString, expectedExpressionString, expectedSimplifiedString, model);

		integerExpressionString =
				"boss = 0 or boss = 7";
		expectedExpressionString =
				"boss = tom or boss = people7";
		expectedSimplifiedString = null; // TODO: theory solver is not dealing properly with unknown constants.
		runTest(integerExpressionString, expectedExpressionString, expectedSimplifiedString, model);
	}

	private void runTest(String integerExpressionString, String expectedExpressionString, String expectedSimplifiedString, ExpressionBasedModel model) {
		FromCategoricalToIntegerDownReduction translator = new FromCategoricalToIntegerDownReduction(model);
		FromIntegerToCategoricalUpReduction translatorBack = new FromIntegerToCategoricalUpReduction(model, translator.getTranslation());

		var actual = translatorBack.translateBack(parse(integerExpressionString));
		println();
		println("Input     : " + integerExpressionString);
		println("Expected  : " + expectedExpressionString);
		println("Actual    : " + actual);
		assertEquals(parse(expectedExpressionString), actual);

		if (expectedSimplifiedString != null) {
			var simplified = new CommonTheory().evaluate(actual, model.getContext());
			println("Expected simplified: " + expectedSimplifiedString);
			println("Actual   simplified: " + simplified);
			assertEquals(parse(expectedSimplifiedString), simplified);
		}
	}


}
