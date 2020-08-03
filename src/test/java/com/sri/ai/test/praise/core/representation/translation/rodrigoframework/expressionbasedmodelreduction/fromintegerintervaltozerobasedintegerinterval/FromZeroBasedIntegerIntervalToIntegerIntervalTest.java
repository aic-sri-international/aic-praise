package com.sri.ai.test.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval;

import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromcategoricaltointeger.FromCategoricalToInteger;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromcategoricaltointeger.FromIntegerToCategorical;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval.FromIntegerIntervalToZeroBasedIntegerInterval;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval.FromZeroBasedIntegerIntervalToIntegerInterval;
import org.junit.jupiter.api.Test;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FromZeroBasedIntegerIntervalToIntegerIntervalTest {

	@Test
	void test() {

		String zeroBasedExpressionString;
		String expectedExpressionString;
		String expectedSimplifiedString;
		ExpressionBasedModel model;

		model = new HOGMExpressionBasedModel(
				"random i: 10..20;" +
						"random j: 0..5;" +
						"if i + j = 10 and i < 20 and j < 3 then 0.1 else 0.9;"
		);
		zeroBasedExpressionString =
				"if ((i + 10) + j = 10) and i = 5 and (i + 10 < 20) and (j < 3) then 0.1 else 0.9";
		expectedExpressionString =
				"if (((i - 10) + 10) + j = 10) and i = 15 and ((i - 10) + 10 < 20) and (j < 3) then 0.1 else 0.9";
		expectedSimplifiedString = null;
		runTest(zeroBasedExpressionString, expectedExpressionString, expectedSimplifiedString, model);

//		zeroBasedExpressionString =
//				"boss = 0 or boss = 7";
//		expectedExpressionString =
//				"boss = tom or boss = people7";
//		expectedSimplifiedString = null; // TODO: theory solver is not dealing properly with unknown constants.
//		runTest(zeroBasedExpressionString, expectedExpressionString, expectedSimplifiedString, model);
	}

	private void runTest(
			String zeroBasedExpressionString,
			String expectedExpressionString,
			String expectedSimplifiedString,
			ExpressionBasedModel model) {

		var translator = new FromIntegerIntervalToZeroBasedIntegerInterval(model);
		var translatorBack = new FromZeroBasedIntegerIntervalToIntegerInterval(model, translator.getTranslation());

		var actual = translatorBack.translateBack(parse(zeroBasedExpressionString));
		println();
		println("Input     : " + zeroBasedExpressionString);
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
