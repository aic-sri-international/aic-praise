package com.sri.ai.test.praise.core.representation.translation.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval;

import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval.FromIntegerIntervalToZeroBasedIntegerIntervalDownReduction;
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromintegerintervaltozerobasedintegerinterval.FromZeroBasedIntegerIntervalToIntegerIntervalUpReduction;
import org.junit.jupiter.api.Test;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FromZeroBasedIntegerIntervalToIntegerIntervalUpReductionTest {

	@Test
	void test() {

		String zeroBasedExpressionString;
		String expectedExpressionString;
		String expectedSimplifiedString;
		ExpressionBasedModel model;

		model = new HOGMExpressionBasedModel(
				"random i: 10..20;" +
						"random k: 10..20" +
						"random j: 0..5;" +
						"if i + j = 10 and i < 20 and j < 3 then 0.1 else 0.9;"
		);
		zeroBasedExpressionString =
				"if ((i + 10) + j = 10) and i = 5 and i < 10 and 10 > i and i != 7 " +
						"and i > j + 2 and i = k and i = j and (i + 10 < 20) and (j < 3) then 0.1 else 0.9";
		expectedExpressionString =
				"if (((i - 10) + 10) + j = 10) and i = 15 and i < 20 and 20 > i and i != 17 " +
						"and ((i - 10) > j + 2) and i = k and (i - 10) = j and ((i - 10) + 10 < 20) and (j < 3) then 0.1 else 0.9";
		expectedSimplifiedString = null;
		runTest(zeroBasedExpressionString, expectedExpressionString, expectedSimplifiedString, model);
	}

	private void runTest(
			String zeroBasedExpressionString,
			String expectedExpressionString,
			String expectedSimplifiedString,
			ExpressionBasedModel model) {

		println();
		println("Input     : " + zeroBasedExpressionString);
		println("Expected  : " + expectedExpressionString);

		var translator = new FromIntegerIntervalToZeroBasedIntegerIntervalDownReduction(model);
		var translatorBack = new FromZeroBasedIntegerIntervalToIntegerIntervalUpReduction(model, translator.getTranslatedModel());

		var actual = translatorBack.translateBack(parse(zeroBasedExpressionString));
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
