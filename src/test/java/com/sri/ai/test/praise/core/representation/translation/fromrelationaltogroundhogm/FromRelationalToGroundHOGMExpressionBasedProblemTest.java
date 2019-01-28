package com.sri.ai.test.praise.core.representation.translation.fromrelationaltogroundhogm;

import static com.sri.ai.util.Util.println;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem;

class FromRelationalToGroundHOGMExpressionBasedProblemTest {

	@Test
	void test() {
		HOGMExpressionBasedModel model = new HOGMExpressionBasedModel(
				""
						+ "sort People: 5, ann, bob, carl;"
						+ "random happy: People -> Boolean;"
						+ ""
						+ "for all X in People: happy(X);"
				);
		String actual = FromRelationalToGroundHOGMExpressionBasedProblem.getModelString(model);
		println(actual);
	}

}
