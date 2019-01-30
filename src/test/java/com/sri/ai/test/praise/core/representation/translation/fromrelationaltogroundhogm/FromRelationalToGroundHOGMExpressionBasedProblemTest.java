package com.sri.ai.test.praise.core.representation.translation.fromrelationaltogroundhogm;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem.makeRelationalExpressionFromGroundedVariable;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem;

class FromRelationalToGroundHOGMExpressionBasedProblemTest {

	
	@Test 
	public void makeExpressionFromGroundedVariableTest() {
		String string;
		String expected;
		Expression actual;
		
		string = "f";
		expected = "f";
		actual = makeRelationalExpressionFromGroundedVariable(parse(string));
		assertEquals(parse(expected), actual);
		
		string = "foo";
		expected = "foo";
		actual = makeRelationalExpressionFromGroundedVariable(parse(string));
		assertEquals(parse(expected), actual);
		
		string = "10";
		expected = "10";
		actual = makeRelationalExpressionFromGroundedVariable(parse(string));
		assertEquals(parse(expected), actual);
		
		string = "1.0";
		expected = "1.0";
		actual = makeRelationalExpressionFromGroundedVariable(parse(string));
		assertEquals(parse(expected), actual);

		for (String functor : list("f", "foo", "f123")) {
			string = functor + "__a_10";
			expected = functor + "(a, 10)";
			actual = makeRelationalExpressionFromGroundedVariable(parse(string));
			assertEquals(parse(expected), actual);

			string = functor + "__";
			expected = functor + "()";
			actual = makeRelationalExpressionFromGroundedVariable(parse(string));
			assertEquals(parse(expected), actual);

			string = functor + "__abc";
			expected = functor + "(abc)";
			actual = makeRelationalExpressionFromGroundedVariable(parse(string));
			assertEquals(parse(expected), actual);

			string = functor + "__0_11_222_3333";
			expected = functor + "(0, 11, 222, 3333)";
			actual = makeRelationalExpressionFromGroundedVariable(parse(string));
			assertEquals(parse(expected), actual);
		}
	}
	
	@Test
	void test() {
		HOGMExpressionBasedModel model = new HOGMExpressionBasedModel(
				""
						+ "sort People: 5, ann, bob, carl;"
						+ "sort Countries: 3, US, France, Canada;"
						+ "random Normal: Real x Real -> Real;"
						+ "random sunny: Boolean;"
						+ "random year: 0..10;"
						+ "random happy: People -> Boolean;"
						+ "random married: People x People -> Boolean;"
						+ "random wealth: People x 0..10 -> [-10;10];"
						+ "constant nationality: People -> Countries;"
						+ ""
						+ "for all X in People : for all Y in 0..10 : wealth(X,Y) = Normal(100,0);"
						+ "year = 1971 <=> sunny;"
						+ "for all X in People : for all Y in People : if married(X, Y) and nationality(X) = Canada then nationality(Y) = Canada else not nationality(Y) = Canada;"
				);
		String actual = FromRelationalToGroundHOGMExpressionBasedProblem.getGroundedModelString(model);
		println(actual);
		assertEquals(expected(), actual);
	}

	private String expected() {
		return "sort People: 5, ann, bob, carl;\n" + 
				"sort Countries: 3, US, France, Canada;\n" + 
				"\n" + 
				"constant nationality__ann: Countries;\n" + 
				"constant nationality__bob: Countries;\n" + 
				"constant nationality__carl: Countries;\n" + 
				"constant nationality__people4: Countries;\n" + 
				"constant nationality__people5: Countries;\n" + 
				"\n" + 
				"random Normal : Real x Real -> Real;\n" + 
				"random sunny: Boolean;\n" + 
				"\n" + 
				"random year: 0..10;\n" + 
				"\n" + 
				"random happy__ann: Boolean;\n" + 
				"random happy__bob: Boolean;\n" + 
				"random happy__carl: Boolean;\n" + 
				"random happy__people4: Boolean;\n" + 
				"random happy__people5: Boolean;\n" + 
				"\n" + 
				"random married__ann_ann: Boolean;\n" + 
				"random married__bob_ann: Boolean;\n" + 
				"random married__carl_ann: Boolean;\n" + 
				"random married__people4_ann: Boolean;\n" + 
				"random married__people5_ann: Boolean;\n" + 
				"random married__ann_bob: Boolean;\n" + 
				"random married__bob_bob: Boolean;\n" + 
				"random married__carl_bob: Boolean;\n" + 
				"random married__people4_bob: Boolean;\n" + 
				"random married__people5_bob: Boolean;\n" + 
				"random married__ann_carl: Boolean;\n" + 
				"random married__bob_carl: Boolean;\n" + 
				"random married__carl_carl: Boolean;\n" + 
				"random married__people4_carl: Boolean;\n" + 
				"random married__people5_carl: Boolean;\n" + 
				"random married__ann_people4: Boolean;\n" + 
				"random married__bob_people4: Boolean;\n" + 
				"random married__carl_people4: Boolean;\n" + 
				"random married__people4_people4: Boolean;\n" + 
				"random married__people5_people4: Boolean;\n" + 
				"random married__ann_people5: Boolean;\n" + 
				"random married__bob_people5: Boolean;\n" + 
				"random married__carl_people5: Boolean;\n" + 
				"random married__people4_people5: Boolean;\n" + 
				"random married__people5_people5: Boolean;\n" + 
				"\n" + 
				"random wealth__ann_0: [-10;10];\n" + 
				"random wealth__bob_0: [-10;10];\n" + 
				"random wealth__carl_0: [-10;10];\n" + 
				"random wealth__people4_0: [-10;10];\n" + 
				"random wealth__people5_0: [-10;10];\n" + 
				"random wealth__ann_1: [-10;10];\n" + 
				"random wealth__bob_1: [-10;10];\n" + 
				"random wealth__carl_1: [-10;10];\n" + 
				"random wealth__people4_1: [-10;10];\n" + 
				"random wealth__people5_1: [-10;10];\n" + 
				"random wealth__ann_2: [-10;10];\n" + 
				"random wealth__bob_2: [-10;10];\n" + 
				"random wealth__carl_2: [-10;10];\n" + 
				"random wealth__people4_2: [-10;10];\n" + 
				"random wealth__people5_2: [-10;10];\n" + 
				"random wealth__ann_3: [-10;10];\n" + 
				"random wealth__bob_3: [-10;10];\n" + 
				"random wealth__carl_3: [-10;10];\n" + 
				"random wealth__people4_3: [-10;10];\n" + 
				"random wealth__people5_3: [-10;10];\n" + 
				"random wealth__ann_4: [-10;10];\n" + 
				"random wealth__bob_4: [-10;10];\n" + 
				"random wealth__carl_4: [-10;10];\n" + 
				"random wealth__people4_4: [-10;10];\n" + 
				"random wealth__people5_4: [-10;10];\n" + 
				"random wealth__ann_5: [-10;10];\n" + 
				"random wealth__bob_5: [-10;10];\n" + 
				"random wealth__carl_5: [-10;10];\n" + 
				"random wealth__people4_5: [-10;10];\n" + 
				"random wealth__people5_5: [-10;10];\n" + 
				"random wealth__ann_6: [-10;10];\n" + 
				"random wealth__bob_6: [-10;10];\n" + 
				"random wealth__carl_6: [-10;10];\n" + 
				"random wealth__people4_6: [-10;10];\n" + 
				"random wealth__people5_6: [-10;10];\n" + 
				"random wealth__ann_7: [-10;10];\n" + 
				"random wealth__bob_7: [-10;10];\n" + 
				"random wealth__carl_7: [-10;10];\n" + 
				"random wealth__people4_7: [-10;10];\n" + 
				"random wealth__people5_7: [-10;10];\n" + 
				"random wealth__ann_8: [-10;10];\n" + 
				"random wealth__bob_8: [-10;10];\n" + 
				"random wealth__carl_8: [-10;10];\n" + 
				"random wealth__people4_8: [-10;10];\n" + 
				"random wealth__people5_8: [-10;10];\n" + 
				"random wealth__ann_9: [-10;10];\n" + 
				"random wealth__bob_9: [-10;10];\n" + 
				"random wealth__carl_9: [-10;10];\n" + 
				"random wealth__people4_9: [-10;10];\n" + 
				"random wealth__people5_9: [-10;10];\n" + 
				"random wealth__ann_10: [-10;10];\n" + 
				"random wealth__bob_10: [-10;10];\n" + 
				"random wealth__carl_10: [-10;10];\n" + 
				"random wealth__people4_10: [-10;10];\n" + 
				"random wealth__people5_10: [-10;10];\n" + 
				"\n" + 
				"wealth__ann_0 = Normal(100, 0);\n" + 
				"wealth__bob_0 = Normal(100, 0);\n" + 
				"wealth__carl_0 = Normal(100, 0);\n" + 
				"wealth__people4_0 = Normal(100, 0);\n" + 
				"wealth__people5_0 = Normal(100, 0);\n" + 
				"wealth__ann_1 = Normal(100, 0);\n" + 
				"wealth__bob_1 = Normal(100, 0);\n" + 
				"wealth__carl_1 = Normal(100, 0);\n" + 
				"wealth__people4_1 = Normal(100, 0);\n" + 
				"wealth__people5_1 = Normal(100, 0);\n" + 
				"wealth__ann_2 = Normal(100, 0);\n" + 
				"wealth__bob_2 = Normal(100, 0);\n" + 
				"wealth__carl_2 = Normal(100, 0);\n" + 
				"wealth__people4_2 = Normal(100, 0);\n" + 
				"wealth__people5_2 = Normal(100, 0);\n" + 
				"wealth__ann_3 = Normal(100, 0);\n" + 
				"wealth__bob_3 = Normal(100, 0);\n" + 
				"wealth__carl_3 = Normal(100, 0);\n" + 
				"wealth__people4_3 = Normal(100, 0);\n" + 
				"wealth__people5_3 = Normal(100, 0);\n" + 
				"wealth__ann_4 = Normal(100, 0);\n" + 
				"wealth__bob_4 = Normal(100, 0);\n" + 
				"wealth__carl_4 = Normal(100, 0);\n" + 
				"wealth__people4_4 = Normal(100, 0);\n" + 
				"wealth__people5_4 = Normal(100, 0);\n" + 
				"wealth__ann_5 = Normal(100, 0);\n" + 
				"wealth__bob_5 = Normal(100, 0);\n" + 
				"wealth__carl_5 = Normal(100, 0);\n" + 
				"wealth__people4_5 = Normal(100, 0);\n" + 
				"wealth__people5_5 = Normal(100, 0);\n" + 
				"wealth__ann_6 = Normal(100, 0);\n" + 
				"wealth__bob_6 = Normal(100, 0);\n" + 
				"wealth__carl_6 = Normal(100, 0);\n" + 
				"wealth__people4_6 = Normal(100, 0);\n" + 
				"wealth__people5_6 = Normal(100, 0);\n" + 
				"wealth__ann_7 = Normal(100, 0);\n" + 
				"wealth__bob_7 = Normal(100, 0);\n" + 
				"wealth__carl_7 = Normal(100, 0);\n" + 
				"wealth__people4_7 = Normal(100, 0);\n" + 
				"wealth__people5_7 = Normal(100, 0);\n" + 
				"wealth__ann_8 = Normal(100, 0);\n" + 
				"wealth__bob_8 = Normal(100, 0);\n" + 
				"wealth__carl_8 = Normal(100, 0);\n" + 
				"wealth__people4_8 = Normal(100, 0);\n" + 
				"wealth__people5_8 = Normal(100, 0);\n" + 
				"wealth__ann_9 = Normal(100, 0);\n" + 
				"wealth__bob_9 = Normal(100, 0);\n" + 
				"wealth__carl_9 = Normal(100, 0);\n" + 
				"wealth__people4_9 = Normal(100, 0);\n" + 
				"wealth__people5_9 = Normal(100, 0);\n" + 
				"wealth__ann_10 = Normal(100, 0);\n" + 
				"wealth__bob_10 = Normal(100, 0);\n" + 
				"wealth__carl_10 = Normal(100, 0);\n" + 
				"wealth__people4_10 = Normal(100, 0);\n" + 
				"wealth__people5_10 = Normal(100, 0);\n" + 
				"(year = 1971) <=> sunny;\n" + 
				"if married__ann_ann and (nationality__ann = Canada) then nationality__ann = Canada else not (nationality__ann = Canada);\n" + 
				"if married__bob_ann and (nationality__bob = Canada) then nationality__ann = Canada else not (nationality__ann = Canada);\n" + 
				"if married__carl_ann and (nationality__carl = Canada) then nationality__ann = Canada else not (nationality__ann = Canada);\n" + 
				"if married__people4_ann and (nationality__people4 = Canada) then nationality__ann = Canada else not (nationality__ann = Canada);\n" + 
				"if married__people5_ann and (nationality__people5 = Canada) then nationality__ann = Canada else not (nationality__ann = Canada);\n" + 
				"if married__ann_bob and (nationality__ann = Canada) then nationality__bob = Canada else not (nationality__bob = Canada);\n" + 
				"if married__bob_bob and (nationality__bob = Canada) then nationality__bob = Canada else not (nationality__bob = Canada);\n" + 
				"if married__carl_bob and (nationality__carl = Canada) then nationality__bob = Canada else not (nationality__bob = Canada);\n" + 
				"if married__people4_bob and (nationality__people4 = Canada) then nationality__bob = Canada else not (nationality__bob = Canada);\n" + 
				"if married__people5_bob and (nationality__people5 = Canada) then nationality__bob = Canada else not (nationality__bob = Canada);\n" + 
				"if married__ann_carl and (nationality__ann = Canada) then nationality__carl = Canada else not (nationality__carl = Canada);\n" + 
				"if married__bob_carl and (nationality__bob = Canada) then nationality__carl = Canada else not (nationality__carl = Canada);\n" + 
				"if married__carl_carl and (nationality__carl = Canada) then nationality__carl = Canada else not (nationality__carl = Canada);\n" + 
				"if married__people4_carl and (nationality__people4 = Canada) then nationality__carl = Canada else not (nationality__carl = Canada);\n" + 
				"if married__people5_carl and (nationality__people5 = Canada) then nationality__carl = Canada else not (nationality__carl = Canada);\n" + 
				"if married__ann_people4 and (nationality__ann = Canada) then nationality__people4 = Canada else not (nationality__people4 = Canada);\n" + 
				"if married__bob_people4 and (nationality__bob = Canada) then nationality__people4 = Canada else not (nationality__people4 = Canada);\n" + 
				"if married__carl_people4 and (nationality__carl = Canada) then nationality__people4 = Canada else not (nationality__people4 = Canada);\n" + 
				"if married__people4_people4 and (nationality__people4 = Canada) then nationality__people4 = Canada else not (nationality__people4 = Canada);\n" + 
				"if married__people5_people4 and (nationality__people5 = Canada) then nationality__people4 = Canada else not (nationality__people4 = Canada);\n" + 
				"if married__ann_people5 and (nationality__ann = Canada) then nationality__people5 = Canada else not (nationality__people5 = Canada);\n" + 
				"if married__bob_people5 and (nationality__bob = Canada) then nationality__people5 = Canada else not (nationality__people5 = Canada);\n" + 
				"if married__carl_people5 and (nationality__carl = Canada) then nationality__people5 = Canada else not (nationality__people5 = Canada);\n" + 
				"if married__people4_people5 and (nationality__people4 = Canada) then nationality__people5 = Canada else not (nationality__people5 = Canada);\n" + 
				"if married__people5_people5 and (nationality__people5 = Canada) then nationality__people5 = Canada else not (nationality__people5 = Canada);\n" + 
				"";
	}

}
