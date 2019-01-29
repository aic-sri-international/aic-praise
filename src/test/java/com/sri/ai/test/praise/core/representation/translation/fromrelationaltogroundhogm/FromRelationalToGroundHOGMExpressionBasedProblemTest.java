package com.sri.ai.test.praise.core.representation.translation.fromrelationaltogroundhogm;

import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm.FromRelationalToGroundHOGMExpressionBasedProblem;

class FromRelationalToGroundHOGMExpressionBasedProblemTest {

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
				"constant nationality___ann: Countries;\n" + 
				"constant nationality___bob: Countries;\n" + 
				"constant nationality___carl: Countries;\n" + 
				"constant nationality___people4: Countries;\n" + 
				"constant nationality___people5: Countries;\n" + 
				"\n" + 
				"random Normal : Real x Real -> Real;\n" + 
				"random sunny: Boolean;\n" + 
				"\n" + 
				"random year: 0..10;\n" + 
				"\n" + 
				"random happy___ann: Boolean;\n" + 
				"random happy___bob: Boolean;\n" + 
				"random happy___carl: Boolean;\n" + 
				"random happy___people4: Boolean;\n" + 
				"random happy___people5: Boolean;\n" + 
				"\n" + 
				"random married___ann_ann: Boolean;\n" + 
				"random married___bob_ann: Boolean;\n" + 
				"random married___carl_ann: Boolean;\n" + 
				"random married___people4_ann: Boolean;\n" + 
				"random married___people5_ann: Boolean;\n" + 
				"random married___ann_bob: Boolean;\n" + 
				"random married___bob_bob: Boolean;\n" + 
				"random married___carl_bob: Boolean;\n" + 
				"random married___people4_bob: Boolean;\n" + 
				"random married___people5_bob: Boolean;\n" + 
				"random married___ann_carl: Boolean;\n" + 
				"random married___bob_carl: Boolean;\n" + 
				"random married___carl_carl: Boolean;\n" + 
				"random married___people4_carl: Boolean;\n" + 
				"random married___people5_carl: Boolean;\n" + 
				"random married___ann_people4: Boolean;\n" + 
				"random married___bob_people4: Boolean;\n" + 
				"random married___carl_people4: Boolean;\n" + 
				"random married___people4_people4: Boolean;\n" + 
				"random married___people5_people4: Boolean;\n" + 
				"random married___ann_people5: Boolean;\n" + 
				"random married___bob_people5: Boolean;\n" + 
				"random married___carl_people5: Boolean;\n" + 
				"random married___people4_people5: Boolean;\n" + 
				"random married___people5_people5: Boolean;\n" + 
				"\n" + 
				"random wealth___ann_0: [-10;10];\n" + 
				"random wealth___bob_0: [-10;10];\n" + 
				"random wealth___carl_0: [-10;10];\n" + 
				"random wealth___people4_0: [-10;10];\n" + 
				"random wealth___people5_0: [-10;10];\n" + 
				"random wealth___ann_1: [-10;10];\n" + 
				"random wealth___bob_1: [-10;10];\n" + 
				"random wealth___carl_1: [-10;10];\n" + 
				"random wealth___people4_1: [-10;10];\n" + 
				"random wealth___people5_1: [-10;10];\n" + 
				"random wealth___ann_2: [-10;10];\n" + 
				"random wealth___bob_2: [-10;10];\n" + 
				"random wealth___carl_2: [-10;10];\n" + 
				"random wealth___people4_2: [-10;10];\n" + 
				"random wealth___people5_2: [-10;10];\n" + 
				"random wealth___ann_3: [-10;10];\n" + 
				"random wealth___bob_3: [-10;10];\n" + 
				"random wealth___carl_3: [-10;10];\n" + 
				"random wealth___people4_3: [-10;10];\n" + 
				"random wealth___people5_3: [-10;10];\n" + 
				"random wealth___ann_4: [-10;10];\n" + 
				"random wealth___bob_4: [-10;10];\n" + 
				"random wealth___carl_4: [-10;10];\n" + 
				"random wealth___people4_4: [-10;10];\n" + 
				"random wealth___people5_4: [-10;10];\n" + 
				"random wealth___ann_5: [-10;10];\n" + 
				"random wealth___bob_5: [-10;10];\n" + 
				"random wealth___carl_5: [-10;10];\n" + 
				"random wealth___people4_5: [-10;10];\n" + 
				"random wealth___people5_5: [-10;10];\n" + 
				"random wealth___ann_6: [-10;10];\n" + 
				"random wealth___bob_6: [-10;10];\n" + 
				"random wealth___carl_6: [-10;10];\n" + 
				"random wealth___people4_6: [-10;10];\n" + 
				"random wealth___people5_6: [-10;10];\n" + 
				"random wealth___ann_7: [-10;10];\n" + 
				"random wealth___bob_7: [-10;10];\n" + 
				"random wealth___carl_7: [-10;10];\n" + 
				"random wealth___people4_7: [-10;10];\n" + 
				"random wealth___people5_7: [-10;10];\n" + 
				"random wealth___ann_8: [-10;10];\n" + 
				"random wealth___bob_8: [-10;10];\n" + 
				"random wealth___carl_8: [-10;10];\n" + 
				"random wealth___people4_8: [-10;10];\n" + 
				"random wealth___people5_8: [-10;10];\n" + 
				"random wealth___ann_9: [-10;10];\n" + 
				"random wealth___bob_9: [-10;10];\n" + 
				"random wealth___carl_9: [-10;10];\n" + 
				"random wealth___people4_9: [-10;10];\n" + 
				"random wealth___people5_9: [-10;10];\n" + 
				"random wealth___ann_10: [-10;10];\n" + 
				"random wealth___bob_10: [-10;10];\n" + 
				"random wealth___carl_10: [-10;10];\n" + 
				"random wealth___people4_10: [-10;10];\n" + 
				"random wealth___people5_10: [-10;10];\n" + 
				"\n" + 
				"wealth___ann_0 = Normal(100, 0);\n" + 
				"wealth___bob_0 = Normal(100, 0);\n" + 
				"wealth___carl_0 = Normal(100, 0);\n" + 
				"wealth___people4_0 = Normal(100, 0);\n" + 
				"wealth___people5_0 = Normal(100, 0);\n" + 
				"wealth___ann_1 = Normal(100, 0);\n" + 
				"wealth___bob_1 = Normal(100, 0);\n" + 
				"wealth___carl_1 = Normal(100, 0);\n" + 
				"wealth___people4_1 = Normal(100, 0);\n" + 
				"wealth___people5_1 = Normal(100, 0);\n" + 
				"wealth___ann_2 = Normal(100, 0);\n" + 
				"wealth___bob_2 = Normal(100, 0);\n" + 
				"wealth___carl_2 = Normal(100, 0);\n" + 
				"wealth___people4_2 = Normal(100, 0);\n" + 
				"wealth___people5_2 = Normal(100, 0);\n" + 
				"wealth___ann_3 = Normal(100, 0);\n" + 
				"wealth___bob_3 = Normal(100, 0);\n" + 
				"wealth___carl_3 = Normal(100, 0);\n" + 
				"wealth___people4_3 = Normal(100, 0);\n" + 
				"wealth___people5_3 = Normal(100, 0);\n" + 
				"wealth___ann_4 = Normal(100, 0);\n" + 
				"wealth___bob_4 = Normal(100, 0);\n" + 
				"wealth___carl_4 = Normal(100, 0);\n" + 
				"wealth___people4_4 = Normal(100, 0);\n" + 
				"wealth___people5_4 = Normal(100, 0);\n" + 
				"wealth___ann_5 = Normal(100, 0);\n" + 
				"wealth___bob_5 = Normal(100, 0);\n" + 
				"wealth___carl_5 = Normal(100, 0);\n" + 
				"wealth___people4_5 = Normal(100, 0);\n" + 
				"wealth___people5_5 = Normal(100, 0);\n" + 
				"wealth___ann_6 = Normal(100, 0);\n" + 
				"wealth___bob_6 = Normal(100, 0);\n" + 
				"wealth___carl_6 = Normal(100, 0);\n" + 
				"wealth___people4_6 = Normal(100, 0);\n" + 
				"wealth___people5_6 = Normal(100, 0);\n" + 
				"wealth___ann_7 = Normal(100, 0);\n" + 
				"wealth___bob_7 = Normal(100, 0);\n" + 
				"wealth___carl_7 = Normal(100, 0);\n" + 
				"wealth___people4_7 = Normal(100, 0);\n" + 
				"wealth___people5_7 = Normal(100, 0);\n" + 
				"wealth___ann_8 = Normal(100, 0);\n" + 
				"wealth___bob_8 = Normal(100, 0);\n" + 
				"wealth___carl_8 = Normal(100, 0);\n" + 
				"wealth___people4_8 = Normal(100, 0);\n" + 
				"wealth___people5_8 = Normal(100, 0);\n" + 
				"wealth___ann_9 = Normal(100, 0);\n" + 
				"wealth___bob_9 = Normal(100, 0);\n" + 
				"wealth___carl_9 = Normal(100, 0);\n" + 
				"wealth___people4_9 = Normal(100, 0);\n" + 
				"wealth___people5_9 = Normal(100, 0);\n" + 
				"wealth___ann_10 = Normal(100, 0);\n" + 
				"wealth___bob_10 = Normal(100, 0);\n" + 
				"wealth___carl_10 = Normal(100, 0);\n" + 
				"wealth___people4_10 = Normal(100, 0);\n" + 
				"wealth___people5_10 = Normal(100, 0);\n" + 
				"(year = 1971) <=> sunny;\n" + 
				"if married___ann_ann and (nationality___ann = Canada) then nationality___ann = Canada else not (nationality___ann = Canada);\n" + 
				"if married___bob_ann and (nationality___bob = Canada) then nationality___ann = Canada else not (nationality___ann = Canada);\n" + 
				"if married___carl_ann and (nationality___carl = Canada) then nationality___ann = Canada else not (nationality___ann = Canada);\n" + 
				"if married___people4_ann and (nationality___people4 = Canada) then nationality___ann = Canada else not (nationality___ann = Canada);\n" + 
				"if married___people5_ann and (nationality___people5 = Canada) then nationality___ann = Canada else not (nationality___ann = Canada);\n" + 
				"if married___ann_bob and (nationality___ann = Canada) then nationality___bob = Canada else not (nationality___bob = Canada);\n" + 
				"if married___bob_bob and (nationality___bob = Canada) then nationality___bob = Canada else not (nationality___bob = Canada);\n" + 
				"if married___carl_bob and (nationality___carl = Canada) then nationality___bob = Canada else not (nationality___bob = Canada);\n" + 
				"if married___people4_bob and (nationality___people4 = Canada) then nationality___bob = Canada else not (nationality___bob = Canada);\n" + 
				"if married___people5_bob and (nationality___people5 = Canada) then nationality___bob = Canada else not (nationality___bob = Canada);\n" + 
				"if married___ann_carl and (nationality___ann = Canada) then nationality___carl = Canada else not (nationality___carl = Canada);\n" + 
				"if married___bob_carl and (nationality___bob = Canada) then nationality___carl = Canada else not (nationality___carl = Canada);\n" + 
				"if married___carl_carl and (nationality___carl = Canada) then nationality___carl = Canada else not (nationality___carl = Canada);\n" + 
				"if married___people4_carl and (nationality___people4 = Canada) then nationality___carl = Canada else not (nationality___carl = Canada);\n" + 
				"if married___people5_carl and (nationality___people5 = Canada) then nationality___carl = Canada else not (nationality___carl = Canada);\n" + 
				"if married___ann_people4 and (nationality___ann = Canada) then nationality___people4 = Canada else not (nationality___people4 = Canada);\n" + 
				"if married___bob_people4 and (nationality___bob = Canada) then nationality___people4 = Canada else not (nationality___people4 = Canada);\n" + 
				"if married___carl_people4 and (nationality___carl = Canada) then nationality___people4 = Canada else not (nationality___people4 = Canada);\n" + 
				"if married___people4_people4 and (nationality___people4 = Canada) then nationality___people4 = Canada else not (nationality___people4 = Canada);\n" + 
				"if married___people5_people4 and (nationality___people5 = Canada) then nationality___people4 = Canada else not (nationality___people4 = Canada);\n" + 
				"if married___ann_people5 and (nationality___ann = Canada) then nationality___people5 = Canada else not (nationality___people5 = Canada);\n" + 
				"if married___bob_people5 and (nationality___bob = Canada) then nationality___people5 = Canada else not (nationality___people5 = Canada);\n" + 
				"if married___carl_people5 and (nationality___carl = Canada) then nationality___people5 = Canada else not (nationality___people5 = Canada);\n" + 
				"if married___people4_people5 and (nationality___people4 = Canada) then nationality___people5 = Canada else not (nationality___people5 = Canada);\n" + 
				"if married___people5_people5 and (nationality___people5 = Canada) then nationality___people5 = Canada else not (nationality___people5 = Canada);\n" + 
				"";
	}

}
