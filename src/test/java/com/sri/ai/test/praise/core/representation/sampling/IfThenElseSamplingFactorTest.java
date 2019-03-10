package com.sri.ai.test.praise.core.representation.sampling;

import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.logic.IfThenElseSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.statistics.NormalWithFixedMeanAndStandardDeviation;

class IfThenElseSamplingFactorTest {

	Variable rainy = new DefaultVariable("rainy");
	Variable x = new DefaultVariable("x");
	Variable y = new DefaultVariable("y");
	Random random = new Random();

	@Test
	void samplingRulesTest() {
		IfThenElseSamplingFactor ifThenElseSamplingFactor;
		String expectedSamplingRulesString;
		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(x, 5.0, 1.0, random),
						random);
		expectedSamplingRulesString = 
				"x <= rainy, rainy = true with if (rainy) then x = Normal(0.0, 1.0) else x = Normal(5.0, 1.0)\n" + 
						"x <= rainy, rainy = false with if (rainy) then x = Normal(0.0, 1.0) else x = Normal(5.0, 1.0)\n" + 
						"rainy <= x with if (rainy) then x = Normal(0.0, 1.0) else x = Normal(5.0, 1.0)";
		runSamplingRulesTest(ifThenElseSamplingFactor, expectedSamplingRulesString);

		
		ifThenElseSamplingFactor = 
				new IfThenElseSamplingFactor(
						rainy,
						new NormalWithFixedMeanAndStandardDeviation(x, 0.0, 1.0, random),
						new NormalWithFixedMeanAndStandardDeviation(y, 5.0, 1.0, random),
						random);
		expectedSamplingRulesString = 
				"x <= rainy, rainy = true with if (rainy) then x = Normal(0.0, 1.0) else y = Normal(5.0, 1.0)\n" + 
						"y <= rainy, rainy = false with if (rainy) then x = Normal(0.0, 1.0) else y = Normal(5.0, 1.0)\n" + 
						"rainy <= x, y with if (rainy) then x = Normal(0.0, 1.0) else y = Normal(5.0, 1.0)";
		runSamplingRulesTest(ifThenElseSamplingFactor, expectedSamplingRulesString);
	}

	private void runSamplingRulesTest(IfThenElseSamplingFactor ifThenElseSamplingFactor, String expectedSamplingRulesString) {
		String actualSamplingRulesString = join("\n", ifThenElseSamplingFactor.getSamplingRuleSet().getSamplingRules());
		println("Expected:\n" + expectedSamplingRulesString);
		println("Actual  :\n" + actualSamplingRulesString);
		assertEquals(expectedSamplingRulesString, actualSamplingRulesString);
	}

}
