package com.sri.ai.test.praise.core.representation.interfacebased.expressionsampling;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedMeanAndStandardDeviation;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.SamplingFactorDiscretizedProbabilityDistribution;
import com.sri.ai.util.function.api.values.Value;

class ExpressionSamplingFactorTest {

	@Test
	void test() {
		Random random = new Random();
		ExpressionVariable x = new DefaultExpressionVariable(parse("x"));
		Context context = new TrueContext().setSymbolsAndTypes(map(parse("x"), parse("[0;100]")));
		SamplingFactor normal = new NormalWithFixedMeanAndStandardDeviation(x, 50.0, 10.0, random);
		ExpressionSamplingFactor expressionSamplingFactor = ExpressionSamplingFactor.newInstance(normal, 0, v -> 11, context);
		SamplingFactorDiscretizedProbabilityDistribution distribution = expressionSamplingFactor.getSamplingFactorDiscretizedProbabilityDistribution();
		for (int i = 0; i != 100000; i++) {
			distribution.sample();
		}
		for (double value = 0; value <= 100; value += 10) {
			Value probability = distribution.apply(arrayList(value));
			BigDecimal trimmed = new BigDecimal(probability.doubleValue(), new MathContext(3));
			println("Probability of the bin of " + value + " = " + trimmed);
		}
		
		assertEquals(0.00106, distribution.apply(arrayList(10.0)).doubleValue(), 0.1);
		assertEquals(0.0610, distribution.apply(arrayList(30.0)).doubleValue(), 0.1);
		assertEquals(0.381, distribution.apply(arrayList(50.0)).doubleValue(), 0.1);
		assertEquals(0.0610, distribution.apply(arrayList(70.0)).doubleValue(), 0.1);
		assertEquals(0.00106, distribution.apply(arrayList(90.0)).doubleValue(), 0.1);
	}

}
