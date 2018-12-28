package com.sri.ai.test.praise.core.inference.representation.sampling;

import static com.sri.ai.util.Util.findFirst;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.Random;

import org.junit.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.PotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedMeanAndStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SamplingMarginalizingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SamplingProductFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;

public class SamplingRulesTest {

	private static Random random = new Random();
	
	private SamplingFactor normalOnX;
	private SamplingFactor normalOnY;
	private SamplingFactor normalOnZ;
	private Variable x;
	private Variable y;
	private Variable z;
	private PotentialFactory potentialFactory = new DoublePotentialFactory();
	private DefaultFactorNetwork network;
	private ExactBP solver;
	private SamplingFactor marginalOfZ;
	private SamplingFactor factor;
	private double meanOfZ;
	private double varianceOfZ;
	private String expectedString;

	public void setXYZModel() {
		x = new DefaultVariable("x");
		y = new DefaultVariable("y");
		z = new DefaultVariable("z");
		meanOfZ = 10;
		varianceOfZ = 4;
		normalOnX = new NormalWithFixedStandardDeviation(x, y, 2, potentialFactory, random);
		normalOnY = new NormalWithFixedStandardDeviation(y, z, 2, potentialFactory, random);
		normalOnZ = new NormalWithFixedMeanAndStandardDeviation(z, meanOfZ, Math.pow(varianceOfZ, 0.5), new DoublePotentialFactory(), random);
		network = new DefaultFactorNetwork(list(normalOnY, normalOnX, normalOnZ));
	}

	@Test
	public void testXYZModelSamplingRules() {

		setXYZModel();

		solver = new ExactBP(z, network);
		marginalOfZ = (SamplingFactor) solver.apply();
		println(marginalOfZ.nestedString(true));

		factor = marginalOfZ;
		expectedString = "z <= ";
		println(samplingRulesString(factor));
		assertEquals(expectedString, samplingRulesString(marginalOfZ));

		// message from y to z
		SamplingMarginalizingFactor messageFromYToZ = 
				(SamplingMarginalizingFactor) findFirst(
						((SamplingProductFactor) marginalOfZ).getInputFactors(), 
						f -> f instanceof SamplingMarginalizingFactor);
		factor = messageFromYToZ;
		expectedString = "";
		runSamplingRulesTest();

		SamplingFactor priorOfZ = 
				findFirst(
				((SamplingProductFactor) marginalOfZ).getInputFactors(), 
				f -> ! (f instanceof SamplingMarginalizingFactor));
		factor = priorOfZ;
		expectedString = "z <= ";
		runSamplingRulesTest();

		SamplingProductFactor incomingToY = (SamplingProductFactor) messageFromYToZ.getMarginalizedFactor();
		factor = incomingToY;
		expectedString = "z <= y; y <= z";
		runSamplingRulesTest();

		SamplingFactor YIsNormalZ = incomingToY.getInputFactors().get(0);
		factor = YIsNormalZ;
		expectedString = "z <= y; y <= z";
		runSamplingRulesTest();

		SamplingFactor messageFromXToY = incomingToY.getInputFactors().get(1);
		factor = messageFromXToY;
		expectedString = "";
		runSamplingRulesTest();

	}

	private void runSamplingRulesTest() {
		println(samplingRulesString(factor));
		assertEquals(expectedString, samplingRulesString(factor));
	}

	private String samplingRulesString(SamplingFactor samplingFactor) {
		SamplingRuleSet samplingRules = samplingFactor.getSamplingRuleSet();
		List<String> ruleStrings = mapIntoList(samplingRules.getSamplingRules(), this::ruleString);
		String samplingRulesString = join("; ", ruleStrings);
		return samplingRulesString;
	}
	
	private String ruleString(SamplingRule rule) {
		return join(rule.getConsequents()) + " <= " + join(rule.getAntecendents());
	}
}
