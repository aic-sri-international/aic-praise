package com.sri.ai.test.praise.stress;

import java.util.Map;

import org.junit.Assume;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.praise.lbp.LBPConfiguration;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.lbp.LBPRewriter;
import com.sri.ai.praise.model.Model;
import com.sri.ai.test.praise.AbstractLPITest;

/**
 * Lifted Belief Propagation Stress Tests.
 * 
 * @author oreilly
 *
 */
public class LBPStressIT extends AbstractLPITest {
	
	@Before
	public void ignoreTest() {
		Assume.assumeFalse("Stress Tests Ignored.", Boolean.getBoolean("ignore.stress.tests"));
	}
	
	// #1
	// Note: if caches can grow unbounded then run with: -Xms2G -Xmx16G
    // Note: Slow to process, takes approx 6.3 minutes (as of April 2013, had been 2 hours 5 min before Oct 2012).
	@Ignore // Currently failing assertion 'value depends on random variable value other than its own destination/origin'. 
	@Test
	public void testStressTest1() {
		
		perform(new TestData[] {
			new LoopyBeliefTestData(Expressions.TRUE.toString(),
				new com.sri.ai.praise.model.example.TrivialLoopyMisconceptionExample(),
				"belief([m(X)])",
				false,
				// TODO - is this correct (10 iterations)?
				"if m(X) then 0.00646884468 else 0.993531155")});
	}

	@Test
	public void testStressTest2() {
		perform(new TestData[] {
				new LoopyBeliefTestData(Expressions.TRUE.toString(),
						new com.sri.ai.praise.model.example.TrivialLoopyPQandb(), 
						"belief([p(X)])", 
						false, 
						// TODO -  is this correct (> 2 iterations, doesn't matter as large
						// # calculated gets reduced to 1 in R_normalize logic)?
						"if X = b then if p(b) then 1 else 0 else if p(X) then 1 else 0")});
	}
	
	// Note: This is based on PRAiSE Demo App Example 3.
	@Test
	public void testStressTest3() {
		perform(new TestData[] {
				new LoopyBeliefTestData(Expressions.TRUE.toString(),
						new com.sri.ai.praise.model.example.TrivialLoopyFriendsAnnBobAndSmokerBobExample(),
						"belief([smoker(ann)])",
						false,
						"if smoker(ann) then 0.995514833 else 0.00448516689")});
	}
	
	// Note: This is based on PRAiSE Demo App Example 4.
	@Ignore
	@Test
	public void testStressTest4() {
		perform(new TestData[] {
				new LoopyBeliefTestData(Expressions.TRUE.toString(),
						new Model(Model.getModelDeclarationFromResource("Example4.model")),
						"belief([referenceOf(w7, X)])",
						false,
						"TODO")});
	}
	
	@Ignore
	@Test
	public void testStressTest4DefectIteration2() {
		perform(new TestData[] {
				new LoopyBeliefTestData(Expressions.TRUE.toString(),
						new Model(Model.getModelDeclarationFromResource("Example4DefectIteration2.model")),
						"belief([r(w7, X)])",
						false,
						"TODO")});
	}
	
	private class LoopyBeliefTestData extends TestData {
		private String belief; 
		private Expression exprBelief;
		private Map<Object, Object> globalObjects;
		
		public LoopyBeliefTestData(String contextualConstraint, Model model, String belief, boolean illegalArgumentTest, String expected) {
			this(contextualConstraint, model, belief, null, illegalArgumentTest, expected);
		};
		
		public LoopyBeliefTestData(String contextualConstraint, Model model, String belief, Map<Object, Object> globalObjects, boolean illegalArgumentTest, String expected) {
			super(contextualConstraint, model, illegalArgumentTest, expected);
			this.belief = belief;
			this.globalObjects = globalObjects;
		};
		
		@Override
		public Expression getTopExpression() {
			this.exprBelief = parse(belief);
			return this.exprBelief;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			if (globalObjects != null) {
				process.getGlobalObjects().putAll(globalObjects);
			}
			LBPConfiguration configuration = LBPFactory.newLBPConfiguration();
			// Currently only schedule that will work with a loopy model
			configuration.setBeliefPropagationUpdateSchedule(LBPConfiguration.BeliefPropagationUpdateSchedule.SYNCHRONOUS);
			RewritingProcess lbpProcess = LBPFactory.newLBPProcess(process.getRootExpression(), configuration, process);
			
			
			Expression belief = lbpProcess.rewrite(LBPRewriter.R_belief, exprBelief);
			Expression roundedBelief = Expressions.roundToAGivenPrecision(belief, 9);
			return roundedBelief;
		}
	};
}
