package com.sri.ai.test.praise.stress;

import java.util.Map;

import org.junit.Assume;
import org.junit.Before;
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
	
	@Test
	public void testBeliefForLoopyModels() {
		class LoopyBeliefTestData extends TestData {
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
		
		TestData[] tests = new TestData[] {
				//
				// Basic:
				// 
				
//				// #1
//				// Note: if caches can grow unbounded then run with: -Xms2G -Xmx16G
//		        // Note: Slow to process, takes approx 6.3 minutes (as of April 2013, had been 2 hours 5 min before Oct 2012).
//				new LoopyBeliefTestData(Expressions.TRUE.toString(),
//					new com.sri.ai.praise.model.example.TrivialLoopyMisconceptionExample(),
//					"belief([m(X)])",
//					false,
//					// TODO - is this correct (10 iterations)?
//					"if m(X) then 0.432395068 else 0.567604932"),
//
//				// #2
//                // TODO - appears not to stop processing
////				new LoopyBeliefTestData(Expressions.TRUE.toString(),
////					new TrivialLoopyPQandb(), 
////					"belief([p(X)])", 
////					false, 
////					// TODO -  is this correct (> 2 iterations, doesn't matter as large
////					// # calculated gets reduced to 1 in R_normalize logic)?
////					"if X = b then if p(b) then 1 else 0 else if p(X) then 1 else 0"),
				
					
//				// #3
//			    // TODO - appears not to stop processing:
//				new LoopyBeliefTestData(Expressions.TRUE.toString(),
//					new com.sri.ai.praise.model.example.TrivialLoopyFriendsAnnBobAndSmokerBobExample(),
//					"belief([smoker(ann)])",
//					false,
//					"TODO"),
		};
		
		perform(tests);
	}
}
