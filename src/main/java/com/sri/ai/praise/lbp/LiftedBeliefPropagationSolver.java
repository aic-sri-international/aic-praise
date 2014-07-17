package com.sri.ai.praise.lbp;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.praise.LPIUtil;
import com.sri.ai.praise.Solver;
import com.sri.ai.praise.lbp.core.Belief;
import com.sri.ai.praise.lbp.core.DefaultLBPConfiguration;
import com.sri.ai.praise.model.Model;

public class LiftedBeliefPropagationSolver implements Solver {

	private LBPConfiguration configuration  = new DefaultLBPConfiguration();

	public LiftedBeliefPropagationSolver() {
	}
	
	public LiftedBeliefPropagationSolver(boolean synchronous) {
		if (synchronous) {
			configuration.setBeliefPropagationUpdateSchedule(LBPConfiguration.BeliefPropagationUpdateSchedule.SYNCHRONOUS);
		}
	}
	
	@Override
	public Expression marginal(Expression queryRandomVariableApplication, Model model) {
		Expression       belief         = LPIUtil.makeBelief(queryRandomVariableApplication);
		RewritingProcess process        = model.makeRewritingProcess(belief, configuration);
		Belief           beliefRewriter = new Belief();
		Expression       result         = beliefRewriter.rewrite(belief, process);
		return result;
	}

}
