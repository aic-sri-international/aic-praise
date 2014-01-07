package com.sri.ai.praise.lbp.core;

import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.LPIUtil;

/**
 * Indicates whether a message value expression represents a deterministic message.
 * 
 * @author braz
 *
 */
public class IsDeterministicBooleanMessageValue implements Predicate<Expression> {

	private RewritingProcess process;

	public IsDeterministicBooleanMessageValue(RewritingProcess process) {
		super();
		this.process = process;
	}

	@Override
	public boolean apply(Expression messageValue) {
		return isDeterministicMessageValue(messageValue, process);
	}

	public static boolean isDeterministicMessageValue(Expression messageValue, RewritingProcess process) {
		messageValue = IfThenElse.equivalentWithNonNegatedCondition(messageValue);
		boolean result =
				IfThenElse.isIfThenElse(messageValue)
				&&
				LPIUtil.isRandomVariableValueExpression(IfThenElse.getCondition(messageValue), process)
				&&
				(IfThenElse.getThenBranch(messageValue).equals(0) || IfThenElse.getElseBranch(messageValue).equals(0));
		return result;
	}

}
