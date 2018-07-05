package com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.proceduralattachment;

import static com.sri.ai.util.Util.list;

import java.util.Collections;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.AbstractExpressionFactor;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;

public class ProceduralAttachmentExpressionFactor extends AbstractExpressionFactor {
	
	private static final long serialVersionUID = 1L;
	
	ProceduralAttachment proceduralAttachment;
	
	public ProceduralAttachmentExpressionFactor(Variable variable, Procedure<?> procedure, Context context) {
		super(context);
		proceduralAttachment = new ProceduralAttachment(variable, procedure);
	}

	@Override
	public List<? extends Variable> getVariables() {
		return Collections.unmodifiableList(list(proceduralAttachment.getVariable()));
	}
	
	@Override
	protected Expression computeInnerExpression() {
		Symbol valueExpression = DefaultSymbol.createSymbol(proceduralAttachment.getProcedureValue());
		Expression result = IfThenElse.make(Equality.make(proceduralAttachment.getVariable(), valueExpression), Expressions.ONE, Expressions.ZERO);
		return result;
	}

	@Override
	public boolean equals(Object another) {
		boolean result = this == another;
		return result;
	}
	
	@Override
	public int hashCode() {
		int result = System.identityHashCode(this);
		return result;
	}
}
