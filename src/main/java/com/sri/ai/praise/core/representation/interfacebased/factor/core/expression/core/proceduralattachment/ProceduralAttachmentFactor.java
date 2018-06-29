package com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.proceduralattachment;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.praise.other.integration.proceduralattachment.core.EmptyProcedurePayload.EMPTY_PROCEDURE_PAYLOAD;
import static com.sri.ai.util.Util.list;
import static java.util.Collections.unmodifiableList;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.AbstractExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;

public class ProceduralAttachmentFactor extends AbstractExpressionFactor {
	
	private static final long serialVersionUID = 1L;

	private Variable variable;
	private Procedure<?> procedure;
//	private Context context;
	
	public ProceduralAttachmentFactor(Variable variable, Procedure<?> procedure, Context context) {
		super(context);
		this.variable = variable;
		this.procedure = procedure;
//		this.context = context;
	}

	@Override
	public boolean contains(Variable variable) {
		boolean result = variable.equals(this.variable);
		return result;
	}

//	public Context getContext() {
//		return context;
//	}

	@Override
	public List<? extends Variable> getVariables() {
		return unmodifiableList(list(variable));
	}

	@Override
	public Factor multiply(Factor another) {
		Factor factor = makeFactor();
		Factor result = factor.multiply(another);
		return result;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		Factor result;
		if (variablesToSumOut.contains(variable)) {
			result = IDENTITY_FACTOR;
		}
		else {
			result = this;
		}
		return result;
	}

	@Override
	public boolean isIdentity() {
		return false;
	}

	@Override
	public boolean isZero() {
		return false;
	}

	@Override
	public Double getEntryFor(Map<? extends Variable, ? extends Object> variableInstantiations) {
		Object givenValue = variableInstantiations.get(variable);
		boolean givenValueEqualsProcedureValue = givenValue.equals(getProcedureValue());
		Double result = givenValueEqualsProcedureValue? 1.0 : 0.0;
		return result;
	}

	@Override
	public Factor normalize() {
		return this;
	}

	@Override
	public Factor add(Factor another) {
		Factor result = makeFactor().add(another);
		return result;
	}

	@Override
	public Factor invert() {
		Factor result = makeFactor().invert();
		return result;
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		Factor result = makeFactor().max(variablesToMaximize);
		return result;
	}
	
	private Factor makeFactor() {
		ExpressionFactor result = new DefaultExpressionFactor(makeExpression(), getContext());
		return result;
	}

	protected Expression makeExpression() {
		Expression result = IfThenElse.make(Equality.make(variable, getProcedureValue()), Expressions.ONE, Expressions.ZERO);
		return result;
	}
	
	private Expression value;

	public Expression getProcedureValue() {
		if (this.value == null) {
			Object resultValue = procedure.evaluate(EMPTY_PROCEDURE_PAYLOAD);
			this.value = DefaultSymbol.createSymbol(resultValue);
		}
		return this.value;
	}

	@Override
	public boolean equals(Object another) {
		boolean result;
		if (another instanceof ProceduralAttachmentFactor) {
			throw new Error("Comparing two " + ProceduralAttachmentFactor.class + " can be expensive and should not really be happening, so it's likely there is a problem in the program's logic.");
		}
		else {
			result = false;
		}
		return result;
	}
	
	@Override
	public int hashCode() {
		int result = System.identityHashCode(this);
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		return makeExpression();
	}
}
