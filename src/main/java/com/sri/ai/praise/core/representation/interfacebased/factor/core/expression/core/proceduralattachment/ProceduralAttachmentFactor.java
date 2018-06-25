package com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.proceduralattachment;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.IdentityFactor.IDENTITY_FACTOR;
import static com.sri.ai.praise.other.integration.proceduralattachment.core.EmptyProcedurePayload.EMPTY_PROCEDURE_PAYLOAD;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;
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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.AbstractExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;

public class ProceduralAttachmentFactor extends AbstractExpressionFactor {
	
	private static final long serialVersionUID = 1L;
	
	private ExpressionVariable variable;
	private Procedure<?> procedure;
	
	public ProceduralAttachmentFactor(ExpressionVariable variable, Procedure<?> procedure, Context context) {
		super(context);
		this.variable = variable;
		this.procedure = procedure;
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
	public boolean contains(Variable variable) {
		boolean result = variable.equals(this.variable);
		return result;
	}

	@Override
	public List<? extends Variable> getVariables() {
		return unmodifiableList(list(variable));
	}

	@Override
	public Factor multiply(Factor another) {
		myAssert(another instanceof ExpressionFactor, () -> this.getClass() + ".multiply requires " + ExpressionFactor.class + " argument but got  instance of " + another.getClass());
		Expression anotherExpression = (ExpressionFactor) another;
		Expression anotherExpressionWithValue = anotherExpression.replaceAllOccurrences(variable, getProcedureValue(), getContext());
		ExpressionFactor result = new DefaultExpressionFactor(anotherExpressionWithValue, getContext());
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
		Factor result = getExpressionFactor().add(another);
		return result;
	}

	@Override
	public Factor invert() {
		Factor result = getExpressionFactor().invert();
		return result;
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		Factor result = getExpressionFactor().max(variablesToMaximize);
		return result;
	}
	
	private ExpressionFactor getExpressionFactor() {
		ExpressionFactor result = new DefaultExpressionFactor(getInnerExpression(), getContext());
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		Expression result = IfThenElse.make(Equality.make(variable, getProcedureValue()), Expressions.ONE, Expressions.ZERO);
		return result;
	}
}
