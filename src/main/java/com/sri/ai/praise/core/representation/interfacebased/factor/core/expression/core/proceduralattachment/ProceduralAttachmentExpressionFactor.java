package com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.proceduralattachment;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.AbstractExpressionFactor;
import com.sri.ai.praise.other.integration.proceduralattachment.api.Procedure;

/**
 * An {@link ExpressionFactor} that obtains a value for a variable and indicates potential 1 if it is equal to that value or 0 otherwise.
 * This only works for Integers, Doubles and Booleans values (for variables in compatible expresso types).
 * Real-typed variables are not compared directly to the obtained value, but checked to be within a 1% error margin
 * (due to current difficulties of linear real arithmetic theory with equality).
 * 
 * @author braz
 *
 */
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
		Variable variable = proceduralAttachment.getVariable();
		Object value = proceduralAttachment.getProcedureValue();
		Symbol valueExpression = DefaultSymbol.createSymbol(value);
		Type type = getVariableType(variable, getContext());
		Expression condition;
		if (value instanceof Double) {
			assertVariableIsOfRealType(variable, type, getContext());
			condition = makeIntervalFactorCondition(valueExpression);
		}
		else if (value instanceof Integer) {
			assertVariableIsOfIntegerType(variable, type, getContext());
			condition = makeEquality(valueExpression);
		}
		else if (value instanceof Boolean) {
			assertVariableIsOfBooleanType(variable, type, getContext());
			condition = makeBooleanCondition(valueExpression);
		}
		else {
			throw new Error("Procedural attachments must provide only Double or Integer objects, but procedure for " + proceduralAttachment.getVariable() + " provided " + proceduralAttachment.getProcedureValue() + " of type " + proceduralAttachment.getProcedureValue().getClass());
		}
		Expression result = IfThenElse.make(condition, Expressions.ONE, Expressions.ZERO);
		return result;
	}

	private Type getVariableType(Variable variable, Context context) {
		myAssert(variable instanceof ExpressionVariable, () -> getClass() + " must be defined for " + ExpressionVariable.class + ", but one of them has been defined for " + proceduralAttachment.getVariable() + " of type " + variable.getClass());
		ExpressionVariable expressionVariable = (ExpressionVariable) variable;
		myAssert(context.containsSymbol(expressionVariable), () -> "There is a procedural attachment for variable " + expressionVariable + " but it is not registered in the context");
		Type type = context.getTypeOfRegisteredSymbol(expressionVariable);
		myAssert(type != null, () -> "There is a procedural attachment for variable " + expressionVariable + " but its type is unknown");
		return type;
	}

	private void assertVariableIsOfRealType(Variable variable, Type type, Context context) {
		myAssert(type instanceof RealExpressoType || type instanceof RealInterval, () -> "Procedural attachment provided Double object, but corresponding variable " + proceduralAttachment.getVariable() + " is not of an expresso Real or real interval type, but instead is registered as " + type);
	}

	private void assertVariableIsOfIntegerType(Variable variable, Type type, Context context) {
		myAssert(type instanceof IntegerExpressoType || type instanceof IntegerInterval, () -> "Procedural attachment provided Integer object, but corresponding variable " + proceduralAttachment.getVariable() + " is not of an expresso Integer or integer interval type, but instead is registered as " + type);
	}

	private void assertVariableIsOfBooleanType(Variable variable, Type type, Context context) {
		myAssert(type.getName().equals("Boolean"), () -> "Procedural attachment provided Boolean object, but corresponding variable " + proceduralAttachment.getVariable() + " is not of the expresso Boolean type, but instead is registered as " + type);
	}

	private Expression makeIntervalFactorCondition(Symbol valueExpression) {
		double doubleValue = valueExpression.doubleValue();
		double minimum = doubleValue * 0.99;
		double maximum = doubleValue * 1.01;
		Expression minimumExpression = DefaultSymbol.createSymbol(minimum);
		Expression maximumExpression = DefaultSymbol.createSymbol(maximum);
		Expression greaterThan = Expressions.apply(FunctorConstants.GREATER_THAN, proceduralAttachment.getVariable(), minimumExpression);
		Expression lessThan    = Expressions.apply(FunctorConstants.LESS_THAN,    proceduralAttachment.getVariable(), maximumExpression);
		Expression result = And.make(greaterThan, lessThan);
		return result;
	}

	private Expression makeEquality(Symbol valueExpression) {
		Expression equalityComparison = Equality.make(proceduralAttachment.getVariable(), valueExpression);
		return equalityComparison;
	}

	private Expression makeBooleanCondition(Symbol valueExpression) {
		boolean booleanValue = valueExpression.booleanValue();
		Expression booleanCondition;
		if (booleanValue) {
			booleanCondition = (ExpressionVariable) proceduralAttachment.getVariable();
		}
		else {
			booleanCondition = Not.make((ExpressionVariable) proceduralAttachment.getVariable());
		}
		return booleanCondition;
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

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
		// TODO Auto-generated method stub
		return null;
	}
}
