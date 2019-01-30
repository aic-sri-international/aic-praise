package com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.FromRealExpressionVariableToRealVariableWithRange.makeRealVariableWithRange;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Random;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.UniformDiscreteSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.UniformIntegerSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.UniformRealSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.UniformSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.function.api.values.Value;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Unit;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.values.SetOfIntegerValues;
import com.sri.ai.util.function.core.variables.DefaultSetOfVariables;
import com.sri.ai.util.function.core.variables.EnumVariable;
import com.sri.ai.util.function.core.variables.IntegerVariable;

public class ExpressionDiscretization {

	/**
	 * Prepare a {@link SetOfVariables} specifying discretization for the {@link Expression}s 
	 * in a given a factor and a function indicating the number of discrete variables specified for each expression variable.
	 * 
	 * @param expressions
	 * @param numberOfDiscreteValues
	 * @param context
	 * @return
	 */
	public static SetOfVariables makeSetOfVariablesWithRanges(
			Collection<? extends Expression> expressions, 
			Function<Expression, Integer> numberOfDiscreteValues,
			Registry context) {
		
		ArrayList<Variable> variables = 
				mapIntoArrayList(
						expressions,
						v -> makeVariableWithRange(v, numberOfDiscreteValues.apply(v), context));
		
		SetOfVariables result = new DefaultSetOfVariables(variables);
		return result;
	
	}

	public static Variable makeVariableWithRange(Expression expression, Integer numberOfDiscreteValues, Registry context) {
		Variable result;
		String name = expression.toString();
		Type type = context.getTypeOfRegisteredSymbol(expression);
		if (type instanceof RealInterval) {
			result = makeRealVariableWithRange(name, (RealInterval) type, numberOfDiscreteValues, context);
		}
		else if (type instanceof IntegerInterval) {
			result = makeIntegerVariableWithRange(name, (IntegerInterval) type, context);
		}
		else if (type instanceof Categorical) {
			result = makeEnumVariableWithRange(name, (Categorical) type, context);
		}
		else {
			throw new Error("Discretization only supports real, integer and enum types, but got variable " + expression + " of type " + type);
		}
		return result;
	}

	public static Variable makeIntegerVariableWithRange(String name, IntegerInterval type, Registry context) {
		int first = type.getNonStrictLowerBound().intValue();
		int last = type.getNonStrictUpperBound().intValue();
		SetOfIntegerValues setOfIntegerValues = new SetOfIntegerValues(first, last);
		IntegerVariable integerVariable = new IntegerVariable(name, Unit.NONE, setOfIntegerValues);
		return integerVariable;
	}

	public static Variable makeEnumVariableWithRange(String name, Categorical type, Registry context) {
		myAssert(Expressions.isNumber(type.cardinality()), () -> "Discretization requires categorical types to have known finite cardinality, but got " + type);
		String[] values = mapIntoArray(String.class, type.cardinality().intValue(), type.iterator(), Expression::toString);
		return new EnumVariable(name, values);
	}

	public static UniformSamplingFactor makeUniformSamplingFactor(ExpressionVariable variable, Random random, Registry context) {
		UniformSamplingFactor result;
		Type type = context.getTypeOfRegisteredSymbol(variable);
		if (type instanceof RealInterval) {
			RealInterval realInterval = (RealInterval) type;
			result = 
					new UniformRealSamplingFactor(
							variable, 
							realInterval.getLowerBound().doubleValue(), 
							realInterval.getUpperBound().doubleValue(), 
							new DoublePotentialFactory(), 
							random);
		}
		else if (type instanceof IntegerInterval) {
			IntegerInterval integerInterval = (IntegerInterval) type;
			result = 
					new UniformIntegerSamplingFactor(
							variable, 
							integerInterval.getNonStrictLowerBound().intValue(), 
							integerInterval.getNonStrictUpperBound().intValue() + 1, 
							new DoublePotentialFactory(), 
							random);
		}
		else if (type instanceof Categorical) {
			Categorical categoricalType = (Categorical) type;
			result = 
					new UniformDiscreteSamplingFactor<String>(
							variable, 
							() -> categoricalType.sampleUniquelyNamedConstant(random).toString(), 
							categoricalType.cardinality().intValue(), 
							new DoublePotentialFactory(), 
							random);
		}
		else {
			throw new Error("Discretization only supports real, integer and enum types, but got variable " + variable + " of type " + type);
		}
		return result;
	}

	public
	static
	com.sri.ai.grinder.interpreter.Assignment fromFunctionAssignmentToExpressoAssigment(com.sri.ai.util.function.api.variables.Assignment assignment) {
		Map<Expression, Expression> expressoMap = map();
		for (com.sri.ai.util.function.api.variables.Variable variable : assignment.getSetOfVariables().getVariables()) {
			Value value = assignment.get(variable);
			Expression valueExpression = makeSymbol(value.objectValue());
			Expression variableExpression = makeSymbol(variable.getName().toString());
			expressoMap.put(variableExpression, valueExpression);
		}
		return new com.sri.ai.grinder.interpreter.DefaultAssignment(expressoMap);
	}

}
