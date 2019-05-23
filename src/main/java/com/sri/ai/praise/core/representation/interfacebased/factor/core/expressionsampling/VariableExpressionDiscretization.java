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
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.helper.GrinderUtil;
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

public class VariableExpressionDiscretization {

	/**
	 * Prepare a {@link SetOfVariables} specifying discretization for the {@link Expression}s representing variables 
	 * in a given a factor and a function indicating the number of discrete variables specified for each expression variable.
	 * 
	 * @param variableExpressions
	 * @param numberOfDiscreteValues
	 * @param registry
	 * @return
	 */
	public static SetOfVariables makeSetOfVariablesWithRanges(
			Collection<? extends Expression> variableExpressions, 
			Function<Expression, Integer> numberOfDiscreteValues,
			Registry registry) {
		
		ArrayList<Variable> variables = 
				mapIntoArrayList(
						variableExpressions,
						e -> makeVariableWithRange(e, numberOfDiscreteValues.apply(e), registry));
		
		SetOfVariables result = new DefaultSetOfVariables(variables);
		return result;
	
	}

	/** 
	 * Make a {@link Variable} based on an expression variable, number of discrete values, and type as registered in given registry.
	 */
	public static Variable makeVariableWithRange(Expression variableExpression, Integer numberOfDiscreteValues, Registry registry) {
		Variable result;
		String name = variableExpression.toString();
		Type type = GrinderUtil.getTypeOfExpression(variableExpression, registry);
		
		
		if (type instanceof IntegerExpressoType) {
			if (Expressions.isNumber(variableExpression)) {
				int value = variableExpression.intValue();
				type = new IntegerInterval(value, value);
			}
			else {
				throw new Error("Cannot create variable with infinite (integer) range based on " + variableExpression);
			}
		}
		else if (type instanceof RealExpressoType) {
			if (Expressions.isNumber(variableExpression)) {
				type = new RealInterval(variableExpression, variableExpression, false, false);
			}
			else {
				throw new Error("Cannot create variable with infinite (real) range based on " + variableExpression);
			}
		}
		
		// Type type = registry.getTypeOfRegisteredSymbol(variable);
		if (type instanceof RealInterval) {
			result = makeRealVariableWithRange(name, (RealInterval) type, numberOfDiscreteValues, registry);
		}
		else if (type instanceof IntegerInterval) {
			result = makeIntegerVariableWithRange(name, (IntegerInterval) type);
		}
		else if (type instanceof Categorical) {
			result = makeEnumVariableWithRange(name, (Categorical) type);
		}
		else {
			throw new Error("Discretization only supports real, integer and enum types, but got variable " + variableExpression + " of type " + type);
		}
		return result;
	}

	public static Variable makeIntegerVariableWithRange(String name, IntegerInterval type) {
		int first = type.getNonStrictLowerBound().intValue();
		int last = type.getNonStrictUpperBound().intValue();
		SetOfIntegerValues setOfIntegerValues = new SetOfIntegerValues(first, last);
		IntegerVariable integerVariable = new IntegerVariable(name, Unit.NONE, setOfIntegerValues);
		return integerVariable;
	}

	public static Variable makeEnumVariableWithRange(String name, Categorical type) {
		myAssert(Expressions.isNumber(type.cardinality()), () -> "Discretization requires categorical types to have known finite cardinality, but got " + type);
		String[] values = mapIntoArray(String.class, type.cardinality().intValue(), type.iterator(), Expression::toString);
		return new EnumVariable(name, values);
	}

	/**
	 * Make a uniform sampling factor for a given {@link ExpressionVariable} according to its type registered in given registry.
	 * @param variable
	 * @param random
	 * @param registry
	 * @return
	 */
	public static UniformSamplingFactor makeUniformSamplingFactor(ExpressionVariable variable, Random random, Registry registry) {
		UniformSamplingFactor result;
		Type type = registry.getTypeOfRegisteredSymbol(variable);
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
	com.sri.ai.grinder.interpreter.Assignment 
	fromFunctionAssignmentToExpressoAssigment(
			com.sri.ai.util.function.api.variables.Assignment assignment) {
		
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
