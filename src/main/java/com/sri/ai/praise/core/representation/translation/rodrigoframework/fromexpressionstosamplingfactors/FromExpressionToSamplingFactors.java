package com.sri.ai.praise.core.representation.translation.rodrigoframework.fromexpressionstosamplingfactors;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.getConstantDoubleValueOrThrowErrorWithMessage;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.grinder.library.FunctorConstants.AND;
import static com.sri.ai.grinder.library.FunctorConstants.DIVISION;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EXPONENTIATION;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.grinder.library.FunctorConstants.OR;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.library.FunctorConstants.TIMES;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.thenBranch;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable.expressionVariable;
import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor.conditionResult;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.Predicate;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ConditionedSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.ConstantSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.TableSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.EqualitySamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.logic.ConjunctionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.logic.DisjunctionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.logic.NegationSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.DivisionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.ExponentiationSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.MultiplicationSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.SubtractionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.SumSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.UnaryMinusSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison.GreaterThanOrEqualToSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison.GreaterThanSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison.LessThanOrEqualToSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.number.comparison.LessThanSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.statistics.NormalWithFixedMeanAndStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.library.statistics.NormalWithFixedStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultSample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoubleImportanceFactory;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;

public class FromExpressionToSamplingFactors {
	
	private Predicate<Expression> isVariable;

	private Random random;
	
	private int uniqueVariableOccurrenceIndex;

	@SuppressWarnings("unused")
	private Registry context;
	
	public FromExpressionToSamplingFactors(Predicate<Expression> isVariable, Random random, Registry context) {
		this.isVariable = isVariable;
		this.random = random;
		this.uniqueVariableOccurrenceIndex = 0;
		this.context = context;
	}

	public List<? extends Factor> factorCompilation(Expression expression) {
		List<SamplingFactor> factors = list();
		factorCompilation(expression, factors);
		List<SamplingFactor> factorsConditionedOnConstants = mapIntoList(factors, this::conditionOnConstants);
		return factorsConditionedOnConstants;
	}
	
	private void factorCompilation(Expression factorExpressionPossiblyIfThenElse10, List<SamplingFactor> factors) {
		
		Expression expression = transformIfThenElse10IntoTheirConditions(factorExpressionPossiblyIfThenElse10);
		
		Histogram histogram;
		
		if (isFormulaAssertedToBeTrueWithProbabilityOne(expression)) {
			factorCompilation(IfThenElse.condition(expression), factors);
		}
		else if (isVariable.test(expression)) {
			booleanVariableFactorCompilation(expression, factors);
		}
		else if ((histogram = Histogram.getHistogramOrNull(expression)) != null) {
			histogramFactorCompilation(histogram, factors);
		}
		else if (expression.hasFunctor(IF_THEN_ELSE)) {
			ifThenElseFactorCompilation(expression, factors);
		}
		else if (expression.hasFunctor(EQUALITY)) {
			equalityFactorCompilation(expression, factors);
		}
		else if (expression.hasFunctor(LESS_THAN)) {
			lessThanFactorCompilation(expression, factors);
		}
		else if (expression.hasFunctor(GREATER_THAN)) {
			greaterFactorThanCompilation(expression, factors);
		}
		else if (expression.hasFunctor(LESS_THAN_OR_EQUAL_TO)) {
			lessThanOrEqualToFactorCompilation(expression, factors);
		}
		else if (expression.hasFunctor(GREATER_THAN_OR_EQUAL_TO)) {
			greaterThanOrEqualToFactorCompilation(expression, factors);
		}
		else if (expression.hasFunctor(OR)) {
			orFactorCompilation(expression, factors);
		}
		else if (expression.hasFunctor(AND)) {
			andFactorCompilation(expression, factors);
		}
		else if (expression.hasFunctor(NOT)) {
			notFactorCompilation(expression, factors);
		}
		else {
			throw new Error("Conversion to sampling factors not yet implemented for " + expression);
		}
	}

	

	//////////////////

	private Expression transformIfThenElse10IntoTheirConditions(Expression expression) {
		return 
				expression.replaceAllOccurrences(
						e -> isFormulaAssertedToBeTrueWithProbabilityOne(e)? IfThenElse.condition(e) : e, 
								new TrueContext());
	}
	
	//////////////////

	private void booleanVariableFactorCompilation(Expression expression, List<SamplingFactor> factors) {
		ExpressionVariable variable = DefaultExpressionVariable.expressionVariable(expression);
		SamplingFactor truthFactor = new ConstantSamplingFactor(variable, true, random);
		factors.add(truthFactor);
	}
	
	private void histogramFactorCompilation(Histogram histogram, List<SamplingFactor> factors) {
		Variable variable = expressionVariable(histogram.getVariable());
		ArrayList<Object> values = mapIntoArrayList(histogram, pair -> pair.first.doubleValue());
		ArrayList<Double> probabilities = mapIntoArrayList(histogram, pair -> pair.second.doubleValue());
		TableSamplingFactor histogramFactor = new TableSamplingFactor(variable, values, probabilities, getRandom());
		factors.add(histogramFactor);
	}

	private void ifThenElseFactorCompilation(Expression expression, List<SamplingFactor> factors) {
		Expression condition = IfThenElse.condition(expression);
		Expression thenBranch = IfThenElse.thenBranch(expression);
		Expression elseBranch = IfThenElse.elseBranch(expression);

		Expression conditionAndThenBranch = And.make(condition, thenBranch);
		Expression notCondition = Not.make(condition);
		Expression notConditionAndElseBranch = And.make(notCondition, elseBranch);
		
		Variable conditionVariable = expressionCompilation(condition, factors);
		Variable thenBranchVariable = expressionCompilation(thenBranch, factors);
		Variable elseBranchVariable = expressionCompilation(elseBranch, factors);
		
		Variable conditionAndThenBranchVariable = makeUniqueRandomVariable(conditionAndThenBranch);
		factors.add(
				new ConjunctionSamplingFactor(
						conditionAndThenBranchVariable, 
						list(conditionVariable, thenBranchVariable),
						getRandom()));
		
		Variable notConditionVariable = makeUniqueRandomVariable(notCondition);
		factors.add(
				new NegationSamplingFactor(
						notConditionVariable, 
						conditionVariable,
						getRandom()));
		
		Variable notConditionAndElseBranchVariable = makeUniqueRandomVariable(notConditionAndElseBranch);
		factors.add(
				new ConjunctionSamplingFactor(
						notConditionAndElseBranchVariable, 
						list(notConditionVariable, elseBranchVariable),
						getRandom()));
		
		factors.add(
				new DisjunctionSamplingFactor(
						expressionVariable(TRUE), // needs to be an expression variable with a constant to be conditioned to be always true
						list(conditionAndThenBranchVariable, notConditionAndElseBranchVariable),
						getRandom()));
	}
	
	private void equalityFactorCompilation(Expression expression, List<SamplingFactor> factors) {
		Variable leftHandSideVariable = expressionCompilation(expression.get(0), factors);
		Variable rightHandSideVariable = expressionCompilation(expression.get(1), leftHandSideVariable, factors);
		boolean leftHandSideVariableWasReusedAsRightHandSideVariable = rightHandSideVariable == leftHandSideVariable;
		if (leftHandSideVariableWasReusedAsRightHandSideVariable) {
			// no need to enforce equality
		}
		else {
			ArrayList<Variable> argumentVariables = arrayList(leftHandSideVariable, rightHandSideVariable);
			trueBooleanVarArgSamplingFactorCompilation(EqualitySamplingFactor.class, argumentVariables, factors);
		}
	}

	private void lessThanFactorCompilation(Expression expression, List<SamplingFactor> factors) {
		trueBooleanVarArgSamplingFactorCompilation(LessThanSamplingFactor.class, expression, factors);
	}

	private void greaterFactorThanCompilation(Expression expression, List<SamplingFactor> factors) {
		trueBooleanVarArgSamplingFactorCompilation(GreaterThanSamplingFactor.class, expression, factors);
	}

	private void lessThanOrEqualToFactorCompilation(Expression expression, List<SamplingFactor> factors) {
		trueBooleanVarArgSamplingFactorCompilation(LessThanOrEqualToSamplingFactor.class, expression, factors);
	}

	private void greaterThanOrEqualToFactorCompilation(Expression expression, List<SamplingFactor> factors) {
		trueBooleanVarArgSamplingFactorCompilation(GreaterThanOrEqualToSamplingFactor.class, expression, factors);
	}

	private void orFactorCompilation(Expression expression, List<SamplingFactor> factors) {
		trueBooleanVarArgSamplingFactorCompilation(DisjunctionSamplingFactor.class, expression, factors);
	}

	private void andFactorCompilation(Expression expression, List<SamplingFactor> factors) {
		expression.getArguments().forEach(a -> factorCompilation(a, factors));
	}

	private void notFactorCompilation(Expression expression, List<SamplingFactor> factors) {
		Variable expressionVariable = expressionCompilation(expression, factors);
		SamplingFactor resultFactor = 
				getFirstSatisfyingPredicateOrNull(
				factors, 
				f -> f.getVariables().get(0).equals(expressionVariable));
		myAssert(resultFactor != null, () -> "Factors generated for negation " + expression + " do not contain result variable '" + expressionVariable + "'");
		SamplingFactor conditionedToFalse = SamplingFactor.condition(resultFactor, expressionVariable, true);
		factors.remove(resultFactor);
		factors.add(conditionedToFalse);
	}

	//////////////////////
	
	private void trueBooleanVarArgSamplingFactorCompilation(Class<? extends SamplingFactor> samplingFactorClass, Expression expression, List<SamplingFactor> factors) {
		List<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		trueBooleanVarArgSamplingFactorCompilation(samplingFactorClass, argumentVariables, factors);
	}

	private void trueBooleanVarArgSamplingFactorCompilation(Class<? extends SamplingFactor> samplingFactorClass, List<Variable> argumentVariables, List<SamplingFactor> factors) {
		booleanVarArgSamplingFactorCompilation(true, samplingFactorClass, argumentVariables, factors);
	}

	private void booleanVarArgSamplingFactorCompilation(
			boolean truthValue,
			Class<? extends SamplingFactor> samplingFactorClass, 
			List<Variable> argumentVariables,
			List<SamplingFactor> factors) {
		
		SamplingFactor factor = 
				conditionResult(
						truthValue, 
						truth -> {
							Constructor<?> constructor = samplingFactorClass.getConstructors()[0];
							try {
								return (SamplingFactor) constructor.newInstance(truth, argumentVariables, random);
							} catch (InstantiationException | IllegalAccessException | IllegalArgumentException
									| InvocationTargetException e) {
								throw new Error(e);
							}	
						});
		factors.add(factor);
	}

	////////////////////
	
	private Variable expressionCompilation(Expression expression, List<SamplingFactor> factors) {
		return expressionCompilation(expression, null, factors);
	}

	private Variable expressionCompilation(Expression expression, Variable compoundExpressionVariableIfAvailable, List<SamplingFactor> factors) {
		Variable variable;
		if (expression instanceof Variable) {
			variable = (Variable) expression; // has already been compiled
		}
		else if (isVariable(expression)) {
			variable = DefaultExpressionVariable.expressionVariable(expression);
		}
		else if (isConstant(expression)) {
			variable = expressionVariable(expression);
		}
		else {
			variable = compoundExpressionCompilation(expression, compoundExpressionVariableIfAvailable, factors);
		}
		return variable;
	}

	private Variable compoundExpressionCompilation(Expression expression, Variable compoundExpressionVariableIfAvailable, List<SamplingFactor> factors) throws Error {
		Variable compoundExpressionVariable = 
				compoundExpressionVariableIfAvailable != null
				? compoundExpressionVariableIfAvailable : makeUniqueRandomVariable(expression);
		
		if (isConstant(expression)) {
			constantCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(IF_THEN_ELSE)) {
			ifThenElseFunctionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(PLUS)) {
			sumCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(MINUS) && expression.numberOfArguments() == 2) {
			subtractionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(TIMES)) {
			multiplicationCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(DIVISION)) {
			divisionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(EXPONENTIATION)) {
			exponentiationCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(MINUS) && expression.numberOfArguments() == 1) {
			unaryMinusCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(AND)) {
			andFunctionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(OR)) {
			orFunctionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(NOT) && expression.numberOfArguments() == 1) {
			negationFunctionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(EQUALITY)) {
			equalityFunctionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(LESS_THAN)) {
			lessThanFunctionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(GREATER_THAN)) {
			greaterThanFunctionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(LESS_THAN_OR_EQUAL_TO)) {
			lessThanOrEqualToFunctionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor(GREATER_THAN_OR_EQUAL_TO)) {
			greaterThanOrEqualToFunctionCompilation(compoundExpressionVariable, expression, factors);
		}
		else if (expression.hasFunctor("Normal")) {
			normalCompilation(compoundExpressionVariable, expression, factors);
		}
		else {
			throw new Error("Translation to sampling factors has not yet been defined for " + expression);
		}
		return compoundExpressionVariable;
	}

	private ExpressionVariable makeUniqueRandomVariable(Expression expression) {
		Expression unique = Expressions.apply("unique", expression, uniqueVariableOccurrenceIndex++);
		ExpressionVariable result = expressionVariable(unique);
		return result;
	}

	private void constantCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		SamplingFactor constantFactor = new ConstantSamplingFactor(compoundExpressionVariable, fromExpressionValueToSampleValue(expression), random);
		factors.add(constantFactor);
	}

	private void ifThenElseFunctionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		Expression condition  = IfThenElse.condition(expression);
		Expression thenBranch = IfThenElse.thenBranch(expression);
		Expression elseBranch = IfThenElse.elseBranch(expression);

		Expression compoundExpression = ((ExpressionVariable) compoundExpressionVariable);
		
		Expression compoundEqualsThenBranch = Equality.make(compoundExpression, thenBranch);
		Expression compoundEqualsElseBranch = Equality.make(compoundExpression, elseBranch);
		
		Expression ifThenElseFactor = IfThenElse.make(condition, compoundEqualsThenBranch, compoundEqualsElseBranch);
		
		ifThenElseFactorCompilation(ifThenElseFactor, factors);
	}

	private void sumCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor sumFactor = new SumSamplingFactor(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(sumFactor);
	}

	private void subtractionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor subtractionFactor = new SubtractionSamplingFactor(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(subtractionFactor);
	}

	private void multiplicationCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor sumFactor = new MultiplicationSamplingFactor(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(sumFactor);
	}

	private void divisionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor divisionFactor = new DivisionSamplingFactor(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(divisionFactor);
	}

	private void exponentiationCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor exponentiationFactor = new ExponentiationSamplingFactor(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(exponentiationFactor);
	}

	private void unaryMinusCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor unaryMinusFactor = new UnaryMinusSamplingFactor(compoundExpressionVariable, argumentVariables.get(0), getRandom());
		factors.add(unaryMinusFactor);
	}

	private void andFunctionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor andFactor = new ConjunctionSamplingFactor(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(andFactor);
	}

	private void orFunctionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor orFactor = new DisjunctionSamplingFactor(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(orFactor);
	}

	private void negationFunctionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor negationFactor = new NegationSamplingFactor(compoundExpressionVariable, argumentVariables.get(0), getRandom());
		factors.add(negationFactor);
	}

	private void equalityFunctionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor equalityFunctionFactor = new EqualitySamplingFactor(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(equalityFunctionFactor);
	}

	private void lessThanFunctionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor lessThanFunctionFactor = new LessThanSamplingFactor<Double>(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(lessThanFunctionFactor);
	}

	private void lessThanOrEqualToFunctionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor lessThanOrEqualToFunctionFactor = new LessThanOrEqualToSamplingFactor<Double>(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(lessThanOrEqualToFunctionFactor);
	}

	private void greaterThanFunctionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor greaterThanFunctionFactor = new GreaterThanSamplingFactor<Double>(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(greaterThanFunctionFactor);
	}

	private void greaterThanOrEqualToFunctionCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		ArrayList<Variable> argumentVariables = mapIntoArrayList(expression.getArguments(), a -> expressionCompilation(a, factors));
		SamplingFactor greaterThanOrEqualToFunctionFactor = new GreaterThanOrEqualToSamplingFactor<Double>(compoundExpressionVariable, argumentVariables, getRandom());
		factors.add(greaterThanOrEqualToFunctionFactor);
	}

	private void normalCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) throws Error {
		int numberOfArguments = expression.numberOfArguments();
		myAssert(numberOfArguments == 2, () -> "the Normal distribution must have two arguments, but this one has " + expression.getArguments());
		if (isNumber(expression.get(0))) {
			normalWithFixedMeanCompilation(compoundExpressionVariable, expression, factors);
		}
		else {
			normalWithVariableMeanCompilation(compoundExpressionVariable, expression, factors);
		}
	}

	private void normalWithFixedMeanCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		double mean = getMean(expression);
		double standardDeviation = getStandardDeviation(expression);
		SamplingFactor normalFactor = new NormalWithFixedMeanAndStandardDeviation(compoundExpressionVariable, mean, standardDeviation, getRandom());
		factors.add(normalFactor);
	}

	private void normalWithVariableMeanCompilation(Variable compoundExpressionVariable, Expression expression, List<SamplingFactor> factors) {
		Variable meanVariable = expressionCompilation(expression.get(0), factors);
		double standardDeviation = getStandardDeviation(expression);
		SamplingFactor normalFactor = new NormalWithFixedStandardDeviation(compoundExpressionVariable, meanVariable, standardDeviation, getRandom());
		factors.add(normalFactor);
	}

	private double getMean(Expression expression) throws Error {
		double standardDeviation = 
				getConstantDoubleValueOrThrowErrorWithMessage(
						expression.get(0), 
						"Normal must take a constant mean, but got " + expression.get(0) + " instead");
		return standardDeviation;
	}

	private double getStandardDeviation(Expression expression) throws Error {
		double standardDeviation = 
				getConstantDoubleValueOrThrowErrorWithMessage(
						expression.get(1), 
						"Normal must take a constant standard deviation, but got " + expression.get(1) + " instead");
		return standardDeviation;
	}

	private boolean isVariable(Expression expression) {
		boolean result = isVariable.test(expression);
		return result;
	}

	private boolean isConstant(Expression expression) {
		boolean result = 
				expression.getSyntacticFormType().equals("Symbol") 
				&& 
				! isVariable(expression);
		return result;
	}

	private Object fromExpressionValueToSampleValue(Expression expression) {
		Object expressionValue = expression.getValue();
		if (expressionValue instanceof Number) {
			return ((Number) expressionValue).doubleValue();
		}
		else {
			return expressionValue;
		}
	}

	public Random getRandom() {
		return random;
	}

	private boolean isFormulaAssertedToBeTrueWithProbabilityOne(Expression expression) {
		boolean result = 
				isIfThenElse(expression) 
				&& 
				thenBranch(expression).equals(ONE) 
				&& 
				elseBranch(expression).equals(ZERO);
		return result;
	}
	
	private SamplingFactor conditionOnConstants(SamplingFactor factor) {
		Sample sample = makeSampleFromConstantVariablesOf(factor);
		SamplingFactor result = conditionOnSampleIfNotEmpty(factor, sample);
		return result;
	}

	private Sample makeSampleFromConstantVariablesOf(SamplingFactor factor) {
		Sample sample = new DefaultSample(new DoubleImportanceFactory(), new DoublePotentialFactory());
		for (Variable variable : factor.getVariables()) {
			ExpressionVariable expressionVariable = (ExpressionVariable) variable;
			if (isConstant(expressionVariable)) {
				sample.getAssignment().set(expressionVariable, fromExpressionValueToSampleValue(expressionVariable));
			}
		}
		return sample;
	}

	private SamplingFactor conditionOnSampleIfNotEmpty(SamplingFactor factor, Sample sample) {
		SamplingFactor result;
		if (sample.size() != 0) {
			result = ConditionedSamplingFactor.condition(factor, sample);
		}
		else {
			result = factor;
		}
		return result;
	}

}
