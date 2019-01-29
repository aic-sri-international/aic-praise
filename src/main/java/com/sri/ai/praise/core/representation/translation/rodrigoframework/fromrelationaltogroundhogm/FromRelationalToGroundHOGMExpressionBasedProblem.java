package com.sri.ai.praise.core.representation.translation.rodrigoframework.fromrelationaltogroundhogm;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.makeIndexExpression;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.UniversallyQuantifiedFormula;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedModel;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMExpressionBasedProblem;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMSortDeclaration;
import com.sri.ai.praise.core.representation.classbased.hogm.components.HOGMVariableDeclaration;
import com.sri.ai.util.NewIdentifierMaker;
import com.sri.ai.util.base.Pair;

public class FromRelationalToGroundHOGMExpressionBasedProblem {
	
	private static final LinkedList<String> BUILT_IN_TYPE_NAMES = list("Integer", "String", "Real", "Boolean");
	private static final LinkedList<String> BUILT_IN_FUNCTION_STRINGS = 
			list(
					"Normal", 
					"+", 
					"-", 
					"*", 
					"/", 
					"^",
					"=",
					"!=",
					"<",
					">",
					"<=",
					">=",
					"and",
					"or",
					"not",
					"=>",
					"<=>",
					"'if . then . else .'"
					);
	

	public static ExpressionBasedProblem translate(HOGMExpressionBasedProblem problem) {
		
		String modelString = getModelString(problem.getHOGMExpressionBasedModel());
		
		HOGMExpressionBasedModel model = new HOGMExpressionBasedModel(modelString);
		ExpressionBasedProblem result = new HOGMExpressionBasedProblem(parse("query"), model);
		
		return result;
	}

	public static String getModelString(HOGMExpressionBasedModel model) {
		StringBuilder groundedModelString = new StringBuilder();

		groundedModelString.append(getSorts(model));
		groundedModelString.append(getVariableDeclarations(model, model.getHOGModel().getConstantDeclarations()));
		groundedModelString.append(getVariableDeclarations(model, model.getHOGModel().getRandomVariableDeclarations()));
		groundedModelString.append(getFactors(model, model.getHOGModel().getConditionedPotentials(), model.getContext()));
		
		String modelString = groundedModelString.toString();
		
		return modelString;
	}

	private static String getSorts(HOGMExpressionBasedModel model) {
		return explanationBlock("Copying sorts", code(() -> {
			
			StringBuilder result = new StringBuilder();
			for (HOGMSortDeclaration sortDeclaration : model.getHOGModel().getSortDeclarations()) {
				if (!BUILT_IN_TYPE_NAMES.contains(sortDeclaration.getName().toString())) {
					result.append(sortDeclaration.toHOGMString() + "\n");
				}
			}
			result.append("\n");
			return result.toString();
			
		}));
	}
	
	private static String getVariableDeclarations(HOGMExpressionBasedModel model, List<? extends HOGMVariableDeclaration> randomVariableDeclarations) {
		return explanationBlock("Grounding variable declarations", code(() -> {

			StringBuilder result = new StringBuilder();
			for (HOGMVariableDeclaration declaration : randomVariableDeclarations) {
				if (!BUILT_IN_FUNCTION_STRINGS.contains(declaration.getName().toString())) {
					result.append(grounding(declaration, model.getContext()) + "\n");
				}
				else {
					result.append(declaration.toHOGMString() + "\n");
				}
			}
			return result.toString();

		}));
	}

	private static String grounding(HOGMVariableDeclaration declaration, Context context) {
		return explanationBlock("Grounding ", declaration, code(() -> {

			ExtensionalIndexExpressionsSet parameters = makeParameters(declaration, context);
			StringBuilder result = new StringBuilder();
			AssignmentsIterator parameterValues = new AssignmentsIterator(parameters, context);
			for (Map<Expression, Expression> assignment : in(parameterValues)) {
				List<Expression> argumentValues = getArgumentValues(parameters, assignment);
				result.append(grounding(declaration, argumentValues, context) + "\n");
			}
			return result.toString();

		}));
	}

	private static List<Expression> getArgumentValues(ExtensionalIndexExpressionsSet parameters, Map<Expression, Expression> assignment) {
		return mapIntoList(parameters.getList(), indexExpression -> assignment.get(indexExpression.get(0)));
	}

	private static ExtensionalIndexExpressionsSet makeParameters(HOGMVariableDeclaration declaration, Context context) {
		Function<String, String> newIdentifierMaker = new NewIdentifierMaker(s -> !context.getSymbols().contains(makeSymbol(s)));
		List<Expression> indexExpressions = mapIntoList(declaration.getParameterSorts(), e -> makeParameterIndexExpression(e, newIdentifierMaker, context));
		ExtensionalIndexExpressionsSet result = new ExtensionalIndexExpressionsSet(indexExpressions);
		return result;
	}
	
	private static Expression makeParameterIndexExpression(Expression sort, Function<String, String> newIdentifierMaker, Context context) {
		String identifier = newIdentifierMaker.apply("parameter");
		Expression indexExpression = makeIndexExpression(makeSymbol(identifier), sort);
		return indexExpression;
	}

	private static String grounding(HOGMVariableDeclaration declaration, Collection<Expression> argumentValues, Context context) {
		Expression functor = declaration.getName();
		String name = makeGroundVariableName(functor, argumentValues);
		Expression type = declaration.getRangeSort();
		String result = declaration.getHOGMModifier() + " " + name + ": " + type + ";";
		return result;
	}

	private static String makeGroundVariableName(Expression functor, Collection<Expression> argumentValues) {
		return explanationBlock("Grounding ", functor, "(", argumentValues, ")", code(() -> {

			List<String> nameComponents = list();
			nameComponents.add(functor.toString());
			if (!argumentValues.isEmpty()) {
				nameComponents.add("_");
			}
			mapIntoList(argumentValues, Expression::toString, nameComponents);
			String name = join("_", nameComponents);
			return name;

		}));
	}

	private static String getFactors(HOGMExpressionBasedModel model, List<Expression> factors, Context context) {
		return join("\n", mapIntoList(factors, f -> groundFactor(f, context))) + "\n";
	}
	
	private static String groundFactor(Expression factor, Context context) {
		return explanationBlock("Grounding factor ", factor, code(() -> {
			
			if (IfThenElse.isIfThenElse(factor)) {
				return groundIfThenElse(factor, context);
			}
			else {
				return groundFactorThatIsNotAnDeterministicIfThenElse(factor, context);
			}

		}));
	}

	private static String groundIfThenElse(Expression factor, Context context) {
		Expression actualFactor = getActualFactor(factor);
		return groundFactorThatIsNotAnDeterministicIfThenElse(actualFactor, context);
	}

	private static Expression getActualFactor(Expression factor) {
		Expression actualFactor;
		Expression condition = IfThenElse.condition(factor);
		Expression thenBranch = IfThenElse.thenBranch(factor);
		Expression elseBranch = IfThenElse.elseBranch(factor);
		if (thenBranch.equals(ONE) && elseBranch.equals(ZERO)) {
			actualFactor = condition;
		}
		else {
			actualFactor = factor;
		}
		return actualFactor;
	}

	private static String groundFactorThatIsNotAnDeterministicIfThenElse(Expression factor, Context context) {
		if (factor.getSyntacticFormType().equals("For all")) {
			return groundUniversallyQuantifiedFactor2(factor, context);
		}
		else {
			return factor.toString() + ";";
		}
	}

	private static String groundUniversallyQuantifiedFactor2(Expression factor, Context context) {
		Pair<IndexExpressionsSet, Expression> indexExpressionsAndBody = getIndexExpressionsAndBody(factor);
		IndexExpressionsSet indexExpressions = indexExpressionsAndBody.first;
		Expression body = indexExpressionsAndBody.second;
		AssignmentsIterator assignments = new AssignmentsIterator(indexExpressions, context);
		List<String> bodyGroundings = mapIntoList(assignments, a -> groundNonQuantifiedFactorWithAssignment(body, a, context).toString() + ";");
		return join("\n", bodyGroundings);
	}

	private static Pair<IndexExpressionsSet, Expression> getIndexExpressionsAndBody(Expression factor) {
		return explanationBlock("Getting index expressions and body for ", factor, code(() -> {

			List<Expression> indexExpressions = list();
			Expression current = factor;
			while (current.getSyntacticFormType().equals("For all")) {
				UniversallyQuantifiedFormula universallyQuantifiedCurrent = (UniversallyQuantifiedFormula) current;
				ExtensionalIndexExpressionsSet indexExpressionsSet = (ExtensionalIndexExpressionsSet) universallyQuantifiedCurrent.getIndexExpressions();
				indexExpressions.addAll(indexExpressionsSet.getList());
				current = universallyQuantifiedCurrent.getBody();
			}
			Expression body = current;
			ExtensionalIndexExpressionsSet indexExpressionsSet = new ExtensionalIndexExpressionsSet(indexExpressions);
			return Pair.make(indexExpressionsSet, body);

		}), "Result: ", RESULT);
	}

	private static Expression groundNonQuantifiedFactorWithAssignment(Expression factor, Map<Expression, Expression> assignment, Context context) {
		return factor.replaceAllOccurrences(e -> groundExpression(e, assignment, context), context);
	}
	
	private static Expression groundExpression(Expression expression, Map<Expression, Expression> assignment, Context context) {
		Expression directValue = assignment.get(expression);
		if (directValue != null) {
			return directValue;
		}
		else if (isGroundableFunctionApplication(expression, context)) {
			Expression groundVariable = groundFunctionApplication(expression, assignment);
			return groundVariable;
		}
		else {
			return expression;
		}
	}

	private static boolean isGroundableFunctionApplication(Expression expression, Context context) {
		return 
				expression.getFunctor() != null
				&& 
				isNotApplicationOfBuiltInFunction(expression) 
				&& 
				context.getTypeOfRegisteredSymbol(expression.getFunctor()) instanceof FunctionType;
	}

	private static boolean isNotApplicationOfBuiltInFunction(Expression expression) {
		String functorString = expression.getFunctor().toString();
		boolean isBuiltInFunctionApplication = BUILT_IN_FUNCTION_STRINGS.contains(functorString);
		return ! isBuiltInFunctionApplication;
	}

	private static Expression groundFunctionApplication(Expression expression, Map<Expression, Expression> assignment) {
		return explanationBlock("Grounding ", expression, " with ", assignment, code(() -> {

			List<Expression> argumentValues = mapIntoList(expression.getArguments(), assignment::get);
			Expression groundVariable = makeSymbol(makeGroundVariableName(expression.getFunctor(), argumentValues));
			return groundVariable;

		}), "Result is ", RESULT);
	}

}
