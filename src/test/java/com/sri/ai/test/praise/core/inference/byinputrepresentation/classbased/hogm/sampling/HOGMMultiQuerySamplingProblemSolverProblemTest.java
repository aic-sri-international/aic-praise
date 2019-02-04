package com.sri.ai.test.praise.core.inference.byinputrepresentation.classbased.hogm.sampling;

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static org.junit.Assert.assertEquals;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.sampling.HOGMMultiQuerySamplingProblemSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.sampling.HOGMMultiQuerySamplingPropositionalProblemSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.classbased.hogm.solver.HOGMProblemResult;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expressionsampling.ExpressionWithProbabilityFunction;
import com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d.SamplingFactorDiscretizedProbabilityDistributionFunction;
import com.sri.ai.util.Util;
import com.sri.ai.util.function.api.functions.Function;
import com.sri.ai.util.function.api.functions.Functions;
import com.sri.ai.util.function.api.values.Value;
import com.sri.ai.util.function.api.variables.SetOfVariables;
import com.sri.ai.util.function.api.variables.Variable;
import com.sri.ai.util.function.core.values.SetOfRealValues;
import com.sri.ai.util.function.core.variables.DefaultAssignment;
import com.sri.ai.util.function.core.variables.DefaultSetOfVariables;
import com.sri.ai.util.function.core.variables.RealVariable;
import com.sri.ai.util.graph2d.api.GraphPlot;
import com.sri.ai.util.graph2d.api.GraphSetMaker;

public class HOGMMultiQuerySamplingProblemSolverProblemTest {
  @Test
  public void testRangeChangeXm() {
    String model = "random x : [-10;10]; x = Normal(0.0, 15.0);";
    String query = "x";

    HOGMMultiQuerySamplingProblemSolver queryRunner =
        new HOGMMultiQuerySamplingProblemSolver(
            model,
            Collections.singletonList(query),
            v -> 25,
            1000,
            new Random());

    List<? extends HOGMProblemResult> hogmProblemResults = queryRunner.getResults();
    final HOGMProblemResult hpResult = hogmProblemResults.get(0);
    if (!hpResult.getErrors().isEmpty()) {
      throw new RuntimeException("ERROR: " + hpResult.getErrors().get(0));
    }
    ExpressionWithProbabilityFunction expressionWithProbabilityFunction =
        ((ExpressionWithProbabilityFunction)  hpResult.getResult());
    Function function = expressionWithProbabilityFunction.getDiscretizedConditionalProbabilityDistributionFunction();

    SetOfVariables inputVariables = function.getSetOfInputVariables();
    int queryIndex =
        Util.getIndexOfFirstSatisfyingPredicateOrMinusOne(
            inputVariables.getVariables(), v -> v.getName().equals(query));

    ArrayList<? extends Variable> nonXAxisVariables = inputVariables.getVariables();
    nonXAxisVariables.remove(queryIndex);
    SetOfVariables setOfNonXAxisVariables = new DefaultSetOfVariables(nonXAxisVariables);

    List<? extends Value> values =
        Util.mapIntoList(nonXAxisVariables, v -> v.getSetOfValuesOrNull().get(0));
    DefaultAssignment assignmentOnNonXAxisVariables = new DefaultAssignment(setOfNonXAxisVariables, values);

    Functions functions = Functions.functions(function);
    Variable xmVariable = functions.getAllInputVariables().getVariables().get(queryIndex);

    GraphSetMaker graphSetMaker = GraphSetMaker.graphSetMaker();
    graphSetMaker.setFunctions(functions);
    GraphPlot graphPlot = graphSetMaker.plot(assignmentOnNonXAxisVariables, xmVariable);
    System.out.println(graphPlot);

    // Using the same function but using a different xmVariable generates the same results ??

    // use modified xm

    SetOfRealValues newSetOfRealValues = new SetOfRealValues(1, new BigDecimal(1), 5);
    RealVariable newXm = new RealVariable("x", xmVariable.getUnit(), newSetOfRealValues);

    graphSetMaker = GraphSetMaker.graphSetMaker();
    graphSetMaker.setFunctions(functions);
    graphPlot = graphSetMaker.plot(assignmentOnNonXAxisVariables, newXm);
    System.out.println(graphPlot);
  }

  // Throws a class cast exception
//  @Test
  public void testArrayOutOfBound() {
    String model =
        ""
            + "sort Counties: 4, Abiemnhom, Abyei, Akobo, AweilCentre;"
            + "random capital: Counties;"
            + "capital = Abiemnhom;"
            + "";

    String query = "capital";
    int initialNumberOfSamples = 1000;
    int numberOfDiscreteValues = 25;

    HOGMProblemResult result =
        computeResult(model, query, initialNumberOfSamples, numberOfDiscreteValues);
    Expression exp = result.getResult();
    ExpressionSamplingFactor expressionSamplingFactor = (ExpressionSamplingFactor) exp;
    SamplingFactorDiscretizedProbabilityDistributionFunction function =
        expressionSamplingFactor.getDiscretizedConditionalProbabilityDistributionFunction();
    Functions functions = Functions.functions(function);

    SetOfVariables inputVariables = function.getSetOfInputVariables();

	int queryIndex =
	        Util.getIndexOfFirstSatisfyingPredicateOrMinusOne(
	            inputVariables.getVariables(), v -> v.getName().equals(query));

    ArrayList<? extends Variable> nonXAxisVariables = inputVariables.getVariables();
    nonXAxisVariables.remove(queryIndex);
    SetOfVariables setOfNonXAxisVariables = new DefaultSetOfVariables(nonXAxisVariables);
    
    List<? extends Value> values =
        Util.mapIntoList(nonXAxisVariables, v -> v.getSetOfValuesOrNull().get(0));
    DefaultAssignment assignmentOnNonXAxisVariables = new DefaultAssignment(setOfNonXAxisVariables, values);

    Variable xmVariable = functions.getAllInputVariables().getVariables().get(queryIndex);

    GraphSetMaker graphSetMaker = GraphSetMaker.graphSetMaker();
    graphSetMaker.setFunctions(functions);
    // Array out of bounds exception from below
    //GraphPlot graphPlot = 
    		graphSetMaker.plot(assignmentOnNonXAxisVariables, xmVariable);
  }

  private HOGMProblemResult computeResult(
      String model, String query, int initialNumberOfSamples, int numberOfDiscreteValues) {
    HOGMMultiQuerySamplingPropositionalProblemSolver solver =
        new HOGMMultiQuerySamplingPropositionalProblemSolver(
            model, list(query), v -> numberOfDiscreteValues, initialNumberOfSamples, new Random());

    List<? extends HOGMProblemResult> results = solver.getResults();
    assertEquals(1, results.size());
    HOGMProblemResult result = getFirst(results);
    return result;
  }
}
