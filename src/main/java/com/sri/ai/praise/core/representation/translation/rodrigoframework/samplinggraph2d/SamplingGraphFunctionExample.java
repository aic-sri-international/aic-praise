package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.graph2d.api.functions.Function.function;
import static com.sri.ai.util.graph2d.api.functions.Functions.functions;
import static com.sri.ai.util.graph2d.api.graph.GraphSetMaker.graphSetMaker;
import static com.sri.ai.util.graph2d.api.variables.SetOfVariables.setOfVariables;
import static com.sri.ai.util.graph2d.api.variables.Variable.realVariable;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.graph2d.api.functions.Function;
import com.sri.ai.util.graph2d.api.functions.Functions;
import com.sri.ai.util.graph2d.api.graph.GraphPlot;
import com.sri.ai.util.graph2d.api.graph.GraphSet;
import com.sri.ai.util.graph2d.api.graph.GraphSetMaker;
import com.sri.ai.util.graph2d.api.variables.DefaultSetOfVariables;
import com.sri.ai.util.graph2d.api.variables.Unit;
import com.sri.ai.util.graph2d.api.variables.Variable;

public class SamplingGraphFunctionExample {

	public static void main(String[] args) {
		
		Variable x = realVariable("x", Unit.NONE, "0.0", "0.5", "100.0");
		Variable y = realVariable("y", Unit.NONE, "0.0", "0.5", "100.0");

		DefaultVariable xV = new DefaultVariable("x");
		DefaultVariable yV = new DefaultVariable("y");
		SamplingFactor normalFactor = new NormalWithFixedStandardDeviation(
				yV, 
				xV, 
				10.0, 
				new DoublePotentialFactory(),
				new Random());
		
		int queryIndex = 0;
		int numberOfSamples = 5000;
		Function normal = new SamplingGraphFunction(normalFactor, new DefaultSetOfVariables(list(y,x)), queryIndex, numberOfSamples);
		
		Functions functions = functions(normal);

		GraphSetMaker graphSetMaker = graphSetMaker();

		graphSetMaker.setFunctions(functions);

		GraphSet graphSet = graphSetMaker.make(x);
		
		println(graphSet);

//		// cleanup by removing graph files
//		for (GraphPlot graphPlot : graphSet.getGraphPlots()) {
//			if (graphPlot.getImageFile().delete()) {
//				println("Deleted: " + graphPlot.getImageFile().getName());
//			}
//		}
	}
}