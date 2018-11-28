package com.sri.ai.praise.core.representation.translation.rodrigoframework.samplinggraph2d;

import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.getIndexOf;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.graph2d.api.functions.Functions.functions;
import static com.sri.ai.util.graph2d.api.graph.GraphSetMaker.graphSetMaker;
import static com.sri.ai.util.graph2d.api.variables.Variable.realVariable;

import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedMeanAndStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.distribution.NormalWithFixedStandardDeviation;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.SamplingProductFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DoublePotentialFactory;
import com.sri.ai.util.graph2d.api.functions.Functions;
import com.sri.ai.util.graph2d.api.graph.GraphSet;
import com.sri.ai.util.graph2d.api.graph.GraphSetMaker;
import com.sri.ai.util.graph2d.api.variables.DefaultSetOfVariables;
import com.sri.ai.util.graph2d.api.variables.Unit;
import com.sri.ai.util.graph2d.api.variables.Variable;

public class SamplingGraphFunctionExample {

	public static void main(String[] args) {

		boolean forDavid = true;
		
		Random random = new Random();
		int numberOfSamples = 500000;

		double mean = 50.0;
		double standardDeviation = 5.0;
		int numberOfPoints = 500;

		double axisStart = mean - standardDeviation*3;
		double axisEnd   = mean + standardDeviation*3;
		double step = (axisEnd - axisStart)/(numberOfPoints - 1);
		Variable x = realVariable("x", Unit.NONE, axisStart + "", step + "", axisEnd + "");
		Variable y = realVariable("y", Unit.NONE, axisStart + "", step + "", axisEnd + "");

		DefaultVariable xV = new DefaultVariable("x");
		DefaultVariable yV = new DefaultVariable("y");
		SamplingFactor xFromY = new NormalWithFixedStandardDeviation(
				xV,
				yV,
				standardDeviation, 
				new DoublePotentialFactory(),
				random);
		SamplingFactor yPrior = new NormalWithFixedMeanAndStandardDeviation(
				yV,
				mean,
				0.000001, 
				new DoublePotentialFactory(),
				random);
		
		
		
		SamplingFactor factorToBeShown;
		int queryIndex;
		SamplingGraphFunction function;
		
		if (forDavid) {
			factorToBeShown = new NormalWithFixedMeanAndStandardDeviation(
					xV,
					50.0,
					standardDeviation, 
					new DoublePotentialFactory(),
					random);
			queryIndex = getIndexOf(factorToBeShown.getVariables(), xV);
			function = new SamplingGraphFunction(factorToBeShown, new DefaultSetOfVariables(list(x)), queryIndex);
		}
		else {
			factorToBeShown = new SamplingProductFactor(arrayList(xFromY, yPrior), random);
			queryIndex = getIndexOf(factorToBeShown.getVariables(), xV);
			function = new SamplingGraphFunction(factorToBeShown, new DefaultSetOfVariables(list(x, y)), queryIndex);
		}
		
		Functions functions = functions(function);
		
		for (int i = 0; i < numberOfSamples; i++) {
			function.iterate();
		}

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