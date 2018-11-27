package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRules.union;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.planning.core.Planner.plan;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRules;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.VariableGoal;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.collect.PermutationIterator;
import com.sri.ai.util.planning.api.Plan;

public class SamplingProductFactor extends AbstractCompoundSamplingFactor {
	
	private Plan samplingPlan;

	public SamplingProductFactor(ArrayList<? extends SamplingFactor> multipliedFactors, Random random) {
		super(flattenSamplingProductFactors(multipliedFactors), random);
		// it is important that product factors be flat because we must be sure that all incoming messages of a variable in Exact BP
		// are present in the same product factor if we are to limit the search for samplers to a single product factor.
		this.samplingPlan = makePlan();
	}

	private static ArrayList<? extends SamplingFactor> flattenSamplingProductFactors(ArrayList<? extends SamplingFactor> multipliedFactors) {
		ArrayList<SamplingFactor> flattenedList = arrayList();
		for (SamplingFactor factor : multipliedFactors) {
			if (factor instanceof SamplingProductFactor) {
				SamplingProductFactor productFactor = (SamplingProductFactor) factor;
				// uses the fact that pre-existing product factors are already flattened.
				flattenedList.addAll(productFactor.getInputFactors());
			}
			else {
				flattenedList.add(factor);
			}
		}
		return flattenedList;
	}

	@Override
	protected SamplingRules makeSamplingRules() {
		SamplingRules union = union(mapIntoList(getInputFactors(), SamplingFactor::getSamplingRules));
		SamplingRules samplingRules = union.replaceFactor(this);
		return samplingRules;
	}

	private Plan makePlan() {
		List<? extends VariableGoal> variableGoals = mapIntoList(getVariables(), v -> new VariableGoal(v));
		Plan plan  =plan(variableGoals, getSamplingRules().getSamplingRules());
		return plan;
	}

	@Override
	public void sampleOrWeigh(Sample sampleToComplete) {
		// ATTENTION: there is bit of complicated footwork going on regarding sampleToComplete as a pointer in this and the next method
		// The next method creates a copy and assigns it to 'sampleToComplete' in THERE, but it does not affect the one HERE.
		// So we need to return the result (or null if it failed) and copy all the information back in the original object here.
		Sample completeSampleOrNull = attemptToCompleteUsingAsManyOrderPermutationsAsNeeded(sampleToComplete);
		myAssert(completeSampleOrNull != null, samplingFailureMessage(sampleToComplete));
		sampleToComplete.copy(completeSampleOrNull);
	}

	private Sample attemptToCompleteUsingAsManyOrderPermutationsAsNeeded(Sample sampleToComplete) {
		
		// TODO: this is a temporary placeholder but we need a smarter sampling schedule
		
		boolean successful = false;
		Iterator<ArrayList<? extends SamplingFactor>> permutationIterator = makePermutationIterator();
		
		println("\n=====================");
		println("Starting to try all permutations");
		println("Original sample: " + sampleToComplete);
		println("Factors:");
		println(join("\n", getInputFactors()));
		
		for (ArrayList<? extends SamplingFactor> permutation : in(permutationIterator)) {

			println("------------------");
			println("Trying permutation:\n" + join("\n", permutation));
			println("Sample: " + sampleToComplete);

			Sample originalSampleToComplete = sampleToComplete.copy();
			successful = attemptToCompleteForGivenPermutation(sampleToComplete, permutation);
			
			if (successful) {
				println("------------------");
				println("Permutation successful");
				println("Permutation:\n" + join("\n", permutation));
				println("sampleToComplete: " + sampleToComplete);
				println("originalSampleToComplete: " + originalSampleToComplete);
				break;
			}
			else {
				println("------------------");
				println("Permutation failed");
				println("Permutation:\n" + join("\n", permutation));
				println("Obtained partial sample: " + sampleToComplete);
				println("Original sample: " + originalSampleToComplete);
				sampleToComplete = originalSampleToComplete;
			}
			
		}
		
		if (successful) {
			println("\nUsing all permutations worked");
			println("Obtained sample: " + sampleToComplete);
			println("Factors:");
			println(join("\n", getInputFactors()));
			println("\n=====================");
		}
		else {
			println("\nAll permutations failed");
			println("Original sample: " + sampleToComplete);
			println("Factors:");
			println(join("\n", getInputFactors()));
			println("\n=====================");
			sampleToComplete = null;
		}
		
		return sampleToComplete;
	}

	private boolean attemptToCompleteForGivenPermutation(Sample sampleToComplete, ArrayList<? extends SamplingFactor> multipledFactorsInACertainOrder) {
		
		for (SamplingFactor factor: multipledFactorsInACertainOrder) {
			factor.sampleOrWeigh(sampleToComplete);
		}
		boolean successful = complete(sampleToComplete);
		return successful;
	}

	private boolean complete(Sample sampleToComplete) {
		// TODO: this can be made better than checking every variable at every iteration. We can instead update it according to factor used
		boolean result = forAll(getVariables(), v -> sampleToComplete.getAssignment().contains(v));
		return result;
	}

	@SuppressWarnings("unchecked")
	private Iterator<ArrayList<? extends SamplingFactor>> makePermutationIterator() {
		ArrayList<SamplingFactor> inputFactors = (ArrayList<SamplingFactor>) getInputFactors(); // due to PermutationIterator not accepting "? extends" 
		Iterator<ArrayList<? extends SamplingFactor>> permutationIterator = new PermutationIterator(inputFactors);
		return permutationIterator;
	}

	@Override
	public String operatorName() {
		return "product";
	}

	private NullaryFunction<String> samplingFailureMessage(Sample sampleToComplete) {
		return () -> "No permutation of factors in sampling product factor produced a complete sample.\nSample: " + sampleToComplete + "\nProduct factor: " + this;
	}

	@Override
	public String toString() {
		return "product(" + join(getInputFactors()) + ")";
	}
}
