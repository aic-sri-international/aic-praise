package com.sri.ai.praise.inference.generic.anytime.anytimegradientexactbp;

import static com.sri.ai.praise.inference.generic.anytime.polytope.core.Polytopes.identityPolytope;
import static com.sri.ai.praise.inference.generic.anytime.polytope.core.Polytopes.sumOut;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.sri.ai.praise.inference.generic.anytime.polytope.api.Polytope;
import com.sri.ai.praise.inference.generic.anytime.polytope.core.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.inference.generic.anytime.polytope.core.Simplex;
import com.sri.ai.praise.inference.generic.exactbp.api.ExactBP;
import com.sri.ai.praise.inference.generic.representation.api.Factor;
import com.sri.ai.praise.inference.generic.representation.api.Variable;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.computation.anytime.api.Anytime;
import com.sri.ai.util.computation.anytime.api.Approximation;
import com.sri.ai.util.computation.treecomputation.anytime.gradientdescent.core.AbstractAnytimeTreeComputationWithGradientDescent;

/**
 * An anytime version of {@link ExactBP} algorithms.
 * This is implemented as a antyime tree computation with gradient-based picking of subs 
 * based on an {@link ExactBP}, which is gradually expanded.
 * <p>
 * It uses {@link Simplex} as an initial approximation,
 * and computes an approximation to the base's answer by not summing out the indices whose sub-messages
 * are simplexes (justification of this is to be found in the related publications).
 * 
 * @author redouane
 *
 */
public class GradientAnytimeExactBP<RootType,SubRootType> extends AbstractAnytimeTreeComputationWithGradientDescent<Factor> {

	@Override
	protected boolean evenOneSubWithTotalIgnoranceRendersApproximationEqualToTotalIgnorance() {
		boolean result = getBase().getRoot() instanceof Variable;
		return result;
	}

	public GradientAnytimeExactBP(ExactBP<RootType,SubRootType> base) {
		super(base, new Simplex(base.getMessageVariable()));
	}

	private Iterator<? extends GradientAnytimeExactBP<SubRootType,RootType>> subIteratorForRefinement;

	@Override
	protected void makeSubsAndIterateThemToTheirFirstApproximation() {
		super.makeSubsAndIterateThemToTheirFirstApproximation();
		subIteratorForRefinement = getSubs().iterator();
	}

	@Override
	protected Anytime<Factor> makeAnytimeVersion(NullaryFunction<Factor> baseSub) {
		@SuppressWarnings("unchecked")
		ExactBP<SubRootType, RootType> baseExactBP = (ExactBP<SubRootType, RootType>) baseSub;
		GradientAnytimeExactBP<SubRootType, RootType> result = new GradientAnytimeExactBP<SubRootType,RootType>(baseExactBP);
		return result;
	}

	@SuppressWarnings("unchecked")
	public ExactBP<RootType,SubRootType> getBase() {
		return (ExactBP<RootType,SubRootType>) super.getBase();
	}
	
	@SuppressWarnings("unchecked")
	public ArrayList<? extends GradientAnytimeExactBP<SubRootType,RootType>> getSubs() {
		return (ArrayList<? extends GradientAnytimeExactBP<SubRootType,RootType>>) super.getSubs();
	}
	
	@Override
	public Approximation<Factor> function(List<Approximation<Factor>> subsApproximations) {
		Polytope product = getProductOfAllIncomingPolytopesAndFactorAtRoot(subsApproximations);
		Collection<? extends Variable> freeVariables = product.getFreeVariables();
		List<? extends Variable> variablesSummedOut = getBase().getSummedOutVariables(freeVariables);
		Approximation<Factor> result = sumOut(variablesSummedOut, product);
		return result;
	}

	private Polytope getProductOfAllIncomingPolytopesAndFactorAtRoot(List<Approximation<Factor>> subsApproximations) {
		List<Polytope> polytopesToMultiply = getAllPolytopes(subsApproximations);
		Polytope result = accumulate(polytopesToMultiply, Polytope::multiply, identityPolytope());
		return result;
	}

	private List<Polytope> getAllPolytopes(List<Approximation<Factor>> subsApproximations) {
		List<Polytope> polytopesToMultiply = mapIntoList(subsApproximations, a -> (Polytope) a);
		addFactorAtRootPolytope(polytopesToMultiply);
		return polytopesToMultiply;
	}

	private void addFactorAtRootPolytope(List<Polytope> polytopesToMultiply) {
		IntensionalConvexHullOfFactors singletonConvexHullOfFactorAtRoot = getFactorAtRootPolytope();
		polytopesToMultiply.add(singletonConvexHullOfFactorAtRoot);
	}

	private IntensionalConvexHullOfFactors getFactorAtRootPolytope() {
		Factor factorAtRoot = Factor.multiply(getBase().getFactorsAtRoot());
		IntensionalConvexHullOfFactors singletonConvexHullOfFactorAtRoot = new IntensionalConvexHullOfFactors(list(), factorAtRoot);
		return singletonConvexHullOfFactorAtRoot;
	}

	@Override
	public String toString() {
		return "(Gradient) Anytime " + getBase();
	}

	@Override
	public Double getAbsolutePartialDerivativeWithRespectTo(Anytime<Factor> sub) {
		Approximation<Factor> subApproximation = sub.getCurrentApproximation();
		Polytope subPolytope = checkIfApproximationIsPolytopeAndReturnPolytopeIfYesThrowErrorOtherwise(subApproximation);
		Collection<? extends Variable> subVariables = subPolytope.getFreeVariables();
		List<List<Object>> allCombinationsOfSubVariableValues = getAllPossibleValues(subVariables);
		
		// TODO finish
		return null;
	}

	private Polytope checkIfApproximationIsPolytopeAndReturnPolytopeIfYesThrowErrorOtherwise(Approximation<Factor> approximation) {
		if(!(approximation instanceof Polytope)) {
			throw new Error("Cannot compute the partial derivatives of other types of"
					+ " approximation other than polytopes ; the type was " + approximation.getClass());
		}
		Polytope polytope = (Polytope) approximation;
		return polytope;
	}
	
	
	private List<List<Object>> getAllPossibleValues(Collection<? extends Variable> subVariables) {
		List<List<Object>> values = new ArrayList<List<Object>>();
		for(Variable variable : subVariables) {
			values = iterateThroughValuesAndCreateNewCombinations(values, variable);
		}
		return values;
	}
	
	private List<List<Object>> iterateThroughValuesAndCreateNewCombinations(List<List<Object>> oldCombinations, Variable newVariable) {
		List<List<Object>> result = new ArrayList<List<Object>>();
		for (Object value : newVariable.getValues()) {
			Iterator<List<Object>> oldCombinationsIterator = oldCombinations.iterator();
			while(oldCombinationsIterator.hasNext()) {
				List<Object> combination = oldCombinationsIterator.next();
				combination.add(value);
				result.add(combination);
			}
		}
		return result;
	}
}