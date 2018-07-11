package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.redouane;

import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.Polytopes.identityPolytope;
import static com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.Polytopes.sumOut;
import static com.sri.ai.util.Util.accumulate;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.Polytopes;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.Simplex;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.computation.anytime.api.Anytime;
import com.sri.ai.util.computation.anytime.api.Approximation;
import com.sri.ai.util.computation.treecomputation.anytime.gradientdescent.core.AbstractAnytimeTreeComputationWithGradientDescent;

/**
 * An anytime version of {@link ExactBP} algorithms.
 * This is implemented as a anytime tree computation with gradient-based picking of subs 
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

	public GradientAnytimeExactBP(ExactBPNode<RootType,SubRootType> base) {
		super(base, new Simplex(base.getMessageVariable()));
	}

	private Iterator<? extends Anytime<Factor>> subIteratorForRefinement;

	@Override
	protected void makeSubsAndIterateThemToTheirFirstApproximation() {
		super.makeSubsAndIterateThemToTheirFirstApproximation();
		subIteratorForRefinement = getSubs().iterator();
	}

	@Override
	protected Anytime<Factor> makeAnytimeVersion(NullaryFunction<Factor> baseSub) {
		@SuppressWarnings("unchecked")
		ExactBPNode<SubRootType, RootType> baseExactBP = (ExactBPNode<SubRootType, RootType>) baseSub;
		GradientAnytimeExactBP<SubRootType, RootType> result = new GradientAnytimeExactBP<SubRootType,RootType>(baseExactBP);
		return result;
	}

	@SuppressWarnings("unchecked")
	public ExactBPNode<RootType,SubRootType> getBase() {
		return (ExactBPNode<RootType,SubRootType>) super.getBase();
	}
	
	@SuppressWarnings("unchecked")
	public ArrayList<? extends Anytime<Factor>> getSubs() {
		return (ArrayList<? extends GradientAnytimeExactBP<SubRootType,RootType>>) super.getSubs();
	}
	
	@Override
	public Approximation<Factor> function(List<Approximation<Factor>> subsApproximations) {
		Polytope product = getProductOfAllIncomingPolytopesAndFactorAtRoot(subsApproximations);
		Collection<? extends Variable> freeVariables = product.getFreeVariables();
		List<? extends Variable> variablesSummedOut = getBase().determinedVariablesToBeSummedOut(freeVariables);
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
	public Double getAbsoluteVolumeVariationWithRespectTo(Anytime<Factor> sub) {
		Approximation<Factor> subApproximation = sub.getCurrentApproximation();
		if(!(subApproximation instanceof Polytope)) {
			throw new Error("Gradient descent AEBP works only with polytopes for now");
		}
		Polytope subPolytope = (Polytope) subApproximation;
		AtomicPolytope subAtomicPolytope = collapse(subPolytope);
		Set<? extends Variable> subAtomicPolytopeIndices = getIndices(subAtomicPolytope);
		if(rootIsVariable()) {
			return getAbsoluteVolumeVariationFromFactorToVariableWithRespectTo(subAtomicPolytope, subAtomicPolytopeIndices);
		} 
		return getAbsoluteVolumeVariationFromVariableToFactorWithRespectTo(subAtomicPolytope, subAtomicPolytopeIndices);
	}
	
	private static AtomicPolytope collapse(Polytope subPolytope) {
		Variable freeVariable = getFreeVariable(subPolytope);
		AtomicPolytope subAtomicPolytope = Polytopes.getEquivalentAtomicPolytopeOn(freeVariable, subPolytope);
		return subAtomicPolytope;
	}
	
	private static Variable getFreeVariable(Polytope subPolytope) {
		Collection<? extends Variable> freeVariables = subPolytope.getFreeVariables();
		if(freeVariables.size() != 1) {
			throw new Error("BP messages should have one and only one free variable");
		}
		Variable freeVariable = null;
		for(Variable variable : freeVariables) {
			freeVariable = variable;
		}
		return freeVariable;
	}
	
	private static Set<? extends Variable> getIndices(AtomicPolytope subAtomicPolytope) {
		Set<Variable> result = new HashSet<>();
		if(subAtomicPolytope instanceof Simplex) {
			Simplex subSimplex = (Simplex) subAtomicPolytope;
			result.add(subSimplex.getVariable());
		} else if(subAtomicPolytope instanceof IntensionalConvexHullOfFactors) {
			IntensionalConvexHullOfFactors subConvexHull = (IntensionalConvexHullOfFactors) subAtomicPolytope;
			result.addAll(subConvexHull.getIndices());
		} else {
			throw new Error("New unsupported type of atomic polytope added");
		}
		return result;
	}

	private Double getAbsoluteVolumeVariationFromVariableToFactorWithRespectTo(AtomicPolytope subPolytope, Collection<? extends Variable> subAtomicPolytopeIndices) {
		// TODO Auto-generated method stub
		return null;
	}

	private Double getAbsoluteVolumeVariationFromFactorToVariableWithRespectTo(AtomicPolytope subPolytope, Collection<? extends Variable> subAtomicPolytopeIndices) {
		// TODO Auto-generated method stub
		return null;
	}

	private boolean rootIsVariable() {
		// TODO Auto-generated method stub
		return false;
	}

	private Polytope checkIfApproximationIsPolytopeAndReturnPolytopeIfYesThrowErrorOtherwise(Approximation<Factor> approximation) {
		if(!(approximation instanceof Polytope)) {
			throw new Error("Cannot compute the partial derivatives of other types of"
					+ " approximation other than polytopes ; the type was " + approximation.getClass());
		}
		Polytope polytope = (Polytope) approximation;
		return polytope;
	}
	
	private List<Approximation<Factor>> getSubsCurrentApproximations() {
		List<Approximation<Factor>> result = new ArrayList<Approximation<Factor>>(); 
		for(Anytime<Factor> sub : getSubs()) {
			Approximation<Factor> subLastApproximation = sub.getCurrentApproximation();
			result.add(subLastApproximation);
		}
		return result;
	}
	
	private List<Approximation<Factor>> getAllSubsButOneCurrentApproximations(Anytime<Factor> subToRemove) {
		List<Approximation<Factor>> result = new ArrayList<Approximation<Factor>>(); 
		for(Anytime<Factor> sub : getSubs()) {
			if(!sub.equals(subToRemove)) {
				Approximation<Factor> subLastApproximation = sub.getCurrentApproximation();
				result.add(subLastApproximation);
			}
		}
		return result;
	}
	
	private List<? extends Variable> getVariablesNotSummedOut(List<Approximation<Factor>> subsApproximations) {
		Polytope product = getProductOfAllIncomingPolytopesAndFactorAtRoot(subsApproximations);
		Collection<? extends Variable> freeVariables = product.getFreeVariables();
		//List<? extends Variable> variablesSummedOut = getBase().getSummedOutVariables(freeVariables);
		//return variablesSummedOut;
		return null;
	}

	
}
