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
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ConstantFactor;
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

	@SuppressWarnings("unused")
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

	@Override
	@SuppressWarnings("unchecked")
	public ExactBPNode<RootType,SubRootType> getBase() {
		return (ExactBPNode<RootType,SubRootType>) super.getBase();
	}
	
	@Override
	public ArrayList<? extends Anytime<Factor>> getSubs() {
		return super.getSubs();
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
		if(sub.equals(null)) {
			throw new Error("Sub does not exist, can't compute volume variation");
		}
		Approximation<Factor> subApproximation = sub.getCurrentApproximation();
		AtomicPolytope subAtomicPolytope = transformApproximationToAtomicPolytopeOrThrowsErrorIfNotPossible(subApproximation);
		Set<? extends Variable> subAtomicPolytopeIndices = getIndices(subAtomicPolytope);
		if(evenOneSubWithTotalIgnoranceRendersApproximationEqualToTotalIgnorance()) {
			// root is a variable
			return getAbsoluteVolumeVariationFromFactorToVariableWithRespectTo(sub, subAtomicPolytopeIndices);
		} 
		return getAbsoluteVolumeVariationFromVariableToFactorWithRespectTo(sub, subAtomicPolytopeIndices);
	}
	
	private static AtomicPolytope transformApproximationToAtomicPolytopeOrThrowsErrorIfNotPossible(Approximation<Factor> approximation) {
		if(!(approximation instanceof Polytope)) {
			throw new Error("Gradient descent AEBP works only with polytopes for now");
		}
		Polytope polytope = (Polytope) approximation;
		AtomicPolytope atomicPolytope = collapse(polytope);
		return atomicPolytope;
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
	
	// implementation based on logVolume

	private Double getAbsoluteVolumeVariationFromVariableToFactorWithRespectTo(Anytime<Factor> sub, Collection<? extends Variable> subAtomicPolytopeIndices) {
		
		// root is a factor
		
		Approximation<Factor> subApproximation = sub.getCurrentApproximation();
		transformApproximationToAtomicPolytopeOrThrowsErrorIfNotPossible(subApproximation);
		
		Approximation<Factor> rootApproximation = getCurrentApproximation(); 
		IntensionalConvexHullOfFactors rootConvexHull = transformApproximationToConvexHullOrThrowsErrorIfNotPossible(rootApproximation);
		Collection<? extends Variable> rootIndices = rootConvexHull.getIndices();
		Factor rootFactor = rootConvexHull.getFactor();
		
		createNormalizedMaxFactor(rootIndices);
		createNormalizedMinFactor(rootIndices);
		Factor invertedMaxMinusMinFactor = computeInvertedMaxMinusMinFactor(rootFactor, subAtomicPolytopeIndices);
		
		Factor normalizedMaxFactorWithoutOneSub = createNormalizedMaxFactorWithoutOneSub(sub, rootIndices);
		Factor normalizedMinFactorWithoutOneSub = createNormalizedMinFactorWithoutOneSub(sub, rootIndices);
		
		Factor minusNormalizedMinFactorWithoutOneSub = normalizedMinFactorWithoutOneSub.multiply(new ConstantFactor(1.));
		Factor MaxMinusMinFactorWithoutOneSub = normalizedMaxFactorWithoutOneSub.add(minusNormalizedMinFactorWithoutOneSub);
		
		Factor firstTermProduct = invertedMaxMinusMinFactor.multiply(MaxMinusMinFactorWithoutOneSub);
		firstTermProduct.sumOut(new ArrayList<>(rootConvexHull.getFreeVariables()));
		
		// TODO second term
		
		return null;
		
		
	}

	private Double getAbsoluteVolumeVariationFromFactorToVariableWithRespectTo(Anytime<Factor> sub, Collection<? extends Variable> subAtomicPolytopeIndices) {
		
		// root is a variable
		
		Approximation<Factor> subApproximation = sub.getCurrentApproximation();
		transformApproximationToAtomicPolytopeOrThrowsErrorIfNotPossible(subApproximation);
		
		Approximation<Factor> rootApproximation = getCurrentApproximation(); 
		IntensionalConvexHullOfFactors rootConvexHull = transformApproximationToConvexHullOrThrowsErrorIfNotPossible(rootApproximation);
		Collection<? extends Variable> rootIndices = rootConvexHull.getIndices();
		Factor rootFactor = rootConvexHull.getFactor();
		
		createNormalizedMaxFactor(rootIndices);
		createNormalizedMinFactor(rootIndices);
		Factor invertedMaxMinusMinFactor = computeInvertedMaxMinusMinFactor(rootFactor, subAtomicPolytopeIndices);
		
		List<Variable> rootFreeVariables = new ArrayList<>(rootConvexHull.getFreeVariables());
		
		Factor maxFactor = rootFactor.max(rootIndices);
		Factor maxNormalizationConstant = maxFactor.sumOut(rootFreeVariables);
		Factor invertedMaxNormalizationConstant = maxNormalizationConstant.invert();
		invertedMaxMinusMinFactor.multiply(invertedMaxNormalizationConstant);
		
		Factor minFactor = rootFactor.min(rootIndices);
		Factor minNormalizationConstant = minFactor.sumOut(rootFreeVariables);
		Factor invertedMinNormalizationConstant = minNormalizationConstant.invert();
		invertedMaxMinusMinFactor.multiply(invertedMinNormalizationConstant);
		
		Polytope  summedOutPolytopeWithoutOneSub = sumOutWithoutOneSub(sub);
		IntensionalConvexHullOfFactors rootConvexHullWithoutOneSub = (IntensionalConvexHullOfFactors) summedOutPolytopeWithoutOneSub;
		Factor rootFactorWithoutOneSub = rootConvexHullWithoutOneSub.getFactor();
		
		rootFactorWithoutOneSub.sumOut(rootFreeVariables);
		
		return null;
		// TODO
	}

	private static IntensionalConvexHullOfFactors transformApproximationToConvexHullOrThrowsErrorIfNotPossible(Approximation<Factor> approximation) {
		AtomicPolytope atomicPolytope = transformApproximationToAtomicPolytopeOrThrowsErrorIfNotPossible(approximation);
		if(!(approximation instanceof IntensionalConvexHullOfFactors)) { // can't be simplex because sub is not null
			throw new Error("Unfit type of approximation for gradient descent");
		}
		IntensionalConvexHullOfFactors convexHull = (IntensionalConvexHullOfFactors) atomicPolytope;
		return convexHull;
	}
	
	private static Factor computeInvertedMaxMinusMinFactor(Factor factor, Collection<? extends Variable> variablesToMaximizeAndMinimizeOver) {
		Factor maxFactor = factor.max(variablesToMaximizeAndMinimizeOver);
		Factor minFactor = factor.min(variablesToMaximizeAndMinimizeOver);
		Factor normalizedMaxFactor = maxFactor.normalize();
		Factor normalizedMinFactor = minFactor.normalize();
		Factor minusMinFactor = normalizedMinFactor.multiply(new ConstantFactor(-1.));
		Factor maxMinusMinFactor = normalizedMaxFactor.add(minusMinFactor);
		Factor invertMaxMinusMinFactor = maxMinusMinFactor.invert();
		return invertMaxMinusMinFactor;
	}
	
	private Polytope productWithoutOneSub(Anytime<Factor> sub) {
		List<Approximation<Factor>> subsApproximations = getSubsApproximation();
		Polytope product = getProductOfAllIncomingPolytopesButOne(sub, subsApproximations);
		return product;
	}
	
	private List<Approximation<Factor>> getSubsApproximation() {
		List<Approximation<Factor>> result = new ArrayList<Approximation<Factor>>();
		for(Anytime<Factor> sub : getSubs()) {
			result.add(sub.getCurrentApproximation());
		}
		return result;
	}
	
	private Polytope getProductOfAllIncomingPolytopesButOne(Anytime<Factor> sub, List<Approximation<Factor>> subsApproximations) {
		Polytope subApproximationToRemove = (Polytope) sub.getCurrentApproximation();
		List<Polytope> polytopesToMultiply = getAllPolytopesButOne(subApproximationToRemove, subsApproximations);
		Polytope result = accumulate(polytopesToMultiply, Polytope::multiply, identityPolytope());
		return result;
	}

	private List<Polytope> getAllPolytopesButOne(Polytope subApproximationToRemove, List<Approximation<Factor>> subsApproximations) {
		List<Polytope> polytopesToMultiply = mapIntoList(subsApproximations, a -> (Polytope) a);
		for(Polytope subPolytope : polytopesToMultiply) {
			if(subPolytope.equals(subApproximationToRemove)) {
				polytopesToMultiply.remove(subPolytope);
			}
		}
		return polytopesToMultiply;
	}
	
	private Factor createNormalizedMaxFactor(Collection<? extends Variable> rootIndices) {
		Approximation<Factor> rootApproximation = getCurrentApproximation(); 
		IntensionalConvexHullOfFactors rootConvexHull = transformApproximationToConvexHullOrThrowsErrorIfNotPossible(rootApproximation);
		Factor rootFactor = rootConvexHull.getFactor();
		Factor maxFactor = rootFactor.max(rootIndices);
		Factor normalizedMaxFactor = maxFactor.normalize();
		return normalizedMaxFactor;
	}
	
	private Factor createNormalizedMinFactor(Collection<? extends Variable> rootIndices) {
		Approximation<Factor> rootApproximation = getCurrentApproximation(); 
		IntensionalConvexHullOfFactors rootConvexHull = transformApproximationToConvexHullOrThrowsErrorIfNotPossible(rootApproximation);
		Factor rootFactor = rootConvexHull.getFactor();
		Factor minFactor = rootFactor.min(rootIndices);
		Factor normalizedMinFactor = minFactor.normalize();
		return normalizedMinFactor;
	}
	
	private Factor createNormalizedMaxFactorWithoutOneSub(Anytime<Factor> sub, Collection<? extends Variable> rootIndices) {
		Polytope rootPolytopeWithoutOneSub = productWithoutOneSub(sub);
		IntensionalConvexHullOfFactors rootConvexHullWithoutOneSub = (IntensionalConvexHullOfFactors) rootPolytopeWithoutOneSub;
		Factor rootFactorWithoutOneSub = rootConvexHullWithoutOneSub.getFactor();
		Factor maxRootFactorWithoutOneSub = rootFactorWithoutOneSub.max(rootIndices);
		Factor normalizedMaxFactorWithoutOneSub = maxRootFactorWithoutOneSub.normalize();
		return normalizedMaxFactorWithoutOneSub;
	}
	
	private Factor createNormalizedMinFactorWithoutOneSub(Anytime<Factor> sub, Collection<? extends Variable> rootIndices) {
		Polytope rootPolytopeWithoutOneSub = productWithoutOneSub(sub);
		IntensionalConvexHullOfFactors rootConvexHullWithoutOneSub = (IntensionalConvexHullOfFactors) rootPolytopeWithoutOneSub;
		Factor rootFactorWithoutOneSub = rootConvexHullWithoutOneSub.getFactor();
		Factor minRootFactorWithoutOneSub = rootFactorWithoutOneSub.min(rootIndices);
		Factor normalizedMinFactorWithoutOneSub = minRootFactorWithoutOneSub.normalize();
		return normalizedMinFactorWithoutOneSub;
	}
	
	private Polytope sumOutWithoutOneSub(Anytime<Factor> sub) {
		List<Approximation<Factor>> subsApproximations = getSubsApproximation();
		Polytope product = getProductOfAllIncomingPolytopesButOneAndFactorAtRoot(sub, subsApproximations);
		Collection<? extends Variable> freeVariables = product.getFreeVariables();
		List<? extends Variable> variablesSummedOut = getBase().determinedVariablesToBeSummedOut(freeVariables);
		Polytope result = sumOut(variablesSummedOut, product);
		return result;
	}

	private Polytope getProductOfAllIncomingPolytopesButOneAndFactorAtRoot(Anytime<Factor> sub, List<Approximation<Factor>> subsApproximations) {
		Polytope subApproximationToRemove = (Polytope) sub.getCurrentApproximation();
		List<Polytope> polytopesToMultiply = getAllPolytopesButOneWithFactor(subApproximationToRemove, subsApproximations);
		Polytope result = accumulate(polytopesToMultiply, Polytope::multiply, identityPolytope());
		return result;
	}

	private List<Polytope> getAllPolytopesButOneWithFactor(Polytope subApproximationToRemove, List<Approximation<Factor>> subsApproximations) {
		List<Polytope> polytopesToMultiply = mapIntoList(subsApproximations, a -> (Polytope) a);
		for(Polytope subPolytope : polytopesToMultiply) {
			if(subPolytope.equals(subApproximationToRemove)) {
				polytopesToMultiply.remove(subPolytope);
			}
		}
		addFactorAtRootPolytope(polytopesToMultiply);
		return polytopesToMultiply;
	}

	
}
