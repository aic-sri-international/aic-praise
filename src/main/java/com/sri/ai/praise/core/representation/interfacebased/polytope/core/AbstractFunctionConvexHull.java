package com.sri.ai.praise.core.representation.interfacebased.polytope.core;

import static com.sri.ai.util.Util.getFirstOrNull;
import static com.sri.ai.util.Util.intersection;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.listFrom;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.setDifference;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.sum;
import static com.sri.ai.util.Util.union;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.AtomicPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.FunctionConvexHull;

/**
 * A polytope equal to the convex hull of points provided by a {@link Factor},
 * of which certain arguments (provided by {@link #getIndices()}) represent the indices of vertices
 * and the remaining ones (provided by {@link #getFreeVariables()}) represent the dimensions of the vectors
 * in the polytope.
 * <p>
 * More precisely, given factor <code>phi(I,V)</code> where <code>I</code> are the indices and <code>V</code>
 * are the remaining variables, the represented polytope is the convex hull of <code>{ phi(I,V) }_I</code>.
 * <p>
 * Multiplying two instances of this class will generate a new instance whose points are
 * the products of pairs in the Cartesian product of the points of the original instances.
 * Therefore, as computation progresses we end up with an exponential number of points in the number of convex hulls
 * used.
 * This is wasteful because many of the points will be internal to the convex full and therefore redundant.
 * One way to deal with that is having internal points removed; however,finding internal points of a convex hull
 * is itself expensive. Besides, even after removal of internal points, the convex hull may have too many vertices
 * and we may instead wish to determine a super-set convex hull with less vertices, thus trading
 * accuracy for efficiency.
 * 
 * @author braz
 *
 */
public abstract class AbstractFunctionConvexHull extends AbstractAtomicPolytope implements FunctionConvexHull {

	private static final boolean debug = false;
	
	/////////////////////// ABSTRACT METHODS
	
	/**
	 * An "overridable" constructor so that extending classes create instances of their own
	 * when when relying on methods written at the level of {@link AbstractFunctionConvexHull}.
	 * @param indices
	 * @param factor
	 * @return
	 */
	@Override
	public abstract AbstractFunctionConvexHull newInstance(Collection<? extends Variable> indices, Factor factor);

	/////////////////////// DATA MEMBERS
	
	private Set<? extends Variable> indices;
	private Factor factor;

	/////////////////////// CONSTRUCTORS
	
	public AbstractFunctionConvexHull(Collection<? extends Variable> indices, Factor factor) {
		Set<Variable> indicesAppearingInFactor = intersection(indices, factor.getVariables());
		this.indices = indicesAppearingInFactor;
		this.factor = factor;
	}

	/////////////////////// BASIC METHODS
	
	@Override
	public Collection<? extends Variable> getIndices() {
		return indices;
	}

	private List<Variable> freeVariables;
	@Override
	public Collection<? extends Variable> getFreeVariables() {
		if (freeVariables == null) {
			List<? extends Variable> all = factor.getVariables();
			freeVariables = subtract(all, getIndices());
		}
		return freeVariables;
	}

	@Override
	public Factor getFactor() {
		return factor;
	}

	@Override
	public boolean isIdentity() {
		boolean result = 
				indices.isEmpty()
				&&
				factor.isIdentity();
		return result;
	}
	
	@Override
	public FunctionConvexHull addIndices(Collection<? extends Variable> newIndices) {
		return newInstance(union(list(newIndices, getIndices())), getFactor());
	}

	/////////////////////// MULTIPLICATION
	
	@Override
	public AtomicPolytope getProductIfItIsASimplificationOrNullOtherwise(AtomicPolytope anotherAtomicPolytope) {
		AtomicPolytope result;
		if (anotherAtomicPolytope instanceof FunctionConvexHull) {
			result = multiplyByFunctionConvexHullIfSameIndicesOrNull(anotherAtomicPolytope);
		}
		else {
			result = null;
		}
		return result;
	}

	private AtomicPolytope multiplyByFunctionConvexHullIfSameIndicesOrNull(AtomicPolytope another) {
		AtomicPolytope result;
		FunctionConvexHull anotherFunctionConvexHull = (FunctionConvexHull) another;
		if (indices.equals(anotherFunctionConvexHull.getIndices())) {
			Factor productFactor = factor.multiply(anotherFunctionConvexHull.getFactor());
			result = newInstance(indices, productFactor);
		}
		else {
			result = null;
		}
		return result;
		
		// We could multiply function convex hulls with *unifiable* indices, that is,
		// indices with the same type but different names.
		// However whether that is worth it seems to be an empirical question.
	}

	/////////////////////////////// SUMMING OUT

	@Override
	public FunctionConvexHull sumOut(Collection<? extends Variable> eliminated) {
		var eliminatedOccurringInPolytope = intersection(eliminated, getFreeVariables());
		if (eliminatedOccurringInPolytope.isEmpty()) {
			return this;
		}
		else {
			var newFactor = getFactor().sumOut(listFrom(eliminatedOccurringInPolytope));
			return newInstance(getIndices(), newFactor);
		}
		// Note that this implementation considers polytopes equivalent modulo normalization.
		// This plays a role here because sum_V Polytope_on_U for V containing variables other than U will result in their cardinality multiplying the result.
		// If we want to represent that, we must rely on the specific polytope implementation used.
	}

	//////////////////////// GET SINGLE ATOMIC POLYTOPE FOR A VARIABLE
	
	@Override
	public AtomicPolytope getEquivalentAtomicPolytopeOn(Variable variable) {
		if (getFreeVariables().contains(variable)) {
			if (getFreeVariables().size() == 1) {
				// already in desired form, nothing to do
				return this;
			}
			else {
				// sum the other ones out
				return sumOut(setDifference(getFreeVariables(), list(variable)));
			}
		}
		else {
			// variable is required to be in polytope
			throw new Error(getClass() + " has variables " + getFreeVariables() + " but getEquivalentAtomicPolytopeOn was requested for one not in it: " + variable);
		}
	}

	//////////////////////// MULTIPLY INTO A SINGLE FUNCTION CONVEX HULL

	/**
	 * Computes the product of a non-empty collection of {@link FunctionConvexHull}s.
	 * This simply delegates to {@link FunctionConvexHull#dynamicMultiplyIntoSingleFunctionConvexHull(Collection)} 
	 * of the first element for convenience. The reason we have a non-static method is so it can be overridden.
	 * @param functionConvexHulls
	 * @return
	 */
	public static FunctionConvexHull multiplyIntoSingleFunctionConvexHull(Collection<? extends FunctionConvexHull> functionConvexHulls) {
		var first = getFirstOrNull(functionConvexHulls);
		if (first == null) {
			throw new Error(AbstractFunctionConvexHull.class + ".staticGetEquivalentAtomicPolytope should not receive empty argument.");
		}
		else {
			return first.dynamicMultiplyIntoSingleFunctionConvexHull(functionConvexHulls);
		}
	}

	/**
	 * Multiplies {@link AbstractFunctionConvexHull} from a non-empty collection of {@link FunctionConvexHull}s.
	 * All other methods should perform such multiplications through this method so that overridden versions take effect.
	 * IMPORTANT: if an index appears in more than one given convex hull, it will appear only once in the result.
	 * This is fine as in end-user contexts indices will be standardized apart.
	 * However, this is actually also relied upon in {@link ProductPolytope}'s summing out method (see comments there).
	 * @param functionConvexHulls
	 * @return
	 */
	@Override
	public FunctionConvexHull dynamicMultiplyIntoSingleFunctionConvexHull(Collection<? extends FunctionConvexHull> functionConvexHulls) {
		LinkedHashSet<? extends Variable> indices = union(functionIterator(functionConvexHulls, FunctionConvexHull::getIndices));
		checkForOverlappingIndices(functionConvexHulls, indices);
		var factors = mapIntoList(functionConvexHulls, FunctionConvexHull::getFactor);
		var factorsProduct = Factor.multiply(factors);
		return newInstance(indices, factorsProduct);
	}

	private void checkForOverlappingIndices(Collection<? extends FunctionConvexHull> functionConvexHulls, LinkedHashSet<? extends Variable> indices) {
		if (debug) {
			int numberOfOriginalIndices = sum(functionIterator(functionConvexHulls, f -> f.getIndices().size())).intValue();
			if (indices.size() != numberOfOriginalIndices) {
				println("\nAbstractFunctionConvexHull: Multiplying convex hulls with overlapping indices: " + join("\n", functionConvexHulls));
				println("indices per hull per line:\n" + join("\n", mapIntoList(functionConvexHulls, FunctionConvexHull::getIndices)));
			}
		}
	}

	////////////////// NORMALIZATION

	@Override
	public FunctionConvexHull normalize(Collection<? extends Variable> variables) {
		return newInstance(getIndices(), getFactor().normalize(variables));
	}

	//////////////////////// ANCILLARY

	@Override
	public String toString() {
		String indicesString = indices.isEmpty()? "" : "(on " + join(indices) + ") ";
		String result = "{" + indicesString + factor + "}";
		return result;
	}

	@Override
	public boolean equals(Object another) {
		boolean result =
				another instanceof FunctionConvexHull
				&&
				((FunctionConvexHull) another).getIndices().equals(getIndices())
				&&
				((FunctionConvexHull) another).getFactor().equals(getFactor());
		return result;
	}

	@Override
	public int hashCode() {
		return getIndices().hashCode() + getFactor().hashCode();
	}

}