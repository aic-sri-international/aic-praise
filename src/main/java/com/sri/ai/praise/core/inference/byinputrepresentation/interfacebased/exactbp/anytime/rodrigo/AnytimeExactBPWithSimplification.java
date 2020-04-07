package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.computation.anytime.api.Approximation;
import com.sri.ai.util.computation.treecomputation.anytime.core.AbstractAnytimeEagerTreeComputationWithSimplification;

@Deprecated // see base class for explanation why
public class AnytimeExactBPWithSimplification<RootType,SubRootType> extends AbstractAnytimeEagerTreeComputationWithSimplification<Factor> {

	public AnytimeExactBPWithSimplification(AnytimeExactBP<RootType,SubRootType> base) {
		super(base);
	}

	public AnytimeExactBPWithSimplification(ExactBPNode<RootType,SubRootType> base) {
		this(new AnytimeExactBP<RootType,SubRootType>(base));
	}

	@SuppressWarnings("unchecked")
	@Override
	public AnytimeExactBP<RootType,SubRootType> getBase() {
		return (AnytimeExactBP<RootType, SubRootType>) super.getBase();
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected AnytimeExactBPWithSimplification<RootType,SubRootType> newInstance(NullaryFunction<Approximation<Factor>> baseSub) {
		return new AnytimeExactBPWithSimplification((AnytimeExactBP) baseSub);
	}

	@Override
	public Approximation<Factor> simplify(Approximation<Factor> approximation) {
		return approximation;
	}

	@Override
	public Approximation<Factor> updateByItself(Approximation<Factor> approximation) {
		
		// We "unsum" the newly free variables by removing them from indices and re-creating their simplices
		// This is based on the fact that indices always result from summing out a simplex variable
		// Also note that summed-out non-simplex variables never become free because they never become externally free.
		// To see this, consider that at the time of their summing out, they were not simplex variables (or they would have become indices).
		// They were also not the root variable, or they would be free and therefore not summed out.
		// If they were neither simplex variables not the root variable, all their factor neighbors were already included in this branch.
		// Therefore their factor neighbors were not in any other branch.
		// Since only their neighbor factors have any information about them and were not in external branches, the variable itself
		// might not appear in an external branch, so it is never made free by expanding them.

		var exactBP = getBase().getBase();
		Polytope polytope = (Polytope) approximation;
		return polytope.unSumOutSimplexVariables(exactBP::isFreeVariable);
	}

}
