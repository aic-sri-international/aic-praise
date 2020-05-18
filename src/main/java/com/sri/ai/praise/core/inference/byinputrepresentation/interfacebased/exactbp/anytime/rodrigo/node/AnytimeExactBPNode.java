package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.util.computation.anytime.api.Approximation;
import com.sri.ai.util.computation.treecomputation.anytime.api.AnytimeEagerTreeComputation;

public interface AnytimeExactBPNode<RootType, SubRootType> extends AnytimeEagerTreeComputation<Factor> {

	@Override
	boolean evenOneSubWithTotalIgnoranceRendersApproximationEqualToTotalIgnorance();

	@Override
	AnytimeExactBPNode<SubRootType, RootType> pickNextSubToIterate();

	ExactBPNode<RootType, SubRootType> getBase();

	@Override
	ArrayList<? extends AbstractAnytimeExactBPNodeWithSimplificationMechanism<SubRootType, RootType>> getSubs();

	@Override
	Approximation<Factor> function(List<Approximation<Factor>> subsApproximations);

	@Override
	void setCurrentApproximation(Approximation<Factor> newCurrentApproximation);

}