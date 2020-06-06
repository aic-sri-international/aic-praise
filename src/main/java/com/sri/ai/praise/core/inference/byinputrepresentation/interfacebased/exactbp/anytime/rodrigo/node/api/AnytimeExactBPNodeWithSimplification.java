package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node.api;

import java.util.ArrayList;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.util.computation.anytime.api.Approximation;

public interface AnytimeExactBPNodeWithSimplification<RootType, SubRootType> extends AnytimeExactBPNode<RootType, SubRootType> {
	
	Approximation<Factor> 
	simplify(Approximation<Factor> approximation);

	Approximation<Factor> 
	computeUpdatedByItselfApproximationGivenThatExternalContextHasChanged(Approximation<Factor> currentApproximation);
	
	@Override
	ArrayList<? extends AnytimeExactBPNodeWithSimplification<SubRootType, RootType>> getSubs();

	@Override
	AnytimeExactBPNodeWithSimplification<SubRootType, RootType> pickNextSubToIterate();
}