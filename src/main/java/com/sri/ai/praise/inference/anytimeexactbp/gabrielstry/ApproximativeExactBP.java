package com.sri.ai.praise.inference.anytimeexactbp.gabrielstry;
import java.util.ArrayList;

import com.sri.ai.util.computation.treecomputation.api.TreeComputation;
/**
 * 
 * @author gabriel
 *
 * @param <RootType>
 * @param <SubRootType>
 */
//The best option would be BoundedExactBP<RootType,SubRootType> extends ExactBP<RootType,SubRootType> provided that "Factor" is actually a FactorApproximation
public interface ApproximativeExactBP<RootType,SubRootType> extends TreeComputation<FactorApproximaton>{
	
	SubRootType getParent();

	RootType getRoot();
	
	@Override
	ArrayList<ApproximativeExactBP<SubRootType,RootType>> getSubs();
}
