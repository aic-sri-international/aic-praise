package com.sri.ai.praise.inference.anytimeexactbp.gabrielstry;
import com.sri.ai.util.computation.treecomputation.api.TreeComputation;
/**
 * 
 * @author gabriel
 *
 * @param <RootType>
 * @param <SubRootType>
 */
//The best option would be BoundedExactBP<RootType,SubRootType> extendds ExactBP<RootType,SubRootType> provided that "Factor" is actually a appri
public interface BoundedExactBP<RootType,SubRootType> extends TreeComputation<FactorApproximaton>{
	
}
