package com.sri.ai.praise.inference.anytimeexactbp.gabrielstry;

import java.util.Set;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.util.computation.anytime.api.Anytime;

/**
 * 
 * @author gabriel
 *
 */

public abstract class AnytimeExactBP implements Anytime<FactorApproximaton>{
	
	private ApproximativeExactBP<Variable, FactorApproximaton> partialTree;
	private Set<Factor> wholeSetOfFactors;
	private FactorApproximaton currentApproximation;
	
	public void ExpandTree() {
		//TODO
	}
	
	
}
