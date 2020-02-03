package com.sri.ai.praise.core.representation.interfacebased.factor.api;

import java.util.Collection;

/**
 * This interface adds to a FactorNetwork the functionalities 
 * I think we need for a Factor Graph.
 * @author gabriel
 *
 */
public interface EditableFactorNetwork extends FactorNetwork {
	
	EditableFactorNetwork makeEmptyNetwork();
	
	void add(Factor factor);
	
	void remove(Factor factor);
	
	void removeAll(Collection<? extends Factor> factors);
	
	boolean containsFactor(Factor f);
	
	/**
	 * This method probably needs to be remove as it is too low-level;
	 * it makes more sense to add a factor.
	 * @param factor
	 * @param variable
	 */
	void add(Factor factor, Variable variable);
	
}
