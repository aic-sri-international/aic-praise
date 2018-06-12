package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.representation.api;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
/**
 * This interface adds to a FactorNetwork the functionalities 
 * I think we need for a Factor Graph.
 * @author gabriel
 *
 */
public interface EditableFactorNetwork extends FactorNetwork {
	
	EditableFactorNetwork makeEmptyNetwork();
	
	boolean containsFactor(Factor f);
	
	void add(Factor factor, Variable variable);
	
}
