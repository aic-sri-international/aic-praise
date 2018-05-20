package com.sri.ai.praise.inference.gabrielstry.representation.api;

import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.FactorNetwork;
import com.sri.ai.praise.inference.representation.api.Variable;
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
