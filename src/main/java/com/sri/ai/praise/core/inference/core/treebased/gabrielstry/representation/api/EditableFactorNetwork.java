package com.sri.ai.praise.core.inference.core.treebased.gabrielstry.representation.api;

import com.sri.ai.praise.core.model.core.treebased.api.Factor;
import com.sri.ai.praise.core.model.core.treebased.api.FactorNetwork;
import com.sri.ai.praise.core.model.core.treebased.api.Variable;
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
