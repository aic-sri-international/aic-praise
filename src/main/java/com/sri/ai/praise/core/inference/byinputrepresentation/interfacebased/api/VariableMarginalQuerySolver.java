package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.VariableMarginalQuery;

public interface VariableMarginalQuerySolver {

	Expression solve(VariableMarginalQuery exactBPQuery);

}