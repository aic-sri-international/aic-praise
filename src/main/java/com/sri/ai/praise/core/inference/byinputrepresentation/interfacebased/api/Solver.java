package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Problem;

public interface Solver {

	Expression solve(Problem problem);
	
	void interrupt();

}