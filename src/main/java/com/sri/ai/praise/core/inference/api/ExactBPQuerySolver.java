package com.sri.ai.praise.core.inference.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.core.treebased.exactbp.api.ExactBPQuery;

public interface ExactBPQuerySolver {

	Expression solve(ExactBPQuery exactBPQuery);

}