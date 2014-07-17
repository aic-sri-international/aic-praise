package com.sri.ai.praise;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.model.Model;

/**
 * An object implementing a probabilistic inference solver.
 *
 * @author braz
 */
public interface Solver {

	/**
	 * Returns an expression encoding the marginal probability,
	 * or an approximation of it, given a query and a model
	 * (which must include evidence if present).
	 */
	public Expression marginal(Expression query, Model model);
}
