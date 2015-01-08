package com.sri.ai.praise.rules;

import java.util.LinkedHashSet;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;

/**
 * An exception thrown by the rule converter when a query contains random variables not contained in the given model.
 * 
 * @author oreilly
 */
public class QueryContainsUnknownRandomVariablesException extends Exception {
	private static final long serialVersionUID = 1L;
	//
	private Set<Expression> unknownRandomVariables = new LinkedHashSet<Expression>();
	
	public QueryContainsUnknownRandomVariablesException(Set<Expression> unknownRandomVariables) {
		this.unknownRandomVariables.addAll(unknownRandomVariables);
	}
	
	public Set<Expression> getUnknownRandomVariables() {
		return unknownRandomVariables;
	}
}
