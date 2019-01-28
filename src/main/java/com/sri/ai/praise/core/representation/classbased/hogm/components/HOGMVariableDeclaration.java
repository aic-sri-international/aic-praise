package com.sri.ai.praise.core.representation.classbased.hogm.components;

import java.util.List;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;

public interface HOGMVariableDeclaration {

	/**
	 * 
	 * @return the unique identifying name for the variable.
	 */
	Expression getName();

	/**
	 * 
	 * @return the arity of the number of parameters that the parametric variable declaration takes.
	 */
	Expression getArity();

	/**
	 * 
	 * @return the actual value of the number of parameters that the parametric
	 *         variable declaration takes.
	 */
	int getArityValue();

	/**
	 * 
	 * @return the sorts for the parameters of the variable declaration.
	 */
	List<Expression> getParameterSorts();

	/**
	 * 
	 * @return the sort for the range that the variable can take.
	 */
	Expression getRangeSort();

	/**
	 * 
	 * @return an expression representing the full variable declaration.
	 */
	Expression getVariableDeclaration(String functor);

	/**
	 * 
	 * @return the type representation of the variable declaration.
	 */
	String toTypeRepresentation();

	/**
	 * 
	 * @return a set of IntegerInterval type strings for referenced sorts that are integer intervals.
	 */
	Set<String> getReferencedIntegerIntervalTypes();

	/**
	 * 
	 * @return a set of RealInterval type strings for referenced sorts that are real intervals.
	 */
	Set<String> getReferencedRealIntervalTypes();

	/**
	 * 
	 * @return an expression representing the full variable declaration.
	 */
	Expression getVariableDeclaration();

	/**
	 * The HOGM modifier (random, constant, etc).
	 * @return
	 */
	String getHOGMModifier();
	
	/**
	 * The declaration in HOGM (for example, "random variable: Boolean;")
	 * @return
	 */
	String toHOGMString();
}