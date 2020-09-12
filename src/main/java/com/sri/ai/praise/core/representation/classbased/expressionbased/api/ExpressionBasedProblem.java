package com.sri.ai.praise.core.representation.classbased.expressionbased.api;

import java.util.List;
import java.util.function.Predicate;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.praise.other.integration.proceduralattachment.api.ProceduralAttachments;

/**
 * Represents a (always marginalization, for now) problem based on an {@link ExpressionBasedModel}.
 *
 * Currently, a problem is based on a given {@link ExpressionBasedProblem} and a query expression.
 * If the query is a compound expression (as opposed to a variable symbol), a fresh query symbol is introduced
 * and a factor defining this new variable in terms of the compound expression query is introduced.
 *
 * @author braz
 *
 */
public interface ExpressionBasedProblem {

	/** 
	 * Replaces variable query ("query") by original query in given expression, 
	 * and simplifies it with original model's context.
	 */
	Expression replaceQuerySymbolByQueryExpressionIfNeeded(Expression expression);

	/** The original model. */
	ExpressionBasedModel getOriginalExpressionBasedModel();

	/** The model after the possible introduction of a fresh new query variable symbol. */
	ExpressionBasedModel getExpressionBasedModel();

	/** The original query expression. */
	Expression getQueryExpression();

	/** Whether the original query expression was compound, meaning that a new query variable symbol was introduced. */
	boolean getQueryIsCompound();

	/** The quert variable symbol (equal to original query if it was already a symbol). */
	Expression getQuerySymbol();

	List<Expression> getFactorExpressionsIncludingQueryDefinitionIfAny();

	List<Expression> getRandomVariablesExcludingQuerySymbol();

	List<Expression> getParameters();

	boolean modelIsKnownToBeBayesianNetwork();

	Context getContext();

	Predicate<Expression> getIsParameterPredicate();
	
	ProceduralAttachments getProceduralAttachments();
	
	void setProceduralAttachments(ProceduralAttachments proceduralAttachments);

}