package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.algorithm;

import static com.sri.ai.util.base.ConstructorByLazyReflection.constructorByLazyReflectionOfClassAndParameters;

import java.util.Iterator;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.api.AnytimeSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node.api.AnytimeExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node.core.AnytimeExactBPNodeWithIdentitySimplification;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBPRootNode;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.ConstructorByLazyReflection;
import com.sri.ai.util.computation.anytime.api.Approximation;

/**
 * An adapter from a given {@link AnytimeExactBPNode} class
 * (default {@link AnytimeExactBPNodeWithIdentitySimplification})
 * to {@link BinaryFunction} on a query variable and factor network.
 * 
 * @author braz
 *
 */
public class AnytimeExactBP implements AnytimeSolver {

	private ConstructorByLazyReflection<AnytimeExactBPNode> anytimeExactBPNodeConstructor;
	
	public AnytimeExactBP(Class<? extends AnytimeExactBPNode> anytimeExactBPNodeClass) {
		this.anytimeExactBPNodeConstructor = constructorByLazyReflectionOfClassAndParameters(anytimeExactBPNodeClass, ExactBPNode.class);
	}

	public AnytimeExactBP() {
		this(AnytimeExactBPNodeWithIdentitySimplification.class);
	}

	@Override
	public Iterator<Approximation<Factor>> apply(Variable query, FactorNetwork factorNetwork) {
		return makeRootAnytimeExactBPRootNodeNode(new ExactBPRootNode(query, factorNetwork));
	}

	/**
	 * Makes the root {@link AnytimeExactBPNode} for solving this problem.
	 */
	@SuppressWarnings("unchecked")
	protected AnytimeExactBPNode<Variable, Factor> makeRootAnytimeExactBPRootNodeNode(ExactBPRootNode exactBPRootNode) {
		return anytimeExactBPNodeConstructor.newInstance(exactBPRootNode);
	}

}
