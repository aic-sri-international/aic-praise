package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.algorithm;

import static com.sri.ai.util.base.ConstructorReflectionManager.constructorReflectionManager;

import java.util.Iterator;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node.AnytimeExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.node.AnytimeExactBPNodeWithoutSimplification;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.api.ExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.ConstructorReflectionManager;
import com.sri.ai.util.computation.anytime.api.Approximation;

/**
 * An adapter from a given {@link AnytimeExactBPNode} class
 * (default {@link AnytimeExactBPNodeWithoutSimplification})
 * to {@link BinaryFunction} on a query variable and factor network.
 * 
 * @author braz
 *
 */
public class AnytimeExactBP implements BinaryFunction<Variable, FactorNetwork, Iterator<Approximation<Factor>>> {

	private ConstructorReflectionManager<AnytimeExactBPNode> nodeConstructor;
	
	public AnytimeExactBP(Class<? extends AnytimeExactBPNode> anytimeExactBPNodeClass) {
		this.nodeConstructor = constructorReflectionManager(anytimeExactBPNodeClass, ExactBPNode.class);
	}

	public AnytimeExactBP() {
		this(AnytimeExactBPNodeWithoutSimplification.class);
	}

	@Override
	public Iterator<Approximation<Factor>> apply(Variable query, FactorNetwork factorNetwork) {
		return makeAnytimeExactBPNodeWithoutSimplification(new ExactBP(query, factorNetwork));
	}

	/**
	 * Makes the {@link AnytimeExactBPNode} from an {@link ExactBP} object.
	 */
	@SuppressWarnings("unchecked")
	protected AnytimeExactBPNode<Variable, Factor> makeAnytimeExactBPNodeWithoutSimplification(ExactBP exactBP) {
		return nodeConstructor.newInstance(exactBP);
	}

}
