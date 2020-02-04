package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.base.ZeroFactor.ZERO_FACTOR;
import static com.sri.ai.util.Util.intersect;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.subtract;
import static com.sri.ai.util.Util.unionOfMapsIfCompatibleOrNull;

import java.util.Collection;
import java.util.List;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.sample.DefaultAssignment;
import com.sri.ai.util.Enclosing;
import com.sri.ai.util.explanation.tree.ExplanationTree;

// TODO: unit tests

/**
 * A abstract implementation of factors based on a base factor which are conditioned by an assignment
 * to some of its variables.
 * <p>
 * The class in abstract because it does not commit to a particular representation
 * for the base factors. Extending classes that make such commitments must be defined.
 * <p>
 * This class follows an unusual strategy for construction of new objects.
 * The available constructor does not accept empty conditions or trivial (zero or identity) factors
 * (errors are thrown in those cases)
 * because in those cases there are more efficient ways to proceed than to return a new instance.
 * <p>
 * Instead, the class offers the static builder {@link #build(Sample, Factor, BuildingPolicy)}.
 * Extending classes should declare a new static method (suggested to be also <code>build</code>)
 * but without the policy parameter, which in turn invokes {@link #build(Sample, Factor, BuildingPolicy)}
 * with a class-specific building policy object.
 * <p>
 * Naturally, this means defining a class-specific {@link BuildingPolicy} that knows how to
 * condition non-trivial and non-conditioned factors within the chosen
 * type of factor implementation.
 * <p>
 * The extending class must also define a {@link #getBuildingPolicy()} method returning
 * a appropriate making policy. This method is used by the class internally whenever it needs
 * to build a conditioned factor.
 * <p>
 * In summary, extending classes must do the following:
 * <ul>
 * <li> define an implementation of {@link BuildingPolicy} for the chosen representation
 * <li> define a static method <code>build(Sample, Factor)</code> (this is a suggested name)
 * which invokes {@link #build(Sample, Factor, BuildingPolicy)}
 * which an appropriate instance of {@link BuildingPolicy}.
 * <li> implement the abstract method {@link #getBuildingPolicy()} to return
 * an appropriate instance of {@link BuildingPolicy}. Note that this method is not static
 * so it cannot be used to provide the instance needed by <code>build(Sample, Factor)</code>,
 * so the implementation of this method will have to create an instance
 * of the appropriate implementation of {@link BuildingPolicy}
 * </ul>
 * 
 * @author braz
 *
 */
public abstract class AbstractConditionedFactor implements Factor {

	/*
	 * Design note:
	 * 
	 * One might ask why this class was designed to have a {@link BuildingPolicy}.
	 * This allows the static method {@link #build(Sample, Factor, BuildingPolicy)}
	 * to be re-used in extending methods.
	 * If we had followed the alternative of simply having an abstract method for building,
	 * then a static method in extending classes would not have access to that
	 * (because the abstract method would not be static itself).
	 * One way to circumvent that would be to create a dummy conditioned factor
	 * to be passed to the state method just to give it access to the abstract method
	 * implementation.
	 * This however would be contrived since this object would not really be a real conditioned factor.
	 * In fact, the policy design is basically the same idea, but instead of using a contrived fake conditioned factor
	 * to give access to non-static methods to a static method, we explicit define this object to
	 * be a policy, this representing its true purpose accurately and clearly.
	 */
	
	//////////////////////// ABSTRACT METHODS
	
	abstract protected BuildingPolicy getBuildingPolicy();

	/**
	 * This method remains abstract at the level of {@link AbstractConditionedFactor} because
	 * it is highly dependent on the specific representation of the factor being conditioned.
	 */
	@Override
	abstract public Factor normalize();

	////////////////////////
	
	/**
	 * An interface for objects encoding the building policy for implementations of {@link AbstractConditionedFactor};
	 * it contains one method which will typically just invoke the extending class's protected constructor.
	 */
	protected static interface BuildingPolicy {
		/**
		 * Makes a new conditioned factor in the extending class, using the knowledge that
		 * the given conditions are not empty and the given factor is not zero.
		 * This will typically simply invoke the protected constructor.
		 */
		Factor makeConditionedNonZeroAndNonConditionedFactor(Sample conditioningSample, Factor factor);
	}
	
	/**
	 * A static method for building new conditioned factors, given a {@link BuildingPolicy}.
	 */
	protected static Factor build(Sample conditioningSample, Factor factor, BuildingPolicy buildingPolicy) {
		if (conditioningSample.size() == 0) {
			return factor;
		}
		else if (factor.isZero()) {
			return ZERO_FACTOR;
		}
		else if (factor instanceof AbstractConditionedFactor) {
			return conditionAlreadyConditionedfactor(conditioningSample, (AbstractConditionedFactor) factor, buildingPolicy);
		}
		else {
			return buildingPolicy.makeConditionedNonZeroAndNonConditionedFactor(conditioningSample, factor);
		}
	}

	private static Factor conditionAlreadyConditionedfactor(Sample conditioningSample, AbstractConditionedFactor conditionedFactor, BuildingPolicy makingPolicy) {
		Sample intersectionOfConditioningSamples = intersection(conditioningSample, conditionedFactor.conditioningSample);
		if (intersectionOfConditioningSamples == null) {
			return ZERO_FACTOR;
		}
		else {
			return build(intersectionOfConditioningSamples, conditionedFactor.factor, makingPolicy);
		}
	}

	private static Sample intersection(Sample sample1, Sample sample2) {
		var map1 = sample1.getAssignment().mapValue();
		var map2 = sample2.getAssignment().mapValue();
		var unionOrNull = unionOfMapsIfCompatibleOrNull(map1, map2);
		if (unionOrNull != null) {
			DefaultAssignment unionAssignment = new DefaultAssignment(unionOrNull);
			return sample1.copyWithNewAssignment(unionAssignment);
		}
		else {
			return null;
		}
	}

	/////////////////////
	
	private Sample conditioningSample;
	
	private Factor factor;
	
	/**
	 * Constructor of a new {@link AbstractConditionedFactor} instance
	 * requiring that the conditions be not trivial and the factor not zero, throwing an exception otherwise.
	 * @param conditioningSample
	 * @param factor
	 */
	protected AbstractConditionedFactor(Sample conditioningSample, Factor factor) {
		myAssert(conditioningSample.size() != 0,  () -> AbstractConditionedFactor.class + " constructor cannot be invoked on empty conditioning sample.");
		myAssert(!factor.isZero(),     () -> AbstractConditionedFactor.class + " constructor cannot be invoked on zero factor.");
		this.conditioningSample = conditioningSample;
		this.factor = factor;
	}

	/////////////////////

	public Sample getConditioningSample() {
		return conditioningSample;
	}

	public Factor getFactor() {
		return factor;
	}

	/////////////////////

	/**
	 * Returns a conditioned factor according to the present class's {@link BuildingPolicy} as returned by {@link #getBuildingPolicy()}.
	 * @param conditioningSample
	 * @param factor
	 * @return
	 */
	protected final Factor getConditionedFactor(Sample conditioningSample, Factor factor) {
		return build(conditioningSample, factor, getBuildingPolicy());
	}

	/////////////////////
	
	@Override
	public boolean contains(Variable variable) {
		boolean result =
				conditioningSample.get(variable) == null
				&&
				factor.getVariables().contains(variable);
		return result;
	}

	@Override
	public List<? extends Variable> getVariables() {
		List<Variable> result = subtract(factor.getVariables(), conditioningSample.getVariables());
		return result;
	}

	/////////////////////
	
	@Override
	public boolean isIdentity() {
		// This is always false given the conditions enforced by constructor.
		return false;
	}

	@Override
	public boolean isZero() {
		// This is always false given the conditions enforced by constructor.
		return false;
	}

	/////////////////////
	
	@Override
	public Factor multiply(Factor another) {
		Factor factorTimesAnother = factor.multiply(another);
		Factor result = getConditionedFactor(conditioningSample, factorTimesAnother);
		return result;
	}

	@Override
	public Factor sumOut(List<? extends Variable> variablesToSumOut) {
		Factor summedOutFactor = factor.sumOut(variablesToSumOut);
		Factor result = getConditionedFactor(conditioningSample, summedOutFactor);
		return result;
	}

	@Override
	public Factor add(Factor another) {
		myAssert(!intersect(conditioningSample.getVariables(), another.getVariables()), () -> (new Enclosing() {}).methodName() + " does not support the addition of a conditioned factor and another factor, if the latter contains variables conditioned in the first one");
		Factor factorPlusAnother = factor.add(another);
		Factor result = getConditionedFactor(conditioningSample, factorPlusAnother);
		return result;
	}

	@Override
	public Factor ABS_invert() {
		Factor invertedFactor = factor.ABS_invert();
		Factor result = getConditionedFactor(conditioningSample, invertedFactor);
		return result;
	}

	@Override
	public Factor max(Collection<? extends Variable> variablesToMaximize) {
		Factor maximizedFactor = factor.max(variablesToMaximize);
		Factor result = getConditionedFactor(conditioningSample, maximizedFactor);
		return result;
	}

	@Override
	public Factor argmax(Collection<? extends Variable> variablesToMaximize) {
		Factor maximizedFactor = factor.argmax(variablesToMaximize);
		Factor result = getConditionedFactor(conditioningSample, maximizedFactor);
		return result;
	}

	@Override
	public Factor min(Collection<? extends Variable> variablesToMinimize) {
		Factor minimizedFactor = factor.min(variablesToMinimize);
		Factor result = getConditionedFactor(conditioningSample, minimizedFactor);
		return result;
	}

	@Override
	public Factor argmin(Collection<? extends Variable> variablesToMinimize) {
		Factor minimizedConditionedFactor = factor.argmin(variablesToMinimize);
		Factor result = getConditionedFactor(conditioningSample, minimizedConditionedFactor);
		return result;
	}

	@Override
	public ExplanationTree getExplanation() {
		throw new Error((new Enclosing() {}).methodName() + " not supported by " + AbstractConditionedFactor.class);
	}

	@Override
	public void setExplanation(ExplanationTree explanation) {
		throw new Error((new Enclosing() {}).methodName() + " not supported by " + AbstractConditionedFactor.class);
	}

	///////////////////
	
	@Override
	public String toString() {
		return "Conditioned factor by " + conditioningSample.getAssignment() + ", original factor is " + factor;
	}
}
