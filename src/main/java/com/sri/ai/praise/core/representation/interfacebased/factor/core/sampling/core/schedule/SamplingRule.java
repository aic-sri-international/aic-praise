package com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.util.Util.assertType;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.myAssert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableIsDefinedGoal;
import com.sri.ai.util.planning.api.Plan;
import com.sri.ai.util.planning.api.Rule;
import com.sri.ai.util.planning.api.State;
import com.sri.ai.util.planning.core.AbstractAtomicPlan;
import com.sri.ai.util.tree.DefaultTree;
import com.sri.ai.util.tree.Tree;

public class SamplingRule extends AbstractAtomicPlan implements Rule<SamplingGoal> {
	
	public static final double MAXIMUM_ESTIMATED_SUCCESS_WEIGHT = Plan.MAXIMUM_ESTIMATED_SUCCESS_WEIGHT;

	private SamplingFactor sampler;

	private Collection<? extends SamplingGoal> antecedents;

	private Collection<? extends SamplingGoal> consequents;

	private boolean hasFired;
	
	public static SamplingRule samplingRuleFromGoals(
			SamplingFactor sampler, 
			Collection<? extends SamplingGoal> consequents, 
			Collection<? extends SamplingGoal> antecedents, 
			double estimatedSuccessWeight) {
		
		return new SamplingRule(sampler, consequents, antecedents, estimatedSuccessWeight);
	}

	/**
	 * Convenience creator taking variables to stand for {@link VariableIsDefined} goals.
	 * @param sampler
	 * @param consequentVariables
	 * @param antecedentVariables
	 * @param estimatedSuccessWeight
	 * @return
	 */
	public static SamplingRule samplingRuleFromVariables(
			SamplingFactor sampler, 
			Collection<? extends Variable> consequentVariables, 
			Collection<? extends Variable> antecedentVariables, 
			double estimatedSuccessWeight) {
		
		List<? extends SamplingGoal> antecedents = mapIntoList(antecedentVariables, v -> new VariableIsDefinedGoal(v));
		List<? extends SamplingGoal> consequents = mapIntoList(consequentVariables, v -> new VariableIsDefinedGoal(v));
		
		return new SamplingRule(sampler, consequents, antecedents, estimatedSuccessWeight);
	}

	/**
	 * Convenience creator taking either variables or goals as consequents and antecedents.
	 * Variables are converted into {@link VariableIsDefined} goals.
	 * @param sampler
	 * @param consequents
	 * @param antecedents
	 * @param estimatedSuccessWeight
	 * @return
	 */
	public static SamplingRule samplingRule(
			SamplingFactor sampler, 
			Collection<?> consequents, 
			Collection<?> antecedents, 
			double estimatedSuccessWeight) {
		
		List<? extends SamplingGoal> antecedentGoals = mapIntoList(antecedents, SamplingRule::convertToGoal);
		List<? extends SamplingGoal> consequentGoals = mapIntoList(consequents, SamplingRule::convertToGoal);
		
		return new SamplingRule(sampler, consequentGoals, antecedentGoals, estimatedSuccessWeight);
	}
	
	private static SamplingGoal convertToGoal(Object object) {
		if (object instanceof Variable) {
			return new VariableIsDefinedGoal((Variable) object);
		}
		else if (object instanceof SamplingGoal){
			return (SamplingGoal) object;
		}
		else {
			throw new Error("Sampling rule should have received either variable or sampling goal, but got " + object.getClass().getSimpleName() + " " + object);
		}
	}

	/**
	 * Convenience deterministic sampling rule creator taking variables to stand for {@link VariableIsDefined} goals.
	 * @param consequentVariables
	 * @param antecedentVariables
	 * @return
	 */
	public static SamplingRule deterministicSamplingRuleFromVariables(
			Collection<? extends Variable> consequentVariables, 
			Collection<? extends Variable> antecedentVariables) {
		
		return samplingRuleFromVariables(null, consequentVariables, antecedentVariables, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}

	/**
	 * Convenience deterministic sampling rule creator taking variables to stand for {@link VariableIsDefined} goals.
	 * @param sampler
	 * @param consequentVariables
	 * @param antecedentVariables
	 * @return
	 */
	public static SamplingRule deterministicSamplingRuleFromVariables(
			SamplingFactor sampler, 
			Collection<? extends Variable> consequentVariables, 
			Collection<? extends Variable> antecedentVariables) {
		
		return samplingRuleFromVariables(sampler, consequentVariables, antecedentVariables, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}

	public static SamplingRule deterministicSamplingRuleFromGoals(
			SamplingFactor sampler, 
			Collection<? extends SamplingGoal> consequentVariables, 
			Collection<? extends SamplingGoal> antecedentVariables) {
		
		return samplingRuleFromGoals(sampler, consequentVariables, antecedentVariables, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}

	/**
	 * Convenience creator for deterministic sampling rule taking either variables or goals as consequents and antecedents.
	 * Variables are converted into {@link VariableIsDefined} goals.
	 * @param sampler
	 * @param consequents
	 * @param antecedents
	 * @return
	 */
	public static SamplingRule deterministicSamplingRule(SamplingFactor sampler, Collection<?> consequents, Collection<?> antecedents) {
		return samplingRule(sampler, consequents, antecedents, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}

	/**
	 * Convenience creator for deterministic sampling rule taking either variables or goals as consequents and antecedents
	 * and setting sampler factor to null.
	 * Variables are converted into {@link VariableIsDefined} goals.
	 * @param sampler
	 * @param consequents
	 * @param antecedents
	 * @return
	 */
	public static SamplingRule deterministicSamplingRule(Collection<?> consequents, Collection<?> antecedents) {
		return samplingRule(null, consequents, antecedents, MAXIMUM_ESTIMATED_SUCCESS_WEIGHT);
	}
	
	public SamplingRule(SamplingFactor sampler, Collection<? extends SamplingGoal> consequents, Collection<? extends SamplingGoal> antecedents, double estimatedSuccessWeight) {
		super(estimatedSuccessWeight);
		this.sampler = sampler;
		this.antecedents = antecedents;
		this.consequents =  consequents;
		this.hasFired = false;
	}

	public SamplingFactor getSamplingFactor() {
		myAssert(sampler != null, this, () -> " does not have a sampling factor associated with it but it has been requested.");
		return sampler;
	}
	
	public boolean hasFired() {
		return hasFired;
	}
	
	@Override
	public Collection<? extends SamplingGoal> getConsequents() {
		return consequents;
	}

	@Override
	public Collection<? extends SamplingGoal> getAntecendents() {
		return antecedents;
	}

	public void reset() {
		hasFired = false;
	}

	public SamplingRule replaceFactor(SamplingFactor newSamplingFactor) {
		SamplingRule result = new SamplingRule(newSamplingFactor, getConsequents(), getAntecendents(), getEstimatedSuccessWeight());
		return result;
	}

	public SamplingRule replaceGoals(Function<SamplingGoal, SamplingGoal> replacement) {
		List<SamplingGoal> newAntecedents = mapIntoList(getAntecendents(), replacement);
		List<SamplingGoal> newConsequents = mapIntoList(getConsequents(), replacement);
		SamplingRule result = new SamplingRule(getSamplingFactor(), newConsequents, newAntecedents, getEstimatedSuccessWeight());
		return result;
	}
	
	@Override
	public State execute(State state) {
//		println("SamplingRule: Executing rule " + this);
//		println("SamplingRule: Factor: " + getSamplingFactor());
		SamplingState samplingState = assertType(state, SamplingState.class, getClass());
//		println("SamplingRule: Values of variable antecedents: " + getVariableAntecendentsAndTheirValues(samplingState.getUnmodifiableSample()));
		samplingState.sampleOrWeight(getSamplingFactor());
//		println("SamplingRule: Values of variable consequents: " + getVariableConsequentsAndTheirValues(samplingState.getUnmodifiableSample()));
//		println("SamplingRule: Weight: " + samplingState.getUnmodifiableSample().getPotential());
//		println("SamplingRule: Sample now: " + samplingState.getUnmodifiableSample());
//		println();
		hasFired = true;
		return state;
	}	
//	private Map<Variable, Object> getVariableAntecendentsAndTheirValues(Sample sample) {
//		return getGoalVariablesAndTheirValues(getAntecendents(), sample);
//	}
//
//	private Map<Variable, Object> getVariableConsequentsAndTheirValues(Sample sample) {
//		return getGoalVariablesAndTheirValues(getConsequents(), sample);
//	}

//	private
//	Map<Variable, Object>
//	getGoalVariablesAndTheirValues(Collection<? extends SamplingGoal> goals, Sample sample) {
//		Map<Variable, Object> result = map();
//		for (SamplingGoal goal : goals) {
//			if (goal instanceof VariableIsDefinedGoal) {
//				Variable variable = ((VariableIsDefinedGoal) goal).getVariable();
//				Object value = sample.get(variable);
//				result.put(variable, value);
//			}
//		}
//		return result;
//	}

	public void sampleOrWeigh(Sample sample) {
		getSamplingFactor().sampleOrWeigh(sample);
	}
	
	@Override
	public Tree<String> stringTree() {
		return new DefaultTree<String>(toString());
	}

	public static final Comparator<? super SamplingRule> SUCCESS_COMPARATOR = new Comparator<SamplingRule>() {
	
		@Override
		public int compare(SamplingRule rule1, SamplingRule rule2) {
			return Double.compare(rule2.getEstimatedSuccessWeight(), rule1.getEstimatedSuccessWeight());
			// note the inversion, as we want descending order
		}
		
	};

	public boolean isSatisfied(Sample sample) {
		return forAll(getAntecendents(), a -> a.isSatisfied(sample));
	}

	public SamplingRule copyWithNewSampler(SamplingFactor newSampler) {
		return new SamplingRule(newSampler, consequents, antecedents, getEstimatedSuccessWeight());
	}

	public SamplingRule copyWithNewSamplerAndAntecedents(SamplingFactor newSampler, ArrayList<SamplingGoal> newAntecedents) {
		return new SamplingRule(newSampler, consequents, newAntecedents, getEstimatedSuccessWeight());
	}

	public SamplingRule copyWithNewAntecedents(ArrayList<SamplingGoal> newAntecedents) {
		return new SamplingRule(sampler, consequents, newAntecedents, getEstimatedSuccessWeight());
	}

	@Override
	public String toString() {
		String consequentsString = join(getConsequents());
		String antecedentsString = join(getAntecendents());
		String factorString = getSamplingFactor().toString();
		String weightString = isDeterministic()? "infinite" : Double.toString(getEstimatedSuccessWeight());
		String result = consequentsString + " <= " + antecedentsString + "        with sampling factor: " + factorString + " and weight " + weightString;
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((antecedents == null) ? 0 : antecedents.hashCode());
		result = prime * result + ((consequents == null) ? 0 : consequents.hashCode());
		result = prime * result + ((sampler == null) ? 0 : sampler.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		SamplingRule other = (SamplingRule) obj;
		if (antecedents == null) {
			if (other.antecedents != null) {
				return false;
			}
		} else if (!antecedents.equals(other.antecedents)) {
			return false;
		}
		if (consequents == null) {
			if (other.consequents != null) {
				return false;
			}
		} else if (!consequents.equals(other.consequents)) {
			return false;
		}
		if (sampler == null) {
			if (other.sampler != null) {
				return false;
			}
		} else if (!sampler.equals(other.sampler)) {
			return false;
		}
		return true;
	}
}
