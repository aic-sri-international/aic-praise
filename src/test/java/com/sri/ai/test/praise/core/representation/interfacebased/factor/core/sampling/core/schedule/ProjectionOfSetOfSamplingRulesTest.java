package com.sri.ai.test.praise.core.representation.interfacebased.factor.core.sampling.core.schedule;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule.deterministicSamplingRule;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.Collection;
import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.function.Predicate;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.factor.SamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.sample.Sample;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.api.schedule.SamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.factor.AbstractSamplingFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.DefaultSamplingRuleSet;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.ProjectionOfSetOfSamplingRules;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.SamplingRule;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.FunctionOnSetOfVariablesSatisfiesCondition;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.LuckySamplingGoal;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.sampling.core.schedule.goal.VariableEqualsSomethingDifferentFrom;
import com.sri.ai.util.planning.core.ProjectionOfSetOfRules;

/**
 * Tests the projection of sampling rules.
 * Note that the projection of rules in general is already tested at the level of {@link ProjectionOfSetOfRules},
 * so here the point is to test the unique aspects of {@link ProjectionOfSetOfSamplingRules}
 * concerning the treatment of contingent goals.
 *
 * @author braz
 *
 */
class ProjectionOfSetOfSamplingRulesTest {

	@Test
	void test() {
		
		Variable x = new DefaultVariable("x");
		Variable y = new DefaultVariable("y");
		Variable z = new DefaultVariable("z");
		Variable w = new DefaultVariable("w");
		List<Variable> variables;
		List<Variable> remainingVariables;
		
		List<SamplingRule> samplingRules;
		List<SamplingRule> expectedSamplingRules;
		
		////////////////
		
		variables = list(x, y, w, z);
		samplingRules = list();
		
		remainingVariables = list(y);
		expectedSamplingRules = list();
		
		runTest(variables, samplingRules, remainingVariables, expectedSamplingRules);
		
		////////////////
		
		variables = list(x, y, w, z);
		samplingRules = 
				list(
						rule(list(x), list()),
						rule(list(y), list(x)));
		
		remainingVariables = list(y);
		expectedSamplingRules = 
				list(
						rule(list(y), list()));
		
		runTest(variables, samplingRules, remainingVariables, expectedSamplingRules);
		
		////////////////
		
		variables = list(x, y, w, z);
		samplingRules = 
				list(
						rule(list(x), list()),
						rule(list(y), list(x, z)));
		
		remainingVariables = list(y, z);
		expectedSamplingRules = 
				list(
						rule(list(y), list(z)));
		
		runTest(variables, samplingRules, remainingVariables, expectedSamplingRules);
		
		////////////////
		
		variables = list(x, y, w, z);
		samplingRules = 
				list(
						rule(list(x), list()),
						rule(
								list(y), 
								list(x, new VariableEqualsSomethingDifferentFrom(x, 0), z)));
		
		remainingVariables = list(y, z);
		expectedSamplingRules = 
				list(
						rule(
								list(y), 
								list(
										lucky(new VariableEqualsSomethingDifferentFrom(x, 0), variables, samplingRules), 
										z)));
		
		runTest(variables, samplingRules, remainingVariables, expectedSamplingRules);
		
		////////////////
		
		variables = list(x, y, w, z);
		samplingRules = 
				list(
						rule(list(x), list()),
						rule(
								list(y), 
								list(
										x, 
										new VariableEqualsSomethingDifferentFrom(x, 0), 
										z, 
										new VariableEqualsSomethingDifferentFrom(z, 0))));
		
		remainingVariables = list(y, z);
		expectedSamplingRules = 
				list(
						rule(
								list(y), 
								list(
										lucky(new VariableEqualsSomethingDifferentFrom(x, 0), variables, samplingRules), 
										z, 
										new VariableEqualsSomethingDifferentFrom(z, 0))));
		
		runTest(variables, samplingRules, remainingVariables, expectedSamplingRules);
		
		////////////////
		
		// Using contingent goal that mixes remaining and eliminated variables
		// Adding an original sampling rule y <- w and making w remaining for good measure
		
		Function<Collection<Integer>, Integer> dummyFunction = c -> 0;
		Predicate<Integer> dummyPredicate = i -> true;
		
		variables = list(x, y, w, z);
		samplingRules = 
				list(
						rule(list(y), list(w)),
						
						rule(list(x), list()),
						
						rule(
								list(y), 
								list(
										x, 
										new VariableEqualsSomethingDifferentFrom(x, 0), 
										z, 
										new VariableEqualsSomethingDifferentFrom(z, 0),
										new FunctionOnSetOfVariablesSatisfiesCondition<Integer>(
												"dummy function", 
												list(x,z), 
												dummyFunction, 
												dummyPredicate))));
		
		remainingVariables = list(y, z, w);
		expectedSamplingRules = 
				list(
						rule(list(y), list(w)),
						rule(
								list(y), 
								list(
										lucky(new VariableEqualsSomethingDifferentFrom(x, 0), variables, samplingRules), 
										z, 
										new VariableEqualsSomethingDifferentFrom(z, 0),
										lucky(
												new FunctionOnSetOfVariablesSatisfiesCondition<Integer>(
														"dummy function", 
														list(x,z), 
														dummyFunction, 
														dummyPredicate),
												variables, samplingRules
												))));
		
		runTest(variables, samplingRules, remainingVariables, expectedSamplingRules);
		
		////////////////
		
		variables = list(x, y, w, z);
		samplingRules = 
				list(
						rule(list(x), list()),
						rule(
								list(y), 
								list(
										x, 
										new VariableEqualsSomethingDifferentFrom(x, 0), 
										z, 
										new VariableEqualsSomethingDifferentFrom(z, 0))));
		
		remainingVariables = list(y); // Z NO LONGER REMAINING VARIABLE, but cannot be internally sampled either
		expectedSamplingRules = list();
		
		runTest(variables, samplingRules, remainingVariables, expectedSamplingRules);
		
		////////////////
		
		variables = list(x, y, w, z);
		samplingRules = 
				list(
						rule(list(x), list()),
						rule(list(z), list()), // NOW Z can be internally sampled
						rule(
								list(y), 
								list(
										x, 
										new VariableEqualsSomethingDifferentFrom(x, 0), 
										z, 
										new VariableEqualsSomethingDifferentFrom(z, 0))));
		
		remainingVariables = list(y);
		expectedSamplingRules = 
				list(
						rule(
								list(y), 
								list(
										lucky(new VariableEqualsSomethingDifferentFrom(x, 0), variables, samplingRules), 
										lucky(new VariableEqualsSomethingDifferentFrom(z, 0), variables, samplingRules))));
		
		runTest(variables, samplingRules, remainingVariables, expectedSamplingRules);
	}

	private LuckySamplingGoal lucky(
			SamplingGoal innerGoal,
			List<Variable> variables,
			List<SamplingRule> samplingRules) {
		return new LuckySamplingGoal(innerGoal, originalFactor(variables, samplingRules));
	}

	private void runTest(
			List<Variable> variables,
			List<SamplingRule> samplingRules,
			List<Variable> remainingVariables,
			List<SamplingRule> expectedSamplingRules) {

		SamplingFactor originalFactor;
		SamplingFactor newFactor;
		originalFactor = originalFactor(variables, samplingRules);
		newFactor = new TestSamplingFactor("new", remainingVariables, list());
		expectedSamplingRules = mapIntoList(expectedSamplingRules, r -> r.copyWithNewSampler(newFactor));
		SamplingRuleSet result = ProjectionOfSetOfSamplingRules.project(remainingVariables, newFactor, originalFactor);
		
		println("Expected:");
		println(join(".\n", expectedSamplingRules));
		println("Actual:");
		println(join(".\n", result.getSamplingRules()));
		assertEquals(expectedSamplingRules, result.getSamplingRules());
	}

	private TestSamplingFactor originalFactor(List<Variable> variables, List<SamplingRule> samplingRules) {
		return new TestSamplingFactor("original", variables, samplingRules);
	}
	
	private static SamplingRule rule(List<?> consequents, List<?> antecedents) {
		return deterministicSamplingRule(consequents, antecedents);
	}

	private static class TestSamplingFactor extends AbstractSamplingFactor {

		private String name;
		private SamplingRuleSet samplingRules;
		
		public TestSamplingFactor(String name, List<? extends Variable> variables, List<? extends SamplingRule> samplingRules) {
			super(variables, new Random());
			this.name = name;
			this.samplingRules = new DefaultSamplingRuleSet(mapIntoList(samplingRules, r -> r.copyWithNewSampler(this)));
		}

		@Override
		public void sampleOrWeigh(Sample sample) {
			// do nothing
		}

		@Override
		protected SamplingRuleSet makeSamplingRules() {
			return samplingRules;
		}
		
		@Override
		public String toString() {
			return name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((name == null) ? 0 : name.hashCode());
			result = prime * result + ((samplingRules == null) ? 0 : samplingRules.hashCode());
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
			TestSamplingFactor other = (TestSamplingFactor) obj;
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			if (samplingRules == null) {
				if (other.samplingRules != null) {
					return false;
				}
			} else if (!samplingRules.equals(other.samplingRules)) {
				return false;
			}
			return true;
		}
		
	}
}
