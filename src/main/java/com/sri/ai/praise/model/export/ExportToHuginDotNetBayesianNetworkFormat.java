package com.sri.ai.praise.model.export;

import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.stream.Stream;

import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Parser;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.praise.lbp.LBPFactory;
import com.sri.ai.praise.model.GetRandomVariables;
import com.sri.ai.praise.model.Model;
import com.sri.ai.praise.model.ModelGrounding;
import com.sri.ai.praise.model.ModelGrounding.GroundedModelResult;
import com.sri.ai.praise.model.ParfactorsDeclaration;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.CartesianProductEnumeration;
import com.sri.ai.util.math.MixedRadixNumber;

/**
 * Exports an LBP Model, which is grounded first, to <a href="http://www.hugin.com/">Hugin's</a>
 * Bayesian Network <a href="http://download.hugin.com/webdocs/manuals/api-manual.pdf">.net</a> format.
 * 
 * @author oreilly
 *
 */
public class ExportToHuginDotNetBayesianNetworkFormat {
	
	public static interface CPTOutputListener {
		void randomVariables(Iterator<Expression> randomVariables);
		void newCPT(CPT cpt);
	}
	
	public static void main(String[] args) throws IOException {
		if (args.length < 1 || args.length % 2 != 1) {
			throw new IllegalArgumentException("Invalid # of arguments, must be 1=model file and the optional 2 to n pairs of sort name followed by size to use when grounding the model");
		}
		
		Parser parser = new AntlrGrinderParserWrapper();
		// Read in the model declaration
		StringJoiner modelDeclarationSJ = new StringJoiner("\n");
		try (Stream<String> declarationLines = Files.lines(Paths.get(args[0]))) {
			declarationLines.sequential().forEachOrdered(line -> modelDeclarationSJ.add(line));
		}
		// Can be called with explicitly assigned sizes to the sorts associated with the model
		Map<Expression, Expression> globalObjects = new LinkedHashMap<Expression, Expression>();
		for (int i = 1; i < args.length; i+=2) {
			globalObjects.put(parser.parse("| "+args[i]+" |"), Expressions.makeSymbol(Integer.valueOf(args[i+1])));
		}
		
		Expression modelDefinition = parser.parse(modelDeclarationSJ.toString());
		Model      modelToExport   = new Model(modelDefinition, Collections.emptySet());
		
		RewritingProcess process = LBPFactory.newLBPProcess(modelDefinition);
		process.getGlobalObjects().putAll(globalObjects);
		
		process = Model.setRewritingProcessesModel(modelDefinition, modelToExport.getKnownRandomVariableNameAndArities(), process);
		
		try {
			GroundedModelResult groundedModelResult = ModelGrounding.groundModel(process);
		
			export(groundedModelResult, new CPTOutputListener() {
				
				@Override
				public void randomVariables(Iterator<Expression> randomVariables) {
					
					while (randomVariables.hasNext()) {
						Expression rv = randomVariables.next();
						StringJoiner sj = new StringJoiner("\n");
						
						sj.add("node "+getLegalHuginId(rv));
						sj.add("{");
						sj.add("  states = (\"false\" \"true\");");
						sj.add("  label  = \""+rv+"\";");
						sj.add("}");
						
						System.out.println(sj.toString());
					}
				}
				
				@Override
				public void newCPT(CPT cpt) {
					StringJoiner sj = new StringJoiner("\n");
					sj.add("potential "+getPotentialSignature(cpt));
					sj.add("{");
					sj.add("    data = "+getPotentialData(cpt));
					sj.add("}");
					
					System.out.println(sj.toString());
				}
				
				private String getLegalHuginId(Expression rv) {
					return rv.toString().replace('(', '_').replace(',', '_').replace(')', '_');
				}
				
				private String getPotentialSignature(CPT cpt) {
					StringJoiner sj = new StringJoiner(" ", "(", ")");
					
					sj.add(getLegalHuginId(cpt.randomVariable));
					sj.add("|");
					for (Expression p : cpt.parents) {
						sj.add(getLegalHuginId(p));
					}
					
					return sj.toString();
				}
				
				private String getPotentialData(CPT cpt) {
					StringJoiner sj = new StringJoiner(" ", "(", ");");
					
					for (double d : cpt.values) {
						sj.add(""+d);
					}
					
					return sj.toString();
				}
			}, process);
		} catch (ModelGrounding.ModelGroundingException mge) {
			System.err.println("Model Ground Exception: "+mge.getMessage());
			for (ModelGrounding.ModelGroundingError error : mge.getErrors()) {
				System.err.println(error.getErrorType());
				System.err.println(error.getInExpression());
			}
		}
	}
	
	public static void export(ModelGrounding.GroundedModelResult groundedModelResult, CPTOutputListener cptOutListener, RewritingProcess process) 
			throws IOException {

		Model groundedModel = groundedModelResult.getGroundedModel();
		ParfactorsDeclaration parfacdecs = groundedModel.getParfactorsDeclaration();
		Expression factors = parfacdecs.getParfactors().get(0);
		System.out.println("#ground factors="+ExtensionalSet.cardinality(factors));
		
		// Determine the factor to random variable associations
		Map<Expression, Set<Expression>>  factorToRandomVariables = new LinkedHashMap<>();
		Map<Expression, List<Expression>> randomVariableToFactors = new LinkedHashMap<>();
		Map<Expression, Factor>           factorZToTable          = new LinkedHashMap<>();
		for (Expression factorExpr : ExtensionalSet.getElements(factors)) {
			BracketedExpression factor = (BracketedExpression) factorExpr;
			Expression factorValue = factor.getInnerExpression();
			Set<Expression> randomVariableValues = collectDistinctGroundedRandomVariables(factorValue, process);
			factorToRandomVariables.put(factorValue, randomVariableValues);
			for (Expression rvv : randomVariableValues) {
				List<Expression> rvvFactors = Util.getValuePossiblyCreatingIt(randomVariableToFactors, rvv, ArrayList.class);			
				rvvFactors.add(factorValue);
			}			
		}
		
		cptOutListener.randomVariables(randomVariableToFactors.keySet().iterator());
		
		while (!randomVariableToFactors.isEmpty()) {
			// STEP 1:
			// Pick a variable such that number of random variable neighbors
			// (that is, other vars sharing factors) is minimal.
			Expression c = pickMinimal(factorToRandomVariables);

			// STEP 2:
			// Multiply all factors containing this var - remove them from
			// the factor graph - to give an unnormalized CPT phi for the var
			List<Expression> factorsContainingC = randomVariableToFactors.get(c);
			Set<Expression> p = new LinkedHashSet<>();
			for (Expression f : factorsContainingC) {			
				p.addAll(factorToRandomVariables.get(f));
			}
			
			Factor cFactor = newFactor(p, factorsContainingC, factorToRandomVariables, factorZToTable);
			p.remove(c); // Now ensure c is not included in p
			
			randomVariableToFactors.remove(c);
			for (Expression f : factorsContainingC) {
				factorToRandomVariables.remove(f);
			}
			for (Expression pv : p) {
				randomVariableToFactors.get(pv).removeAll(factorsContainingC);
			}
					
			// STEP 3:
			// In the unnormalized CPT phi, let C be the child 
			// (latest in the ordering) and P the other variables in it.
			// For each assignment to parents P, compute a new factor on P,
			// Z(P) = sum_{C} phi(C,P).	
			// Replace each entry phi(C,P) by phi(C,P)/Z(P).
			// Store the now normalized CPT in the Bayesian network, and 
			// add factor Z to the factor network.
			Pair<Factor, CPT> zFactorAndcCPT = cFactor.sumOutChildAndCreateCPT(c);
			Factor zFactor = zFactorAndcCPT.first;
		    CPT    cCPT    = zFactorAndcCPT.second;
			cptOutListener.newCPT(cCPT);
			
			if (zFactor != null) {
				Expression factorZ = getZFactorName(factorZToTable.size()+1);
				for (Expression rv : p) {
					randomVariableToFactors.get(rv).add(factorZ);
				}
				factorToRandomVariables.put(factorZ, p);
				factorZToTable.put(factorZ, zFactor);			
			}
			// Repeat for the remaining of the factor graph
		}
	}
	
	//
	// PRIVATE
	//
	private static Expression pickMinimal(Map<Expression, Set<Expression>> factorToRandomVariables) {
		Expression result = null;
		Map<Expression, Set<Expression>> randomVariableNeighbors = new LinkedHashMap<>();
		for (Set<Expression> randomVariablesInFactor : factorToRandomVariables.values()) {
			for (Expression rv : randomVariablesInFactor) {
				Set<Expression> neighbors = Util.getValuePossiblyCreatingIt(randomVariableNeighbors, rv, LinkedHashSet.class);
				neighbors.addAll(randomVariablesInFactor); // NOTE: include self for efficience as all counts will end up being + 1.				
			}
		}
		int smallest = Integer.MAX_VALUE;
		for (Map.Entry<Expression, Set<Expression>> entry : randomVariableNeighbors.entrySet()) {			
			if (entry.getValue().size() < smallest) {
				result = entry.getKey();
				smallest = entry.getValue().size();
			}
		}
		
		if (smallest > 30) {
			throw new IllegalStateException("Too large a CPT will need be generated as #parents="+(smallest-1));
		}
		
		return result;
	}
	
	private static Expression getZFactorName(int idx) {
		return DefaultSymbol.createSymbol("factorZ_"+idx);
	}
		
	private static Set<Expression> collectDistinctGroundedRandomVariables(Expression factorValue, RewritingProcess process) {
		Set<Expression> result = new LinkedHashSet<>();
		Iterator<Expression> rvvExpressions = GetRandomVariables.getRandomVariableValueExpressionsIterator(factorValue, process);
		while (rvvExpressions.hasNext()) {
			result.add(rvvExpressions.next());
		}
		return result;
	}
	
	private static Factor newFactor(Set<Expression> randomVariables, List<Expression> factorsContainingC, Map<Expression, Set<Expression>> factorToRandomVariables, Map<Expression, Factor> factorZToTable) {
		Factor result = null;		
		List<Factor> factors = new ArrayList<>();
		for (Expression factorExpr : factorsContainingC) {
			Factor factor = factorZToTable.get(factorExpr);
			if (factor == null) {
				// Is not a Z factor, need to create
				factor = new Factor(factorToRandomVariables.get(factorExpr), factorExpr);	
			}
			factors.add(factor);
		}
		
		result = new Factor(randomVariables, factors);
		
		return result;
	}
	
	static class Factor {
		private Set<Expression> randomVariables = new LinkedHashSet<>();
		private double[]        values;
		//
		private List<List<Expression>>   assignValues     = new ArrayList<>(); 
		private Map<Expression, Integer> randomVarToIndex = new HashMap<>();
		private MixedRadixNumber         mrn;
		
		public Factor(Set<Expression> randomVariables, Expression factorExpr) {
			initialize(randomVariables);

			CartesianProductEnumeration<Expression> cpe = new CartesianProductEnumeration<>(assignValues);
			int idx = 0;
			while (cpe.hasMoreElements()) {
				final List<Expression> assignments = cpe.nextElement();
				Expression valueExpr = factorExpr.replaceAllOccurrences(new AbstractReplacementFunctionWithContextuallyUpdatedProcess() {					
					@Override
					public Expression apply(Expression expression, RewritingProcess process) {
						Expression result = expression;
						Integer idx = randomVarToIndex.get(expression);
						if (idx != null) {
							result = assignments.get(idx);
						}
						return result;
					}
				}, LBPFactory.newLBPProcess());			
				this.values[idx] = LBPFactory.newNormalize().rewrite(valueExpr, LBPFactory.newLBPProcess()).doubleValue();
				idx++;
			}
		}
		
		public Factor(Set<Expression> randomVariables, List<Factor> factors) {
			initialize(randomVariables);	
			
			CartesianProductEnumeration<Expression> cpe = new CartesianProductEnumeration<>(assignValues);
			int idx = 0;
			while (cpe.hasMoreElements()) {
				List<Expression> assignments = cpe.nextElement();
				
				double product = 1;
				for (Factor factor : factors) {
					product *= getValueForFactorsAssignments(factor, assignments);
				}
				values[idx] = product;
				idx++;
			}
		}
		
		public double getValueForAssignments(Map<Expression, Expression> assignmentsMap) {
			double result = 0;
			
			if (!this.randomVariables.containsAll(assignmentsMap.keySet())) {
				throw new IllegalArgumentException("Assignments cannot be mapped to this factor, assignments="+assignmentsMap+", this factor has rvs="+this.randomVariables);
			}
			
			List<List<Expression>> possibleValues = new ArrayList<>();
			for (Expression rv : this.randomVariables) {
				Expression assignment = assignmentsMap.get(rv);
				if (assignment == null) {
					possibleValues.add(getPossibleValuesForRandomVariable(rv));
				}
				else {
					possibleValues.add(Arrays.asList(assignment));
				}
			}
			
			CartesianProductEnumeration<Expression> cpe = new CartesianProductEnumeration<>(possibleValues);
			while (cpe.hasMoreElements()) {
				List<Expression> assignments = cpe.nextElement();
				result += this.values[getIndex(assignments)];
			}
			return result;
		}
		
		public int getIndex(List<Expression> fullAssignment) {
			int result = -1; // i.e. invalid
			
			if (fullAssignment.size() != randomVariables.size()) {
				throw new IllegalArgumentException("Assignments not same size as # of random variables");				
			}
			
			int[] radixValues = new int[fullAssignment.size()];
			for (Expression rv : randomVariables) {
				int idx          = this.randomVarToIndex.get(rv);
				radixValues[idx] = this.getPossibleValuesForRandomVariable(rv).indexOf(fullAssignment.get(idx));
			}
			result = this.mrn.getValueFor(radixValues).intValue();
			
			return result;
		}
		
		public Pair<Factor, CPT> sumOutChildAndCreateCPT(Expression rvToSumOut) {			
			Set<Expression> remaining = new LinkedHashSet<>(randomVariables);
			remaining.remove(rvToSumOut);
			List<Expression> rvPossibleValues = this.getPossibleValuesForRandomVariable(rvToSumOut);
			double[] cptValues = new double[this.values.length];
			Factor summedOut   = null;
			if (remaining.size() == 0) {
				// No summed out factor as no remaining elements but still need to ensure the cpt values are normalized
				int idx = 0;
				while (idx < cptValues.length) {
					double total = 0;
					for (int i = 0; i < rvPossibleValues.size(); i++) {
						total += this.values[idx+i];
					}
					for (int i = 0; i < rvPossibleValues.size(); i++) {
						cptValues[idx+i] = this.values[idx+i]/total;
					}
				    idx += rvPossibleValues.size();
				}
			}
			else {
				summedOut = new Factor(remaining);
				CartesianProductEnumeration<Expression> cpe = new CartesianProductEnumeration<>(summedOut.assignValues);
				int soIdx = 0, cptIdx = 0;
				while (cpe.hasMoreElements()) {
					List<Expression> assignments = cpe.nextElement();
					Map<Expression, Expression> assignmentsMap = new HashMap<>();
					for (Expression rv : summedOut.randomVariables) {
						int rvIdx = summedOut.randomVarToIndex.get(rv);
						assignmentsMap.put(rv, assignments.get(rvIdx));
					}
					summedOut.values[soIdx] = this.getValueForAssignments(assignmentsMap);			
						
					for (Expression rvAssignment : rvPossibleValues) {
						assignmentsMap.put(rvToSumOut, rvAssignment);
						cptValues[cptIdx] = this.getValueForAssignments(assignmentsMap) / summedOut.values[soIdx];
						cptIdx++;
					}
					
					soIdx++;
				}
			}
		
			Pair<Factor, CPT> result = new Pair<>(summedOut, new CPT(rvToSumOut, remaining, cptValues));

			return result; 
		}
		
		public int size() {
			return randomVariables.size();
		}
		
		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			StringJoiner  sj = new StringJoiner(",", "F(", ")");
			for (Expression rv : randomVariables) {
				sj.add(rv.toString());
			}
			sb.append(sj.toString());
			sb.append("):[");
			sj = new StringJoiner(",");
			for (int i = 0; i < values.length; i++) {
				sj.add(""+values[i]);
			}
			sb.append(sj.toString());
			sb.append("]");
			return sb.toString();
		}
		
		//
		// PRIVATE
		//
		private Factor(Set<Expression> randomVariables) {
			initialize(randomVariables);
		}
		
		private void initialize(Set<Expression> randomVariables) {
			this.randomVariables.addAll(randomVariables);
			int[] radices = new int[randomVariables.size()];
			int rIdx = 0;
			for (Expression parent : randomVariables) {
				List<Expression> possibleValues = getPossibleValuesForRandomVariable(parent);
				radices[rIdx] = possibleValues.size();
				rIdx++;
				assignValues.add(possibleValues);
				randomVarToIndex.put(parent, randomVarToIndex.size());
			}
			this.mrn    = new MixedRadixNumber(BigInteger.ZERO, radices);
			this.values = new double[this.mrn.getMaxAllowedValue().intValue()+1];
		}
		
		private double getValueForFactorsAssignments(Factor factor, List<Expression> assignments) {
			double result = 0;
			
			Map<Expression, Expression> assignmentsMap = new HashMap<>();
			for (Expression rv : this.randomVariables) {
				// NOTE: the factor can contains a subset of this factors random variables
				if (factor.randomVariables.contains(rv)) {
					int rvIdx = this.randomVarToIndex.get(rv);
					assignmentsMap.put(rv, assignments.get(rvIdx));
				}
			}
			
			result = factor.getValueForAssignments(assignmentsMap);
			
			return result;
		}
		
		private List<Expression> getPossibleValuesForRandomVariable(Expression rv) {
			return Arrays.asList(Expressions.FALSE, Expressions.TRUE);
		}
	}
	
	static class CPT {
		private Expression      randomVariable;
		private Set<Expression> parents;
		private double[]        values;
		
		public CPT(Expression randomVariable, Set<Expression> parents, double[] values) {
			this.randomVariable = randomVariable;
			this.parents        = parents;
			this.values         = values;
		}
		
		public String toString() {
			StringBuilder sb = new StringBuilder();
			StringJoiner  sj = new StringJoiner(",");
			sb.append("P("+randomVariable+" | ");
			for (Expression p : parents) {
				sj.add(p.toString());
			}
			sb.append(sj.toString());
			sb.append("):[");
			sj = new StringJoiner(",");
			for (int i = 0; i < values.length; i++) {
				sj.add(""+values[i]);
			}
			sb.append(sj.toString());
			sb.append("]");
			return sb.toString();
		}
	}
}