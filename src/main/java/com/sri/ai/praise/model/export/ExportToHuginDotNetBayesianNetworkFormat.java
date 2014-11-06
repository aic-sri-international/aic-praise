package com.sri.ai.praise.model.export;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
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

/**
 * Exports an LBP Model, which is grounded first, to <a href="http://www.hugin.com/">Hugin's</a>
 * Bayesian Network <a href="http://download.hugin.com/webdocs/manuals/api-manual.pdf">.net</a> format.
 * 
 * @author oreilly
 *
 */
public class ExportToHuginDotNetBayesianNetworkFormat {
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
		
			StringWriter stringWriter = new StringWriter();
			export(groundedModelResult, stringWriter, process);
		
			System.out.print(stringWriter.toString());
		} catch (ModelGrounding.ModelGroundingException mge) {
			System.err.println("Model Ground Exception: "+mge.getMessage());
			for (ModelGrounding.ModelGroundingError error : mge.getErrors()) {
				System.err.println(error.getErrorType());
				System.err.println(error.getInExpression());
			}
		}
	}
	
	public static void export(ModelGrounding.GroundedModelResult groundedModelResult, Writer output, RewritingProcess process) 
			throws IOException {

		Model groundedModel = groundedModelResult.getGroundedModel();
		ParfactorsDeclaration parfacdecs = groundedModel.getParfactorsDeclaration();
		Expression factors = parfacdecs.getParfactors().get(0);
		System.out.println("#ground factors="+ExtensionalSet.cardinality(factors));
		
		// Determine the factor to random variable associations
		Map<Expression, List<Expression>> factorToRandomVariables = new LinkedHashMap<>();
		Map<Expression, List<Expression>> randomVariableToFactors = new LinkedHashMap<>();
		for (Expression factorExpr : ExtensionalSet.getElements(factors)) {
			BracketedExpression factor = (BracketedExpression) factorExpr;
			Expression factorValue = factor.getInnerExpression();
			Set<Expression> randomVariableValues = collectDistinctGroundedRandomVariables(factorValue, process);
			factorToRandomVariables.put(factorValue, new ArrayList<>(randomVariableValues));
			for (Expression rvv : randomVariableValues) {
				List<Expression> rvvFactors = randomVariableToFactors.get(rvv);
				if (rvvFactors == null) {
					rvvFactors = new ArrayList<>();
					randomVariableToFactors.put(rvv, rvvFactors);
				}
				rvvFactors.add(factorValue);
			}			
		}
		
		while (!randomVariableToFactors.isEmpty()) {
			// STEP 1:
			// Pick a variable such that number of random variable neighbors
			// (that is, other vars sharing factors) is minimal.
			Expression c = pickMinimal(factorToRandomVariables);
System.out.println("smallest="+c);

			// STEP 2:
			// Multiply all factors containing this var - remove them from
			// the factor graph - to give an unnormalized CPT phi for the var
			List<Expression> factorsContainingC = randomVariableToFactors.get(c);
			randomVariableToFactors.remove(c);
			Set<Expression> p = new LinkedHashSet<>();
			for (Expression f : factorsContainingC) {
				p.addAll(factorToRandomVariables.get(f));
				factorToRandomVariables.remove(f);
			}
			p.remove(c); // Ensure c is not included in p
			
			
			// STEP 3:
			// In the unnormalized CPT phi, let C be the child 
			// (latest in the ordering) and P the other variables in it.
			// For each assignment to parents P, compute a new factor on P,
			// Z(P) = sum_{C} phi(C,P).
			// Replace each entry phi(C,P) by phi(C,P)/Z(P).
			// Store the now normalized CPT in the Bayesian network, and 
			// add factor Z to the factor network.
			
			// Repeat for the remaining of the factor graph

		}
	}
	
	//
	// PRIVATE
	//
	private static Expression pickMinimal(Map<Expression, List<Expression>> factorToRandomVariables) {
		Expression result = null;
		Map<Expression, Set<Expression>> randomVariableNeighbors = new LinkedHashMap<>();
		for (List<Expression> randomVariablesInFactor : factorToRandomVariables.values()) {
			for (Expression rv : randomVariablesInFactor) {
				Set<Expression> neighbors = Util.getValuePossiblyCreatingIt(randomVariableNeighbors, rv, LinkedHashSet.class);
				neighbors.addAll(randomVariablesInFactor); // NOTE: include self for efficience as all counts will end up being + 1.				
			}
		}
		int smallest = Integer.MAX_VALUE;
		for (Map.Entry<Expression, Set<Expression>> entry : randomVariableNeighbors.entrySet()) {
System.out.println("neighbors="+entry);			
			if (entry.getValue().size() < smallest) {
				result = entry.getKey();
				smallest = entry.getValue().size();
			}
		}
		
		return result;
	}
		
	private static Set<Expression> collectDistinctGroundedRandomVariables(Expression factorValue, RewritingProcess process) {
		Set<Expression> result = new LinkedHashSet<>();
		Iterator<Expression> rvvExpressions = GetRandomVariables.getRandomVariableValueExpressionsIterator(factorValue, process);
		while (rvvExpressions.hasNext()) {
			result.add(rvvExpressions.next());
		}
		return result;
	}
}