package com.sri.ai.test.praise.inference.anytimeexactbp;

import static com.sri.ai.util.Util.println;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.praise.inference.anytimeexactbp.AnytimeExactBP;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.api.AtomicPolytope;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.Polytopes;
import com.sri.ai.praise.inference.anytimeexactbp.polytope.core.ProductPolytope;
import com.sri.ai.praise.inference.exactbp.api.ExactBP;
import com.sri.ai.praise.inference.exactbp.core.ExactBPFromVariable;
import com.sri.ai.praise.inference.representation.Table.TableFactorNetwork;
import com.sri.ai.praise.inference.representation.Table.TableVariable;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.inference.representation.api.Variable;
import com.sri.ai.praise.model.v1.imports.uai.UAIModel;
import com.sri.ai.praise.model.v1.imports.uai.UAIModelReader;
import com.sri.ai.util.Util;
/**
 * 
 * I did a function that computes AEBP (rodrigo's version) and and prints the result at the end of each iteration.
 * It seems to be working correctly, except for the following fact:
 * 		- Products are not computed, but only represented (we have lots 
 * of {(on Variables) factors} * {(on Variables) factors} in the Polytope)
 * 		- For some reason, the Polytope returned here is a "ProductPolytope" while in the other test
 * ({@link AnytimeExactBPTest} test with expressions) a "IntensionalConvexHullOfFactors" is returned.
 * 		- I did not understand how to use {@link AssignmentsIterator} since ProductPolytope is not an expression, is it? 
 * @author gabriel
 *
 */
public class AnytimeExactBPTest2 {
	
	private static void solveWithAnytimeExactBP(TableVariable query, TableFactorNetwork factorNetwork) {
		println("\nSolving with Anytime\n");
		ExactBP<Variable,Factor> exactBP = new ExactBPFromVariable(query, factorNetwork);
		AnytimeExactBP<Variable,Factor> anytimeExactBP = new AnytimeExactBP<>(exactBP);
		
		int i = 1;
		println(anytimeExactBP.next());
		while (anytimeExactBP.hasNext()) {
			ProductPolytope p = (ProductPolytope)anytimeExactBP.next();
			println(i++);
			println("p:   " + p);
			println("var: " + p.getFreeVariables());
			println("pol: " + p.getPolytopes());
			AtomicPolytope equivalentAtomicPolytopeOnQuery = Polytopes.getEquivalentAtomicPolytopeOn(query, p);
			println("Computed equivalent atomic polytope on query.");
			if (equivalentAtomicPolytopeOnQuery instanceof IntensionalConvexHullOfFactors) {
				println("Number of indices:" + ((IntensionalConvexHullOfFactors)equivalentAtomicPolytopeOnQuery).getIndices());
			}
			println("Now generating its string to show it.");
			//println("single intensional convex hull: " + equivalentAtomicPolytopeOnQuery);
		}
	}


	public static void main(String[] args) {

		FileReader modelFile;
		try {
			modelFile = new FileReader(new File("").getAbsolutePath()+"/UAITests/BN_0.uai" );
			UAIModel model = UAIModelReader.read(modelFile);
			
			// Converting the network
			TableFactorNetwork network = new TableFactorNetwork(model);
			
			//get one variable and test over the network
			ArrayList<Variable> vars = new ArrayList<>(network.getBs());
			TableVariable query = (TableVariable) vars.get(0);// pick any variable 
			
			solveWithAnytimeExactBP(query, network);
			
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

}
