package com.sri.ai.test.praise.inference.anytimeexactbp;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import com.google.common.base.Predicate;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.AEBP;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.TestCases;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.representation.api.EditableFactorNetwork;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.rodrigo.AnytimeExactBP;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.eager.api.ExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.eager.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.IntensionalConvexHullOfFactors;
import com.sri.ai.util.rplot.AEBPRPlotting;
import com.sri.ai.util.rplot.dataframe.AEBPTestingDataFrame;
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
public class AnytimeExactBPTest3 {

	private static AEBPTestingDataFrame solveGabrielWithoutBoxing(Variable query, EditableFactorNetwork factorNetwork, long maximunTimeInSeconds,
			String PGMName) {
		println("\nSolving with Gabriel's Anytime - no boxing\n");
		AEBP aebp = new AEBP(factorNetwork, query);
		AEBPTestingDataFrame df = AEBPSolver.solve(aebp, query, maximunTimeInSeconds, 1, PGMName, "noBoxing");
		return df;
	}

	public static AEBPTestingDataFrame solveGabrielWithBoxing(Variable query, EditableFactorNetwork factorNetwork, long maximunTimeInSeconds,
			String PGMName) {
		println("\nSolving with Gabriel's Anytime - boxing\n");
		
		
		Predicate<Polytope> criteria = p-> ((IntensionalConvexHullOfFactors)p).getIndices().size()>10;
		AEBP aebp = new AEBP(factorNetwork, query, criteria );
		/*while(aebp.hasNext()) {
			Polytope p = aebp.next();
			println(p);
		}*/
		return  AEBPSolver.solve(aebp, query, maximunTimeInSeconds, 0, PGMName, "Boxing");
	}
	
	public static AEBPTestingDataFrame solveRodrigoWithoutBoxing(Variable query, EditableFactorNetwork factorNetwork, long maximunTimeInSeconds,
			String PGMName) {
		println("\nSolving with Rodrigo's Anytime - no boxing\n");
		ExactBPNode<Variable,Factor> exactBP = new ExactBP(query, factorNetwork);
		AnytimeExactBP<Variable,Factor> anytimeExactBP = new AnytimeExactBP<>(exactBP);
		return AEBPSolver.solve(anytimeExactBP,query,maximunTimeInSeconds,0,PGMName,"Rodrigo");
	}

	public static void testing(List<TableFactor> factors,int time,TableVariable query,String PGMname) {
		TableFactorNetwork network =  new TableFactorNetwork(factors);
		//AEBPTestingDataFrame df  = solveGabrielWithBoxing(query, network, time,PGMname);
		AEBPTestingDataFrame df = solveGabrielWithoutBoxing(query, network, time,PGMname);
		//AEBPTestingDataFrame df3 = solveRodrigoWithoutBoxing(query, network, time, PGMname);
		
		//df.concatenate(df2);
		//df.concatenate(df3);
		AEBPRPlotting.plottingTheInterval(df,true,PGMname + ".pdf");
		AEBPRPlotting.plottingTheInterval(df,true,PGMname + ".pdf");
	}

	public static void main(String[] args) {
		for(Integer i : list(1,2,3,4)) {
			testing(TestCases.treeWithExponentialGaussianRandomEntries(10, i, 2, .3, .1),
					20,
					new TableVariable("g_0_0", 2),
					"treeExpGauss" + i);
			}
		
		List<TableFactor> fs = TestCases.TableFactorALARM();
		testing(fs,
				500,
				new TableVariable("g22", 3),
				"alarmante");
		
		
		/*	for(Integer i : list(-2,-1,0,1)) {
			testing((List<TableFactor>) TestCases.isingModelGridWithWeigthsAndPotetialNormalyDistributed(20, Math.pow(10, i), true),
					5,
					new TableVariable("10_10", 2),
					"IsingNormalRandom" + i);
			}
		
		println(factors.get(27).getVariables());
		for(TableFactor f : factors) {
			println(f);
		}
		List<TableFactor> factors = TestCases.getListOfTableFactors("Seg","BN_8");
		
		testing(factors,
				5000,
				factors.get(27).getVariables().get(0),
				"promedas-");
		*/
		ArrayList<File> Files = TestCases.retrieveUAIFilesInFolder("Segmentation");
		int i = 0;
		for(File file : Files) {
			
			println(i++ + "---------------" + file.getName() + "---------------" );
		}
	
		
		for(File file : Files) {
			file = Files.get(48);
			println("---------------" + file.getName() + "---------------" );
			List<TableFactor> factors = TestCases.getListOfTableFactors("Segmentation",file.getName());
			
			testing(factors,
					20,
					factors.get(0).getVariables().get(0),
					"Segmentation-"+file.getName()+"0");
			
			testing(factors,
					200,
					factors.get(10).getVariables().get(0),
					"Segmentation-"+file.getName()+"1");
			
			testing(factors,
					20,
					factors.get(15).getVariables().get(0),
					"Segmentation-"+file.getName()+"2");
		}
	}

	@SuppressWarnings("unchecked")
	public static void testingIsing() {
		for(Integer i : list(-2,-1,0,1)) {
		testing((List<TableFactor>) TestCases.isingModelGridWithWeigthsAndPotetialNormalyDistributed(20, Math.pow(10, i), true),
				20,
				new TableVariable("10_10", 2),
				"IsingNormalRandom" + i);
		}
	}

	public static void testingALARM() {
		ArrayList<TableFactor> factors = TestCases.TableFactorALARM();
		LinkedHashSet<TableVariable> variablesInALARM= new  LinkedHashSet<>();
		for(TableFactor f : factors) {
			variablesInALARM.addAll(f.getVariables());
		}
		
		println(variablesInALARM);
		for(TableVariable v : variablesInALARM) {
			testing(factors,
				20,
				v,
				"Alarm-Var=" + v
				);
		}
	}
}
