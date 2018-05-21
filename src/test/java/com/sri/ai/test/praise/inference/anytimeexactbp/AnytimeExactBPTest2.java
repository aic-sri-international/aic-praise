package com.sri.ai.test.praise.inference.anytimeexactbp;

import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.println;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.praise.inference.generic.anytime.anytimeexactbp.AnytimeExactBP;
import com.sri.ai.praise.inference.generic.anytime.gabrielstry.AEBP;
import com.sri.ai.praise.inference.generic.anytime.gabrielstry.TestCases;
import com.sri.ai.praise.inference.generic.anytime.polytope.api.Polytope;
import com.sri.ai.praise.inference.generic.anytime.polytope.core.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.inference.generic.anytime.polytope.core.Polytopes;
import com.sri.ai.praise.inference.generic.anytime.polytope.core.ProductPolytope;
import com.sri.ai.praise.inference.generic.anytime.polytope.core.Simplex;
import com.sri.ai.praise.inference.generic.exactbp.api.ExactBP;
import com.sri.ai.praise.inference.generic.exactbp.core.ExactBPFromVariable;
import com.sri.ai.praise.inference.generic.representation.api.Factor;
import com.sri.ai.praise.inference.generic.representation.api.Variable;
import com.sri.ai.praise.inference.generic.representation.table.TableFactor;
import com.sri.ai.praise.inference.generic.representation.table.TableFactorNetwork;
import com.sri.ai.praise.inference.generic.representation.table.TableVariable;
import com.sri.ai.praise.model.v1.imports.uai.UAIModel;
import com.sri.ai.praise.model.v1.imports.uai.UAIModelReader;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.computation.anytime.api.Approximation;
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
public class AnytimeExactBPTest2 {
	
	private static AEBPTestingDataFrame solveWithBothMethods(TableVariable query, TableFactorNetwork factorNetwork, double maximunTimeInSeconds,
			String PGMName) {
		AEBPTestingDataFrame result = solveWithRodrigos(query, factorNetwork, maximunTimeInSeconds, PGMName);
		AEBPTestingDataFrame gabriels = solveWithGabriels(query, factorNetwork, maximunTimeInSeconds, PGMName);
		
		for (int i = 0; i < gabriels.getNumberOfRows(); i++) {
			result.addRow(gabriels.getRow(i));
		}
		return result;
	}
	
	private static AEBPTestingDataFrame solveWithRodrigos(TableVariable query, TableFactorNetwork factorNetwork, double maximunTimeInSeconds,
			String PGMName) {
		println("\nSolving with Rodrigo's Anytime\n");
		ExactBP<Variable,Factor> exactBP = new ExactBPFromVariable(query, factorNetwork);
		AnytimeExactBP<Variable,Factor> anytimeExactBP = new AnytimeExactBP<>(exactBP);
		return solveAndStoreInDataFrame(anytimeExactBP,query,maximunTimeInSeconds,0,PGMName,"Rodrigo's");		
	}
	
	private static AEBPTestingDataFrame solveWithGabriels(TableVariable query, TableFactorNetwork factorNetwork, double maximunTimeInSeconds,
			String PGMName) {
		println("\nSolving with Gabriel's Anytime\n");
		AEBP aebp = new AEBP(factorNetwork, query);
		return solveAndStoreInDataFrame(aebp,query,maximunTimeInSeconds,0,PGMName,"Gabriel's");
	}

	private static AEBPTestingDataFrame solveAndStoreInDataFrame(Iterator<? extends Approximation<Factor>> anytimeEBPIterator, TableVariable query, 
			double maximunTimeInSeconds,
			int runNumber, String PGMName,String methodName) {
		
		AEBPTestingDataFrame df = new AEBPTestingDataFrame();
		
		int i = 0;
		Double currentTotalTime = .0;
		long startTime = System.currentTimeMillis();
		while ( currentTotalTime < maximunTimeInSeconds && anytimeEBPIterator.hasNext()) {
			println(++i);
			Long currentTime = System.currentTimeMillis();
			Polytope result = (Polytope) anytimeEBPIterator.next();
			Pair<? extends List<Double>, ? extends List<Double>> maxAndMin = printPolytopeAndMaxAndMin(query, result);
			currentTotalTime = .001*(System.currentTimeMillis() - startTime);
			Double currentPartialTime = .001*(System.currentTimeMillis() - currentTime);
			println("time: " +  currentTotalTime);	
			
			df.addRow(runNumber,i,maxAndMin.first.get(0),maxAndMin.second.get(0),currentPartialTime,currentTotalTime
					,methodName,PGMName);
			println("...");
		}
		println("-----Done-----");
		return df;
		
	}

	public static Pair<? extends List<Double>,? extends List<Double>> printPolytopeAndMaxAndMin(TableVariable query, Polytope polytope) {
		Pair<? extends List<Double>,? extends List<Double>> result = new Pair<>();
		if(polytope instanceof ProductPolytope) {
			polytope = Polytopes.getEquivalentAtomicPolytopeOn(query, (ProductPolytope) polytope);
		}
		
		if (polytope instanceof IntensionalConvexHullOfFactors) {
			println("Number of indices:" + 
						((IntensionalConvexHullOfFactors)polytope).getIndices().size());
			ArrayList<Double> maxProba = getMaxMinProbabilityFromIntensionalConvHull(
												(IntensionalConvexHullOfFactors)polytope,true);
			if(query.getCardinality()== 2) {
				println("query Cardinality: " + query.getCardinality());
				println("max: " + maxProba.get(0) + " | min: "+ (1-maxProba.get(1)));
				result = new Pair<>(maxProba,list(1-maxProba.get(1),1-maxProba.get(0)));
			}
			else {
				ArrayList<Double> minProba = getMaxMinProbabilityFromIntensionalConvHull(
												(IntensionalConvexHullOfFactors)polytope,false);
				result = new Pair<>(maxProba,minProba);

				println("query Cardinality: " + query.getCardinality());
				println("max: " + maxProba + "\nmin: "+ minProba);
			}
		}
		else if(polytope instanceof Simplex) {
			println("simplex.");
			result = new Pair<>(list(1.,1.),list(0.,0.));
		}
		else {
			unhandledCase();
		}
		
		return result;
	}

	private static ArrayList<Double> getMaxMinProbabilityFromIntensionalConvHull(
			IntensionalConvexHullOfFactors polytopeOnQuery,boolean maxOrMin){
		TableFactor factor = (TableFactor) polytopeOnQuery.getFactor();
		TableVariable query = (TableVariable) polytopeOnQuery.getFreeVariables().iterator().next();
		
		List<Variable> variablesInOrder = new ArrayList<>(polytopeOnQuery.getIndices());
		List<List<Integer>> listOflistOfInstantiations = getListOfListOfInstantiations(variablesInOrder);
		Iterator<ArrayList<Integer>> iter = getCartesianProductWithValuesOfVariablesToSum(listOflistOfInstantiations);

		Double[] resultArray = maxOrMin ? 
				initializeWithValue(query.getCardinality(),-1.0):
					initializeWithValue(query.getCardinality(),2.0);
		
		for(ArrayList<Integer> instantiation : in(iter)) {
			LinkedHashMap<Variable, Integer> mapOfInstantiations = new LinkedHashMap<>();
			int k = 0;
			for(Variable v: variablesInOrder) {
				mapOfInstantiations.put(v,instantiation.get(k++));
			}
			ArrayList<Double> nonNormalizedProbabilitiesForThisInstantiation = new ArrayList<>(query.getCardinality());
			for(int i = 0; i< query.getCardinality();i++) {
				mapOfInstantiations.put(query, i);
				nonNormalizedProbabilitiesForThisInstantiation.add(factor.getEntryFor(mapOfInstantiations));
			}
			
			Double[] normalizedProba = nomalize(nonNormalizedProbabilitiesForThisInstantiation);
		
			for (int i = 0; i < normalizedProba.length; i++) {
				if(maxOrMin && normalizedProba[i] > resultArray[i]) {
					resultArray[i] = normalizedProba[i];
				}
				if(!maxOrMin && normalizedProba[i] < resultArray[i]) {
					resultArray[i] = normalizedProba[i];
				}
			}
			
		}
		ArrayList<Double> result = new ArrayList<>();
		for (int i = 0; i < resultArray.length; i++) {
			result.add(resultArray[i]);
		}
		return result;
		
	}
	
	public static Double[] initializeWithValue(int length,Double value) {
		Double[] result = new Double[length];
		for(int i = 0; i< length;i++) {
			result[i]= value;
		}
		return result;
	}
	
	private static Double[] nomalize(ArrayList<Double> nonNormalized) {
		Double sum = .0;
		for (Double d : nonNormalized) {
			sum += d;
		}
		
		Double[] result = new Double[nonNormalized.size()];
		
		for (int i = 0; i < nonNormalized.size(); i++) {
			result[i] = nonNormalized.get(i)/sum;
		}
		return result;
	}

	private static Iterator<ArrayList<Integer>> getCartesianProductWithValuesOfVariablesToSum(List<List<Integer>> listOfValuesForTheVariables) {
		
		ArrayList<NullaryFunction<Iterator<Integer>>> iteratorForListOfVariableValues = 
				mapIntoArrayList(listOfValuesForTheVariables, element -> () -> element.iterator());
		
		Iterator<ArrayList<Integer>> cartesianProduct = new CartesianProductIterator<Integer>(iteratorForListOfVariableValues);
		return cartesianProduct;
	}

	public static List<List<Integer>> getListOfListOfInstantiations(
			List<Variable> ListOfVariables) {
		List<List<Integer>> listOflistOfInstantiations = new ArrayList<>();
		for(Variable v : ListOfVariables) {
			TableVariable tv = (TableVariable) v;
			ArrayList<Integer> toAdd = new ArrayList<>(tv.getCardinality());
			for(int i = 0; i< tv.getCardinality();i++) {
				toAdd.add(i);
			}
			listOflistOfInstantiations.add(toAdd);
		}
		return listOflistOfInstantiations;
	}

	private static void unhandledCase() {
		println("Error");
	}

	public static Pair<TableVariable, TableFactorNetwork> importUAIFile(String fileName) {
		Pair<TableVariable, TableFactorNetwork> pairQueryNet = new Pair<>();
		
		try {
			FileReader modelFile = new FileReader(new File("").getAbsolutePath()+"/UAITests/"+fileName );
			UAIModel model = UAIModelReader.read(modelFile);
			
			// Converting the network
			pairQueryNet.second= new TableFactorNetwork(model);
			
			//get one variable and test over the network
			ArrayList<Variable> vars = new ArrayList<>(pairQueryNet.second.getBs());
			pairQueryNet.first = (TableVariable) vars.get(0);// pick any variable 	
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return pairQueryNet;
	}

	public static Pair<TableVariable, TableFactorNetwork> isingModel(String queryName, int gridSize,double potential) {
		Pair<TableVariable, TableFactorNetwork> pairQueryNet;
		List<TableFactor> factors = null;//TestCases.isingModelGridWithRandomWeigthsAndPotetial(gridSize, potential);
		
		TableVariable query = null;
		for(TableFactor f : factors) {
			for(TableVariable v : f.getVariables()) {
				if(v.getName().equals(queryName) ){
					query = v;
					//Util.println("Query not null");
				}
			}
			//Util.println(f.getVariables());	
		}
		TableFactorNetwork tfn = new TableFactorNetwork(factors);

		pairQueryNet = new Pair<>(query,tfn);
		return pairQueryNet;
	}

	public static void main(String[] args) {
		
		String[] fileNames= {
		//"1a2j.uai",
		//"1a1x.uai",
		"grid40x40.f5.wrap.uai",
		"or_chain_10.fg.uai",
		"1a2j.uai","grid80x80.f10.uai",
		"1a3c.uai","grid80x80.f10.wrap.uai","grid80x80.f15.uai","or_chain_1.fg.uai",
		"BN_0.uai",      "BN_5.uai",                "grid80x80.f15.uai",
		"BN_101.uai",    "BN_6.uai",                "grid80x80.f15.wrap.uai",
		"BN_10.uai",     "grid10x10.f10.uai",       "grid80x80.f2.uai",
		"BN_11.uai",     "grid10x10.f10.wrap.uai",  "grid80x80.f2.wrap.uai",
		"BN_12.uai",     "grid10x10.f15.wrap.uai",  "grid80x80.f5.uai",
		"BN_13.uai",     "grid10x10.f5.wrap.uai",   "grid80x80.f5.wrap.uai",
		"BN_14.uai",     "grid20x20.f10.uai",       
		"BN_15.uai",     "grid20x20.f15.uai",       "or_chain_1.fg.uai",
		"BN_16.uai",     "grid20x20.f15.wrap.uai",  "pedigree13.uai",
		"BN_18.uai",     "grid20x20.f5.uai",        "pedigree18.uai",
		"BN_1.uai", 	 "grid20x20.f5.wrap.uai",   "pedigree19.uai",
		"BN_2.uai",      "grid40x40.f5.uai",        "pedigree20.uai"
		};

		for(String fileName: fileNames) {
			println("----------" + fileName + "----------");
			printPlotToFile(fileName, 20);
		}
		
		Pair<TableVariable, TableFactorNetwork> pairQueryNet = isingModel("5_5",10,.2);
		AEBPTestingDataFrame df = solveWithBothMethods(pairQueryNet.first, pairQueryNet.second, 180,"Ising");
		AEBPRPlotting.plottingTheInterval(df,true,"IsingWeightPointTwo");
	}

	public static void printPlotToFile(String fileName, double maxTime) {
		Pair<TableVariable, TableFactorNetwork> pairQueryNet = importUAIFile(fileName);	
		AEBPTestingDataFrame df = solveWithBothMethods(pairQueryNet.first, pairQueryNet.second, maxTime,fileName.split(".uai")[0]);
		AEBPRPlotting.plottingTheInterval(df,true, fileName.split(".uai")[0] + ".pdf");
		AEBPRPlotting.plottingTheInterval(df,true, fileName.split(".uai")[0] + ".pdf");
	}
}
