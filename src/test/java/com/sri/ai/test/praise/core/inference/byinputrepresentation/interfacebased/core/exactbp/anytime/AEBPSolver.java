package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime;

import static com.sri.ai.util.Util.argmax;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.BiFunction;

import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.box.Box;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.IntensionalConvexHullOfFactors;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.Polytopes;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.ProductPolytope;
import com.sri.ai.praise.core.representation.interfacebased.polytope.core.byexpressiveness.convexhull.Simplex;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.computation.anytime.api.Approximation;
import com.sri.ai.util.rplot.dataframe.AEBPTestingDataFrame;

public class AEBPSolver {
	public static AEBPTestingDataFrame solve(Iterator<? extends Approximation<Factor>> anytimeEBPIterator,
					Variable query, 
					long maximunTimeInSeconds,
					int runNumber, String PGMName,String methodName) {
		
		AEBPTaskSolver solver = new AEBPTaskSolver(anytimeEBPIterator,runNumber, PGMName,methodName,query); 
		
		AEBPTestingDataFrame result = ExecuteSolverForACertainAmountOftime(maximunTimeInSeconds,solver);
        
		return result;
		
	}

	private static AEBPTestingDataFrame ExecuteSolverForACertainAmountOftime(long maximunTimeInSeconds, AEBPTaskSolver solver) {

		ExecutorService executor = Executors.newSingleThreadExecutor();
		Future<AEBPTestingDataFrame> future   = executor.submit(solver);
		AEBPTestingDataFrame result =  null;
		
		try {
            System.out.println("Started..");
            result = future.get(maximunTimeInSeconds, TimeUnit.SECONDS);
            System.out.println("Finished!");
        }
		catch (TimeoutException toe) {
			System.out.println("Timeout occurred, interrupting solver.");
			result = solver.interrupt();
			future.cancel(true);
			try {
				// Wait until the solver shuts down properly from the interrupt
				System.out.println("Waiting for interrupted result");
				result = future.get();
				System.out.println("Finished waiting for interrupted result");
			}
			catch (Throwable t) {
				System.out.println("Finished waiting for interrupted result : "+(t.getMessage() == null ? t.getClass().getName() : t.getMessage()));
			}
		}
		catch (Throwable t) {
            System.out.println("Terminated! : "+(t.getMessage() == null ? t.getClass().getName() : t.getMessage()));
            t.printStackTrace();
        }

        executor.shutdown();
        System.out.println("executor is shutdown:"+executor.isShutdown());
		return result;
	}

	static class AEBPTaskSolver implements Callable<AEBPTestingDataFrame>{
		Iterator<? extends Approximation<Factor>> anytimeEBPIterator;
		int runNumber;
		String pGMName;
		String methodName;
		Variable query;
		//
		boolean interrupted;
		AEBPTestingDataFrame df;
		//
		Box currentApproximation = null;
		
		public AEBPTaskSolver(Iterator<? extends Approximation<Factor>> anytimeEBPIterator, int runNumber,
				String pGMName, String methodName,Variable query) {
			this.anytimeEBPIterator = anytimeEBPIterator;
			this.runNumber = runNumber;
			this.pGMName = pGMName;
			this.methodName = methodName;
			this.query = query;
			//
			interrupted = false;
			df = new AEBPTestingDataFrame();
		}
		public AEBPTestingDataFrame interrupt() {
			interrupted = true;
			return df;
			
		}
		@Override
		public AEBPTestingDataFrame call() throws Exception {
			if(interrupted) {
				return df;
			}
			
			ExecuteAnytimeIteratorAndStoreResultsOnDataFrame();
			
			return df;
		}
		private void ExecuteAnytimeIteratorAndStoreResultsOnDataFrame() {
			int i = 0;
			Double currentTotalTime = .0;
			long startTime = System.currentTimeMillis();
			
			while(!interrupted && anytimeEBPIterator.hasNext()) {
				println(++i);
				Long currentTime = System.currentTimeMillis();
				Polytope result = (Polytope) anytimeEBPIterator.next();
				Pair<ArrayList<Double>, ArrayList<Double>> maxAndMin = printAndReturnMaxAndMinProbabilities(result);
				currentTotalTime = .001*(System.currentTimeMillis() - startTime);
				Double currentPartialTime = .001*(System.currentTimeMillis() - currentTime);
				
				List<Double> differencesBetwenMaxAndMinProba = differenceBetweenLists(maxAndMin.second,maxAndMin.first);
				int indexOfbiggestEdge = differencesBetwenMaxAndMinProba.indexOf(argmax(differencesBetwenMaxAndMinProba, n->n));
				
				if(!(result instanceof IntensionalConvexHullOfFactors) && !(result instanceof Simplex)) {
					result = Polytopes.getEquivalentAtomicPolytopeOn(query, result);
				}
				
				df.addRow(runNumber, i,
						maxAndMin.first.get(indexOfbiggestEdge),
						maxAndMin.second.get(indexOfbiggestEdge),
						currentPartialTime, currentTotalTime, methodName, pGMName);
				println("Time: " +  currentTotalTime);
				println("\tNumber of indices:" + 
								((result instanceof Simplex)? 
										"Simplex" 
										: 
										((IntensionalConvexHullOfFactors)result).getIndices().size()) );
				println("\t\tMin: " + maxAndMin.first);
				println("\t\tMax: " + maxAndMin.second);
				println("\t\t\tIndex: " + indexOfbiggestEdge);
			
			}
			
		}
		private List<Double> differenceBetweenLists(List<Double> max, List<Double> min) {
			if(max.size()!=min.size()) {
				throw new Error();
			}
			Iterator<Double> itMax = max.iterator();
			Iterator<Double> itMin = min.iterator();
			ArrayList<Double> result = new ArrayList<>(max.size());
			while(itMax.hasNext()) {
				
				result.add(roundTo3decimalDigits(itMax.next()-itMin.next()));
			}
			return result;
		}
		private Double roundTo3decimalDigits(Double d) {
			return Double.parseDouble(String.format("%.3f", d));
		}
		
		private Pair<ArrayList<Double>, ArrayList<Double>> printAndReturnMaxAndMinProbabilities(
				Polytope polytope) {
			
			Pair<ArrayList<Double>,ArrayList<Double>> result = new Pair<>(arrayList(-1.,-1.),arrayList(-1.,-1.));
			
			if(polytope instanceof ProductPolytope) {
				polytope = Polytopes.getEquivalentAtomicPolytopeOn(query, (ProductPolytope) polytope);
			}
			if (polytope instanceof IntensionalConvexHullOfFactors) {
				Pair<TableFactor, TableFactor> maxMinProba = getMaxMinProbabilityFromIntensionalConvHull((IntensionalConvexHullOfFactors) polytope);
				
				result = new Pair<>(
						maxMinProba.first.getEntries(),
						maxMinProba.second.getEntries());
			}
			else if(polytope instanceof Simplex) {
				println("simplex.");
				ArrayList<Double> listOfOnes = Util.fill(query.getValues().size(), ()->1.);
				ArrayList<Double> listOfZeros = Util.fill(query.getValues().size(), ()->0.);
				result = new Pair<>(listOfZeros,listOfOnes);
			}
			else {
				throw new Error();
			}
			return result;
		}
		private Pair<TableFactor,TableFactor> getMaxMinProbabilityFromIntensionalConvHull(IntensionalConvexHullOfFactors polytope) {
			
			Box box = Box.boxIt(polytope);
			
			updateCurrentApproximation(box);
			
			TableFactor phiMin = currentApproximation.getPhiMin();
			TableFactor phiMax = currentApproximation.getPhiMax();
			
			
			return new Pair<>(phiMin,phiMax);
			
			/*List<TableFactor> factors = Polytopes.IntensionalConvexHullToListOfFactors(polytope);
			List<TableFactor> normalizedFactors = mapIntoList(factors, f->f.normalize());
			
			ArrayList<TableVariable> variables = mapIntoArrayList(polytope.getFreeVariables(), v->(TableVariable)v);
			
			TableFactor phiMin= new TableFactor(variables);
			TableFactor phiMax= new TableFactor(variables);
			for(ArrayList<Integer> values : in(getCartesianProduct(variables))){
				Map<TableVariable, Integer> map = Util.mapFromListOfKeysAndListOfValues(variables, values);
				List<Double> phiValues = mapIntoList(normalizedFactors, (f)-> f.getEntryFor(map));

				phiMin.setEntryFor(map, Collections.min(phiValues));
				phiMax.setEntryFor(map, Collections.max(phiValues));
			}			
			return new Pair<>(phiMax,phiMin);*/
		}		
		

		private void updateCurrentApproximation(Box p) {
			if(currentApproximation == null) {
				currentApproximation = p;
			}
			else if(p != null) {
				ArrayList<Double> newPhiMinEntries = getMaxOf(p.getPhiMin().getEntries(),currentApproximation.getPhiMin().getEntries());
				ArrayList<Double> newPhiMaxEntries = getMinOf(p.getPhiMax().getEntries(),currentApproximation.getPhiMax().getEntries());
				TableFactor newPhiMin = new TableFactor(p.getPhiMin().getVariables(),newPhiMinEntries);
				TableFactor newPhiMax = new TableFactor(p.getPhiMax().getVariables(),newPhiMaxEntries);
				currentApproximation = new Box(newPhiMin, newPhiMax);
			}
		}
		
		private ArrayList<Double> getMinOf(ArrayList<Double> entries, ArrayList<Double> entries2) {
			ArrayList<Double> result = collectFromOfTwoLists(entries,entries2, (a,b)->Double.min(a, b));
			return result;
		}
		private ArrayList<Double> getMaxOf(ArrayList<Double> entries, ArrayList<Double> entries2) {
			ArrayList<Double> result = collectFromOfTwoLists(entries,entries2, (a,b)->Double.max(a, b));
			return result;
		}

		private <T> ArrayList<T> collectFromOfTwoLists(ArrayList<T> entries, ArrayList<T> entries2,BiFunction<T, T, T> function) {
			if(entries.size() != entries2.size()) {
				return null;
			}
			ArrayList<T> result = new ArrayList<>();
			Iterator<T> it = entries.iterator();
			Iterator<T> it2 = entries2.iterator();
			while(it.hasNext()) {
				result.add(function.apply(it.next(), it2.next()));
			}
			return result;
		}
	}
}


