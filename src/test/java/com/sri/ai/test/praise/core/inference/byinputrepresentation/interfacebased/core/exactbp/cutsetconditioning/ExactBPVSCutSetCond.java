package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.cutsetconditioning;

import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.anytime.gabriel.TestCases;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.cutsetconditioning.core.CutSetConditioning;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.eager.api.ExactBPNode;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.core.exactbp.eager.core.ExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionExactBP;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.TableVariable;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.rplot.dataframe.ListOfListDataFrame;

/**
 * This class compares exact BP and CUTset conditioning
 * @author gabriel
 *
 */
public class ExactBPVSCutSetCond {
	
	public static <T> Pair<Double,T> solveAndPrint(NullaryFunction<T> func) {
		long startTime = System.currentTimeMillis();
		System.out.println("Started...");
        T result;
		result = func.apply();
        System.out.println("Finished!");
        
        long TotalTime = System.currentTimeMillis() - startTime;
        Double timeInSeconds = TotalTime / 1000.;
        println("result: " + result);
        println("time  : " + timeInSeconds);
        
        return new Pair<Double, T>(timeInSeconds, result);
	}
	
	public static Pair<Double, Factor> solveWithExactBP(Expression query, ExpressionFactorNetwork factorNetwork) {
		ExactBPNode<Variable,Factor> exactBP = new ExpressionExactBP(query, factorNetwork);
		Pair<Double, Factor> result = solveAndPrint(exactBP);
		return result;
	}
	public static Pair<Double, Factor> solveWithExactBP(TableVariable query, TableFactorNetwork factorNetwork) {
		ExactBPNode<Variable,Factor> exactBP = new ExactBP(query, factorNetwork);	
		Pair<Double, Factor> result = solveAndPrint(exactBP);
		return result;
	}
	
	public static Pair<Double, Factor> solveWithCutSetCond(Expression query, ExpressionFactorNetwork factorNetwork) {
		CutSetConditioning exactBP = new CutSetConditioning(new DefaultExpressionVariable(query), factorNetwork);
		Pair<Double, Factor> result = solveAndPrint(exactBP);
		return result;
	}
	public static Pair<Double, Factor> solveWithCutSetCond(TableVariable query, TableFactorNetwork factorNetwork) {
		CutSetConditioning exactBP = new CutSetConditioning(query, factorNetwork);	
		Pair<Double, Factor> result = solveAndPrint(exactBP);
		return result;
	}
	
	public static void main(String[] args) {
		
		//toDfIsingModel();

		toDfAlarm();	
	}

	private static void toDfAlarm() {
		ListOfListDataFrame df = new ListOfListDataFrame(
				Util.list(
						"Time","PTrue",
						"Inference Method","NetworkType","QueryName"), 
				0, 2, 3);
		
		
		List<TableFactor> alarm = TestCases.TableFactorALARM();
		TableFactorNetwork alarmNet = new TableFactorNetwork(alarm);
		
		LinkedHashSet<TableVariable> variablesInAlarm = new LinkedHashSet<>();
		for(TableFactor f : alarm) {
			variablesInAlarm.addAll(f.getVariables());
			//println(f.getVariables());
		}
		
		ArrayList<TableVariable> listVarInAlarm = new ArrayList<>(variablesInAlarm);
		println(listVarInAlarm);

		ArrayList<Boolean> computeOrNot = Util.fill(37, true);

		// 0    1    2    3    4    5    6    7    8    9    10    11    12    13    14    15    16    17
		// NOK  NOK  OK   OK
		computeOrNot.set(0, false);
		computeOrNot.set(1, false);
		computeOrNot.set(2, true);//set true
		computeOrNot.set(3, true);
		computeOrNot.set(4, true);
		computeOrNot.set(5, true);
		
		int i = 0;
		for(TableVariable v : listVarInAlarm) {

			println("---------- var = " + v + " ----------"  );
			testAndPrintToDataSet(v, alarmNet, 5, df, "alarm", computeOrNot.get(i));
			i++;
			if ( i >5) {
				break;
			}
		}
		
		df.printToCsv("exactBPVsCutsetCond-alarm.csv");
	}

	private static void toDfIsingModel() {
		ListOfListDataFrame df = new ListOfListDataFrame(
				Util.list(
						"Time","PTrue",
						"Inference Method","NetworkType","QueryName"), 
				0, 2, 3);
		
		ArrayList<Boolean> computeOrNot = Util.fill(10, true);
		
		computeOrNot.set(6, false);
		computeOrNot.set(7, false);
		computeOrNot.set(8, false);
		
		for(int i = 1; i <= 7;i++ ) {
			println("---------- i = " +i+ " ----------"  );
			@SuppressWarnings("unchecked")
			List<TableFactor> grid = (List<TableFactor>) TestCases.isingModelGridWithWeigthsAndPotetialNormalyDistributed(i, .1, true);
			
			TableVariable query = TestCases.getTableVariableByName("A_"+i/2+"_"+i/2, grid);
			TableFactorNetwork factorNetwork = new TableFactorNetwork(grid);
			
			testAndPrintToDataSet(query, factorNetwork, 5,df,"IsingGridPotentialNormallyDist-It="+i,computeOrNot.get(i));
		}
		
		

		df.printToCsv("exactBPVsCutsetCond-ising.csv");
	}

	private static void testAndPrintToDataSet(
			TableVariable query, 
			TableFactorNetwork factorNetwork,
			int nIterations,
			ListOfListDataFrame df, 
			String NetName,
			boolean testCutsetCond) {
		for (int j = 0; j < nIterations; j++) {	
			println("solveWithExactBP");
			Pair<Double, Factor> p = solveWithExactBP(query, factorNetwork);
			df.addRow(
					p.first,
					((TableFactor)p.second).getEntries().get(0),
					"ExactBP",
					NetName,
					query.toString());
			println("solveWithCutSetCond");
			
			System.out.print(".");
			if(testCutsetCond) {
				p = solveWithCutSetCond(query, factorNetwork);
				df.addRow(
						p.first,
						((TableFactor)p.second).getEntries().get(0),
						"CutSetCond",
						NetName,
						query.toString());
			}
			else {
				df.addRow(
						-1.,-1.,
						"CutSetCond",
						NetName,
						query.toString());
			}
		}
	}
	
	private static void testAndPrintToDataSet(
			TableVariable query, 
			TableFactorNetwork factorNetwork,
			int nIterations,
			ListOfListDataFrame df, 
			String NetName) {
		testAndPrintToDataSet(query, factorNetwork, nIterations, df, NetName,true);	
		
	}
	
}
