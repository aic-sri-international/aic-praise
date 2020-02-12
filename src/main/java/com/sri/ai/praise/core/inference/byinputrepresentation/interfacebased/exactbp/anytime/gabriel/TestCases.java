package com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.gabriel;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.library.FunctorConstants.IF_THEN_ELSE;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.println;
import static java.lang.Double.max;
import static java.lang.Math.exp;
import static java.lang.Math.log;
import static java.lang.Math.round;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.BiFunction;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.UAIModel;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.parsing.UAIEvidenceReading;
import com.sri.ai.praise.core.representation.classbased.table.core.uai.parsing.UAIModelReader;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Factor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.api.ExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionFactor;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.DefaultExpressionVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.ExpressionFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.expression.core.UAIModelToExpressionFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * 
 * TODO: either change my doc or add some compiler that supports equations....
 * 
 * @author gabriel
 */
public class TestCases {

	/**
	 * An Ising model is a N dimensional lattice (like a N-dimensional grid), where each node interact with its nearest neighbors. 
	 * Each node can assume the values +1 or -1, and has an index "i" associated to its position. We usually represent the node
	 * at position i by <math>\sigma_i</math>. The indexes are usually given so that sigma 0 is in teh center of the hyper-cube
	 * <p>
	 * If N = 2, the Ising model is simply a squared grid. Below we represent a 3X3 s dimension Ising model
	 *  <p>
	 *  
	 *  <table style="width:10%">
  	 * <tr>
  	 *      <th>sig2</th><th>--</th><th>sig3 </th><th>--</th><th>sig4</th>
  	 * </tr>
  	 * <tr>
  	 * 		<th>|</th><th>		   </th><th>|</th><th>	 	 </th><th>|</th>  
  	 * </tr>
  	 * <tr>
  	 *   	<th>sig-1</th><th>--</th><th>sig0</th><th>--</th><th>sig1</th>
  	 * </tr>
  	 * <tr>
  	 *   	<th>|</th><th>		   </th><th>|</th><th>	 	 </th><th>|</th>
  	 * </tr>
  	 * <tr>
  	 * 	 	<th>sig-4</th><th>--</th><th>sig-3</th><th>--</th><th>sig-2</th>
  	 * </tr>
  	 * </table>
	 *
	 *  <p>
	 * If N = 1, the model is a line (sig1 -- sig2 -- sig3 -- sig4 -- sig5 ...)<p>
	 * 
	 * we define <math>\sigma = (\sigma_1,\sigma_2,...,\sigma_n)</math>.<p>
	 * 
	 * The Ising model is represented by the following equation:<p>
	 * 
	 * :<math>\tilde{P}(\sigma) = exp(-\beta H(\sigma)) </math><p>
	 * 
	 * Where beta is the POTENTIAL<p>
	 * :<math>H(\sigma) = - \sum_{\langle i~j\rangle} J_{ij} \sigma_i \sigma_j -\mu \sum_{j} h_j\sigma_j</math>
	 * 
	 * Simplifications usually consider  <math> J_{ij} = \mu_j = 1 </math>. That way, the grid model can be represented in the following way: <p>
	 * 
	 * :<math>\tilde{P}(\sigma) = (\prod_{<ij>}\phi(\sigma_i,\sigma_j))(\prod_i\phi'(\simga_i)) </math>
	 * 
	 * Where <ij> mean the set of (i,j) that are directly neighbors, and the factors are defined as follows:<p>
	 * 
	 * :<math>\phi(X,Y)= exp(\beta X Y),\phi'(X) = exp(h X) </math>
	 * 
	 * If we don't consider the simplification, the network correspond to a markov random field  grid with arbitrary factors   
	 * 
	 * <p>-----------------------------------------------------------------------------------<p>
	 * Important results about the Ising model:<p>
	 * 
	 * - Suppose we take as evidence that all the nodes in the frontier of the lattice (surface of the hypercube) 
	 * are equal +1. There exists a <math>\beta_c</math>, such that, 
	 * <math>P(\sigma_0 = True) > \alpha > 0.5</math> for ARBITRARYLY LARGE number of odes on the lattice   <p>
	 * 
	 * - This means that AEBP is going to converge to an interval of size 2*alpha, and then suddenly drops to \ero in the frontier;
	 * 
	 * @param gridSize : gridSize X gridSize is the dimension of the grid
	 * @param potential : Beta, Inverse temperature
	 * @param weight: Theta
	 * @return 
	 */
	public static List<? extends Factor> gridModelWithRandomFactors(int gridSize, boolean TableOrExpression) {
		Random randomGenerator = new Random();

		BiFunction<Pair<Integer, Integer>, Pair<Integer, Integer>, ArrayList<Double>> entries =
				(i,j)->arrayList(
						0.001*randomGenerator.nextInt(1000),
						0.001*randomGenerator.nextInt(1000),
						0.001*randomGenerator.nextInt(1000),
						0.001*randomGenerator.nextInt(1000)
						);
		if(TableOrExpression) {
			ArrayList<ArrayTableFactor> result = tableFactorIsingModel(gridSize, entries, (i)->null);
			return  result;
		}
		else {
			ArrayList<ExpressionFactor> result = expressionFactorIsingModel(gridSize, entries,(i)->null);
			return result;
		}
	}

	/**
	 *  \tilde(P)(\sigma) = \frac{1}{Z} exp(\sum_{i}\theta_i \sigma_i + \sum_{<< i j >>}J_{i,j}\sigma_i\sigma_j),
	 * <p>
	 * where \sigma_i \in \{+1,-1\}, J_{i,j} = J,\theta_{i} = \theta  
	 * @param beta
	 */
	public static List<? extends Factor> isingModelGridWithWeigthsAndPotetialFixed(int gridSize,double J, double theta, boolean TableOrExpression) {
		ArrayList<Double> parwiseEntries = arrayList(exp(J),exp(-1.*J),exp(-1.*J),exp(J));
		ArrayList<Double>  singleEntries = (theta == 0.)? 
													null
													:
													arrayList(exp(theta),exp(-theta));
		if(TableOrExpression) {
			ArrayList<ArrayTableFactor> result = tableFactorIsingModel(gridSize,
					(i,j)-> parwiseEntries ,(i)->singleEntries);
			return  result;
		}
		else {
			ArrayList<ExpressionFactor> result = expressionFactorIsingModel(gridSize,
					(i,j)->parwiseEntries,(i)->singleEntries);
			return result;
		}
	}
	
	 /*tilde(P)(\sigma) = \frac{1}{Z} exp(\sum_{i}\theta_i \sigma_i + \sum_{<< i j >>}J_{i,j}\sigma_i\sigma_j),
	  * <p>
	  * where \sigma_i \in \{+1,-1\}, J_{i,j},\theta{i} ~ N(0,\beta^2)  
	  * @param beta
	  */
	public static List<? extends Factor> isingModelGridWithWeigthsAndPotetialNormalyDistributed(int gridSize,double beta, boolean TableOrExpression) {
		Function<Double,ArrayList<Double>> JPotentialEntries = (J) -> arrayList(exp(J),exp(-1.*J),exp(-1.*J),exp(J));
		Function<Double,ArrayList<Double>> thetaPotentialEntries =   (theta) -> arrayList(exp(theta),exp(-1.*theta),exp(-1.*theta),exp(theta));
		
		Random gen = new Random();
		
		BiFunction<Pair<Integer, Integer>, Pair<Integer, Integer>, ArrayList<Double>> parwiseEntries =
				(i,j) -> JPotentialEntries.apply(gen.nextGaussian()*beta);
		Function<Pair<Integer, Integer>, ArrayList<Double>> singleEntries = 
				(i) -> thetaPotentialEntries.apply(gen.nextGaussian());
		if(TableOrExpression) {
			ArrayList<ArrayTableFactor> result = tableFactorIsingModel(gridSize,
					 parwiseEntries,singleEntries );
			return  result;
		}
		else {
			ArrayList<ExpressionFactor> result = expressionFactorIsingModel(gridSize,
					parwiseEntries,singleEntries);
			return result;
		}
	}
			
	private static ArrayList<ArrayTableFactor> tableFactorIsingModel(int gridSize,
			BiFunction<Pair<Integer, Integer>, Pair<Integer,Integer>, ArrayList<Double>> pairwiseFactorentries,
			Function<Pair<Integer,Integer>,ArrayList<Double>>singleVariableFactorEntries) {
		ArrayList<ArrayList<TableVariable>> variables = new ArrayList<>();
		for (int i = 0; i < gridSize; i++) {
			ArrayList<TableVariable> col = new ArrayList<>();
			variables.add(col);
			for (int j = 0; j < gridSize; j++) {
				col.add(j,new TableVariable("A_"+i+"_"+j, 2));
			}
		}	
		
		ArrayList<ArrayTableFactor> result = new ArrayList<>();
		for(int i = 0; i < gridSize-1; i++) {
			for (int j = 0; j < gridSize; j++) {
				result.add(
						new ArrayTableFactor(
								arrayList(variables.get(i).get(j),variables.get(i+1).get(j)),
								pairwiseFactorentries.apply(new Pair<>(i,j), new Pair<>(i+1,j))
								)
						);
			}
		}
		for(int i = 0; i < gridSize; i++) {
			for (int j = 0; j < gridSize-1; j++) {
				result.add(
						new ArrayTableFactor(
								arrayList(variables.get(i).get(j),variables.get(i).get(j+1)),
								pairwiseFactorentries.apply(new Pair<>(i,j), new Pair<>(i,j+1))
								)
						);
			}
		}
		if(!(singleVariableFactorEntries.apply(new Pair<>(0,0)) == null)) {
			for(int i = 0; i < gridSize; i++) {
				for (int j = 0; j < gridSize; j++) {
					result.add(
							new ArrayTableFactor(
									arrayList(variables.get(i).get(j)),
									singleVariableFactorEntries.apply(new Pair<>(i,j))
									)
							);
				}
			}
		}
		return result;
	}
	
	private static ArrayList<ExpressionFactor> expressionFactorIsingModel(int gridSize,
			BiFunction<Pair<Integer, Integer>, Pair<Integer,Integer>, ArrayList<Double>> pairwiseFactorentries,
			Function<Pair<Integer,Integer>,ArrayList<Double>>singleVariableFactorEntries) {
		ArrayList<ArrayList<ExpressionVariable>> variables = new ArrayList<>();
		Context context = new TrueContext(new CommonTheory());
		
		for (int i = 0; i < gridSize; i++) {
			ArrayList<ExpressionVariable> col = new ArrayList<>();
			variables.add(col);
			for (int j = 0; j < gridSize; j++) {
				ExpressionVariable v =DefaultExpressionVariable.expressionVariable(makeSymbol("A_"+i+"_"+j)); 
				col.add(j,v);
				makeSymbol("A_"+i+"_"+j);
				context = context.extendWithSymbolsAndTypes(v,parse("Boolean"));
			}
		}

		ArrayList<ExpressionFactor> result = new ArrayList<>();
		for(int i = 0; i < gridSize-1; i++) {
			for (int j = 0; j < gridSize; j++) {
//				Expression entry1 = Expressions.makeSymbol(randomGenerator.nextInt(10000)*1./10000.);
				
				ArrayList<Double> entryList = pairwiseFactorentries.apply(new Pair<>(i,j), new Pair<>(i+1,j));
				
				Expression entry1 = makeSymbol(entryList.get(0));
				Expression entry2 = makeSymbol(entryList.get(1));
				Expression entry3 = makeSymbol(entryList.get(2));
				Expression entry4 = makeSymbol(entryList.get(3));

				Expression X = variables.get(i).get(j);
				Expression Y = variables.get(i+1).get(j);
				
				result.add(new DefaultExpressionFactor(
						apply(IF_THEN_ELSE,apply(EQUAL,X,makeSymbol("true")), 
								apply(IF_THEN_ELSE,apply(EQUAL,Y,makeSymbol("true")),entry1,entry2), 
								apply(IF_THEN_ELSE,apply(EQUAL,Y,makeSymbol("true")),entry3,entry4)),
						context));
			}
		}
		for(int i = 0; i < gridSize; i++) {
			for (int j = 0; j < gridSize-1; j++) {

				ArrayList<Double> entryList = pairwiseFactorentries.apply(new Pair<>(i,j), new Pair<>(i,j+1));
				
				Expression entry1 = makeSymbol(entryList.get(0));
				Expression entry2 = makeSymbol(entryList.get(1));
				Expression entry3 = makeSymbol(entryList.get(2));
				Expression entry4 = makeSymbol(entryList.get(3));
				
				Expression X = variables.get(i).get(j);
				Expression Y = variables.get(i).get(j+1);
				
				result.add(new DefaultExpressionFactor(
						apply(IF_THEN_ELSE,apply(EQUAL,X,makeSymbol("true")), 
								apply(IF_THEN_ELSE,apply(EQUAL,Y,makeSymbol("true")),entry1,entry2), 
								apply(IF_THEN_ELSE,apply(EQUAL,Y,makeSymbol("true")),entry3,entry4)),
						context));
			}
		}
		if(!(singleVariableFactorEntries.apply(new Pair<>(0,0)) == null)) {
			for(int i = 0; i < gridSize; i++) {
				for (int j = 0; j < gridSize; j++) {
					ArrayList<Double> entryList = singleVariableFactorEntries.apply(new Pair<>(i,j));
					
					Expression entry1 = makeSymbol(entryList.get(0));
					Expression entry2 = makeSymbol(entryList.get(1));
					
					Expression X = variables.get(i).get(j);
					
					result.add(new DefaultExpressionFactor(
							apply(IF_THEN_ELSE,apply(EQUAL,X,makeSymbol("true")), 
									entry1,
									entry2),
							context));
				}
			}
		}
		return result;
	}
	
	/**
	 * generates the alarm network
	 * @return UAIModel
	 */
	public static ArrayList<ArrayTableFactor> TableFactorALARM(){
		UAIModel model = retrievaALARMUaiFile();
		return makeListOfTableFactorsFromUAIModel(model);
	}

	public static ArrayList<ExpressionFactor> ExpressionFactorALARM(){
		UAIModel model = retrievaALARMUaiFile();
		return makeListOfExpressionFactorsFromUAIModel(model);
	}
	
	private static UAIModel retrievaALARMUaiFile() {
		return makeUAIModelFromUAIFile("alarm", "alarm.uai");
	}
	
	/**
	 * Generates a Promedas network from Alex Ihler repository
	 * 
	 */

	/**
	 * A Bolztmann machine is a ring where there is a pairwise connection between every node and all the others.
	 * Plus, every node is binary with values 1 and 0.
	 * 
	 * This is model is not suitable for an incremental inference, since any result different than Simplex(Q) would demand
	 * exoloring all the nodes. 
	 * 
	 * Instead, as a toy problem, we could test a "Bolztmann machine ring", where each node is only connected to it's neighbors.
	 * 
	 */
	public static void boltzmanMachineRing() {
		//TODO	return a set of Factors
	}

		//-----------------
	
	public static ArrayList<ExpressionFactor> getListOfExpressionFactors(String folderName, String modelNameWithoutDotUAIExtension){
		return makeListOfExpressionFactorsFromUAIModel(makeUAIModelFromUAIFile(folderName, modelNameWithoutDotUAIExtension));
	}

	public static ArrayList<ArrayTableFactor> makeListOfTableFactorsFromUAIModel(UAIModel uaiModel) {
		TableFactorNetwork net = new TableFactorNetwork(uaiModel);
		ArrayList<ArrayTableFactor> result = mapIntoArrayList(net.getAs(), (fwrapper)->(ArrayTableFactor)fwrapper.getObject());
		return result;
	}
	
	public static ArrayList<ExpressionFactor> makeListOfExpressionFactorsFromUAIModel(UAIModel model) {
		ExpressionFactorNetwork net = UAIModelToExpressionFactorNetwork.convert(model);
		ArrayList<ExpressionFactor> result = mapIntoArrayList(net.getAs(), (fwrapper)->(ExpressionFactor)fwrapper.getObject());
		return result;
	}

	public static ArrayList<File> retrieveUAIFilesInFolder(String folderName) {
		File directory = new File("UAITests/" + folderName);
		File[] directoryListing = directory.listFiles();
		
		ArrayList<File> result = new ArrayList<>();
		
		if (directoryListing != null) {
		    for (File child : directoryListing) {
		    	if(child.getName().endsWith(".uai")) {
		    		result.add(child);
		    	}
		    }
		  }
		
		return result;
	}
	
	private static UAIModel makeUAIModelFromUAIFile(String folderName,String fileName) {
		if(!fileName.endsWith(".uai")) {
			fileName = fileName + ".uai";
		}
		
		try {
			FileReader modelFile    = new FileReader(new File("").getAbsolutePath()+"/UAITests/"+ folderName + "/" + fileName);
			
			FileReader evidenceFIle = new FileReader(new File("").getAbsolutePath()+"/UAITests/"+ folderName + "/" + fileName + ".evid");
			
			UAIModel model = UAIModelReader.read(modelFile);
			UAIEvidenceReading.read(evidenceFIle,model);
			
			return model;
		} catch (FileNotFoundException e) {
			FileReader modelFile;
			try {
				modelFile = new FileReader(new File("").getAbsolutePath()+"/UAITests/"+ folderName + "/" + fileName);
				return UAIModelReader.read(modelFile);
			} catch (FileNotFoundException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (IOException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	
	
	//------ Trees
	private static TableVariable[][] treeVariables(int depth, int childrenPerNode,int cardinality){
		int nCol = pow(childrenPerNode, depth);
		TableVariable[][] result = new TableVariable[depth][nCol];
		
		for (int i = 0; i < depth; i++) {
			for (int j = 0; j < pow(childrenPerNode,i); j++) {
				result[i][j]= new TableVariable("A_"+i+"_"+j, cardinality);
			}
		}
		return result;
	}

	private static int pow(int a, int b) {
		return BigInteger.valueOf(a).pow(b).intValue();
	}
	
	private static List<ArrayTableFactor> tree(int depth, int childrenPerNode,Function<ArrayList<TableVariable>, ArrayList<Double>> entryGen,int cardinality){
		TableVariable[][] treeVars = treeVariables(depth, childrenPerNode, cardinality);
		
		List<ArrayTableFactor> result = new ArrayList<>();
		for (int i = 0; i < depth; i++) {		
			for (int j = 0; j < pow(childrenPerNode,i); j++) {
				ArrayList<TableVariable> vars =
						listWithVariableNodeAndItschildren(depth, childrenPerNode, treeVars, i,j);
				
				ArrayTableFactor newNode = new ArrayTableFactor(vars,entryGen.apply(vars));
				result.add(newNode);
			}
		}
		return result;
	}

	private static ArrayList<TableVariable> listWithVariableNodeAndItschildren(int depth, int childrenPerNode,
			TableVariable[][] treeVars, int i, int j) {
		ArrayList<TableVariable> vars = new ArrayList<>();
		vars.add(treeVars[i][j]);
		if(i<depth-1) {
			for (int k = childrenPerNode*j; k < childrenPerNode*(j+1); k++) {
				vars.add(treeVars[i+1][k]);
			}
		}
		return vars;
	}
	
	public static List<ArrayTableFactor> treeWithFixedEntries(int depth, int childrenPerNode,ArrayList<Double> entry,ArrayList<Double> entryLeaf){
		Function<ArrayList<TableVariable>, ArrayList<Double>> entryGen = (l) -> (l!=null && l.size() == 1)? entryLeaf : entry;
		int cardinality = (int) round(log(entry.size())/log(childrenPerNode+1));
		List<ArrayTableFactor> result = tree(depth, childrenPerNode, entryGen,cardinality);
		return result;
	}
	
	public static List<ArrayTableFactor> treeWithRandomEntries(int depth, int childrenPerNode,int cardinality,Function<Random, Double> randomGen){
		Random r = new Random();
		
		Function<ArrayList<TableVariable>, ArrayList<Double>> entryGen = (l) -> Util.fill(pow(cardinality,l.size()),
																									()-> randomGen.apply(r));
		List<ArrayTableFactor> result = tree(depth, childrenPerNode, entryGen,cardinality);
		return result;
	}
	public static List<ArrayTableFactor> treeWithUniformlyRandomEntries(int depth, int childrenPerNode,int cardinality){
		Function<Random, Double> randomGen = (r)-> r.nextInt(1000)*.001;
		return treeWithRandomEntries(depth, childrenPerNode, cardinality, randomGen);
	}
	
	public static List<ArrayTableFactor> treeWithGaussianRandomEntries(int depth, int childrenPerNode,int cardinality, Double mean,Double standardDeviation){
		Function<Random, Double> randomGen = (r)-> max(.00001,mean + r.nextGaussian()*standardDeviation);
		return treeWithRandomEntries(depth, childrenPerNode, cardinality, randomGen);
	}
	public static List<ArrayTableFactor> treeWithExponentialUniformRandomEntries(int depth, int childrenPerNode,int cardinality){
		Function<Random, Double> randomGen = (r)-> exp(r.nextInt(1000)*.001 - .5);
		return treeWithRandomEntries(depth, childrenPerNode, cardinality, randomGen);
	}
	
	public static List<ArrayTableFactor> treeWithExponentialGaussianRandomEntries(int depth, int childrenPerNode,int cardinality, Double mean,Double standardDeviation){
		Function<Random, Double> randomGen = (r)-> exp(mean + r.nextGaussian()*standardDeviation);
		return treeWithRandomEntries(depth, childrenPerNode, cardinality, randomGen);
	}
	
	
	public static void main(String[] args) {
		
		List<ArrayTableFactor> fact = treeWithFixedEntries(5, 2, arrayList(1.,2.,3.,4.,5.,6.,7.,8.),arrayList(1.,1.));
		for(ArrayTableFactor f : fact) {
			println(f);
		}
		
		fact = treeWithGaussianRandomEntries(5, 2, 2, 0., 3.);
		for(ArrayTableFactor f : fact) {
			println(f);
		}
		/*File file = retrieveUAIFilesInFolder("promedas").get(0);
		String name = file.getName();
		println(name);
		ArrayList<TableFactor> factors = getListOfTableFactors("promedas", name);
		for(TableFactor f : factors) {
			println(f);
		}
		
		try {
			FileReader modelFile    = new FileReader(file);

			UAIModel model = UAIModelReader.read(modelFile);
			println(model.getEvidence());
			ArrayList<TableFactor> factors2 = uaiModelToListOfTableFactors(model );
			for(TableFactor f : factors2) {
				println(f);
			}
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			println("ed");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			println("ed");
		}
		*/
	}
	
	public static TableVariable getTableVariableByName(String queryName,List<ArrayTableFactor> grid){
		
		TableVariable query = null;
		for(ArrayTableFactor f : grid) {
			for(TableVariable v : f.getVariables()) {
				if(v.getName().equals(queryName) ){
					query = v;
					//Util.println("Query not null");
				}
			}
			//Util.println(f.getVariables());	
		}
		return query;
	}
	
}

