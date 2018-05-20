package com.sri.ai.praise.inference.representation.expression;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.praise.model.v1.imports.uai.UAIUtil.constructGenericTableExpressionUsingEqualities;
import static com.sri.ai.praise.model.v1.imports.uai.UAIUtil.convertGenericTableToInstance;
import static com.sri.ai.util.Util.mapIntoSet;
import static com.sri.ai.util.Util.println;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.UniquelyNamedConstantIncludingBooleansAndNumbersPredicate;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.praise.inference.FactorsAndTypes;
import com.sri.ai.praise.inference.representation.api.Factor;
import com.sri.ai.praise.lang.grounded.common.FunctionTable;
import com.sri.ai.praise.model.v1.imports.uai.UAIFactorsAndTypes;
import com.sri.ai.praise.model.v1.imports.uai.UAIModel;
import com.sri.ai.praise.model.v1.imports.uai.UAIModelReader;
import com.sri.ai.util.base.IdentityWrapper;

public class UAIModelToExpressionFactorNetwork {
	public static ExpressionFactorNetwork convert(UAIModel uaiModel) {
		return convert(uaiModel,null);
	}
	
	public static ExpressionFactorNetwork convert(UAIModel uaiModel,Theory theory) {
		List<Expression> factorsRepresentedAsExpressions = 
				createListOfExpressionsrepresentingTheFactorsFromAUAIModel(uaiModel);
		
		if (theory ==null) {
			theory =
					new CompoundTheory(
							new EqualityTheory(false, true),
							new DifferenceArithmeticTheory(false, true),
							new LinearRealArithmeticTheory(false, true),
							new PropositionalTheory());
		}
		
		//Add variables in the factors to the context...
		FactorsAndTypes factorsAndTypes = new UAIFactorsAndTypes(factorsRepresentedAsExpressions, uaiModel);
		//Context 
		Context context = fillingContext(theory, factorsAndTypes);
		
		ExpressionFactorNetwork result = new ExpressionFactorNetwork(factorsRepresentedAsExpressions, context);
		return result;
	}

	private static Context fillingContext(Theory theory, FactorsAndTypes factorsAndTypes) {
		LinkedHashMap<String, String> mapFromRandomVariableNameToTypeName = new LinkedHashMap<>(factorsAndTypes.getMapFromRandomVariableNameToTypeName());
		
		Map<String, String> mapFromSymbolNameToTypeName= new LinkedHashMap<>(mapFromRandomVariableNameToTypeName);
		mapFromSymbolNameToTypeName.putAll(factorsAndTypes.getMapFromNonUniquelyNamedConstantNameToTypeName());
		mapFromSymbolNameToTypeName.putAll(factorsAndTypes.getMapFromUniquelyNamedConstantNameToTypeName());
		
		Map<String, String> mapFromCategoricalTypeNameToSizeString = new LinkedHashMap<>(factorsAndTypes.getMapFromCategoricalTypeNameToSizeString());
		
		Set<Expression> uniquelyNamedConstants = mapIntoSet(factorsAndTypes.getMapFromUniquelyNamedConstantNameToTypeName().keySet(), Expressions::parse);
		uniquelyNamedConstants.add(parse("and"));
		uniquelyNamedConstants.add(parse("not"));
		uniquelyNamedConstants.add(parse("'if . then . else .'"));
		
		UniquelyNamedConstantIncludingBooleansAndNumbersPredicate isUniquelyNamedConstantPredicate = 
				new UniquelyNamedConstantIncludingBooleansAndNumbersPredicate(uniquelyNamedConstants);
		
		LinkedList<Type> additionalTypes = new LinkedList<Type>(theory.getNativeTypes()); // add needed types that may not be the type of any variable
		additionalTypes.addAll(factorsAndTypes.getAdditionalTypes());
		
		Context context =
				GrinderUtil.makeContext(
					mapFromSymbolNameToTypeName,
					mapFromCategoricalTypeNameToSizeString,
					additionalTypes,
					isUniquelyNamedConstantPredicate,
					theory
				);
		return context;
	}

	private static List<Expression> createListOfExpressionsrepresentingTheFactorsFromAUAIModel(UAIModel uaiModel){
/*	This is similar to the way They do in the UAIMARSolver.
 *  However, I think it is easier to understand in my way(below, uncommented). 
 *  This code used the fact that one same factor can appear many times (e.g. to factors phi_i and phi_j 
 *  having the same table). 
 *  The UAI model Contains the following maps
 *   	-1 map that links a index to it's table (tableInstanceIdxToTable)
 *   	-one map that does the inverse operation (uniqueTableToTableInstanceIdxs). 
 *   Because a table can be used more than once, it links a FunctionTable to a list of indexes
 *   	-one map that each table to a unique index (sort of a id of each unique table)(getUniqueFunctionTable).
 *   My code only uses the 1st one, while the commented one uses the 2nd and third tables
 *   	
  		List<Expression> factorsRepresentedAsExpressions = new ArrayList<>();
		for (int i = 0; i < uaiModel.numberUniqueFunctionTables(); i++) {
			FunctionTable table = uaiModel.getUniqueFunctionTable(i);
			Expression genericTableExpression;
			genericTableExpression = constructGenericTableExpressionUsingEqualities(table);
			
			for (int tableIdx : uaiModel.getTableIndexes(i)) {
				Expression instanceTableExpression = 
						convertGenericTableToInstance(table, genericTableExpression, uaiModel.getVariableIndexesForTable(tableIdx));
				factorsRepresentedAsExpressions.add(instanceTableExpression);
			}	
		}
		return factorsRepresentedAsExpressions;*/
		
		List<Expression> factorsRepresentedAsExpressions = new ArrayList<>();
		for (int i = 0; i < uaiModel.numberTables(); i++) {
			FunctionTable table = uaiModel.getTable(i);
			Expression genericTableExpression = 
					constructGenericTableExpressionUsingEqualities(table);
			Expression instanceTableExpression = 
					convertGenericTableToInstance(table, genericTableExpression, uaiModel.getVariableIndexesForTable(i));
			factorsRepresentedAsExpressions.add(instanceTableExpression);
		}
		return factorsRepresentedAsExpressions;
	}
	
	public static void main(String[] args) {
		try {
			
			// Importing the file and reading it
			FileReader modelFile = new FileReader(new File("").getAbsolutePath()+"/UAITests/BN_0.uai" );
			UAIModel model = UAIModelReader.read(modelFile);
			
			// Converting the network
			ExpressionFactorNetwork network = convert(model, null);
			
			// Printing the factors
			for(IdentityWrapper<Factor> fwrapped : network.getAs()) {
				ExpressionFactor f = (ExpressionFactor) fwrapped.getObject();
				println(f);
			}
			/*// This seems to be OK! But when we analyze the connections between the factors:
			
			
			IdentityWrapper<Factor> f = network.getAs().iterator().next();
			
			println("Printing one of the factors of the network:\n\t "+f);
			println("Printing this factor's connections:\n\t" + network.getBsOfA(f));
			println("This shows that there is something wrong\n"
					+ "In fact the connections in the graph are made based on the 'freeVariables' of a factor");
			println("freeVariables of f: "+Expressions.freeVariables((ExpressionFactor)f.getObject(),((ExpressionFactor)f.getObject()).getContext()));
			
			
			println("\nWe can check that those 'abnomalies' are indeed variables on the network:");
			for(Variable v:network.getBs()) {
				System.out.print(v + ", ");
			}*/
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		
	}
}
