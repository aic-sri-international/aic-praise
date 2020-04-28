package com.sri.ai.test.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime;

import static com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor.arrayTableFactor;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.Util.round;

import org.junit.jupiter.api.Test;

import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.anytime.rodrigo.AnytimeExactBPSolver;
import com.sri.ai.praise.core.inference.byinputrepresentation.interfacebased.exactbp.fulltime.core.ExactBPSolver;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.FactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.api.Variable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.base.DefaultFactorNetwork;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.base.TableVariable;
import com.sri.ai.praise.core.representation.interfacebased.factor.core.table.core.bydatastructure.arraylist.ArrayTableFactor;
import com.sri.ai.praise.core.representation.interfacebased.polytope.api.Polytope;


public class AnytimeExactBPWithSimplificationTest {

	@Test
	public void test() {
		
		ArrayTableFactor.decimalPlaces = 2;
		
		FactorNetwork factorNetwork;
		Variable query;
		
		TableVariable a = new TableVariable("a", 2);
		TableVariable b = new TableVariable("b", 2);
		TableVariable c = new TableVariable("c", 2);
		TableVariable d = new TableVariable("d", 2);
		
		factorNetwork = new DefaultFactorNetwork(
				arrayTableFactor(
						list(d), 
						(vd) -> 
						vd == 0? 0.5: 0.5),
				arrayTableFactor(
						list(a, d), 
						(va, vd) -> 
						vd == 0? 
								va == 1? 0.8 : 0.2 :  
								va == 1? 0.9 : 0.1),
				arrayTableFactor(
						list(c, d), 
						(vc, vd) -> 
						vd == 0? 
								vc == 1? 0.7 : 0.3 : 
								vc == 1? 0.6 : 0.4),
				arrayTableFactor(
						list(a, b, c), 
						(va, vb, vc) -> vb == va? 1.0 : 0.0)
				);
		
		query = b;
		
		println("Exact: ", new ExactBPSolver().apply(query, factorNetwork));
		
		var it = new AnytimeExactBPSolver().apply(query, factorNetwork);
		
		while (it.hasNext()) {
			var polytope = (Polytope) it.next();
			var normalized = polytope.normalize(list(query));
			println(normalized + ", " + round(normalized.length(), 3));
		}
	}	
}
