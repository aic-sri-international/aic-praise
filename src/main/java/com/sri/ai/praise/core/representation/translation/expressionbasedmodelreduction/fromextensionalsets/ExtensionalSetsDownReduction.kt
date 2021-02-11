package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.fromextensionalsets

import com.sri.ai.expresso.api.Expression
import com.sri.ai.expresso.helper.Expressions
import com.sri.ai.expresso.type.IntegerInterval
import com.sri.ai.grinder.library.Equality
import com.sri.ai.grinder.library.FunctorConstants
import com.sri.ai.grinder.library.FunctorConstants.EQUAL
import com.sri.ai.grinder.library.boole.And
import com.sri.ai.grinder.library.boole.Or
import com.sri.ai.grinder.library.set.Sets
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.base.AbstractExpressionBasedModelDownReduction

/**
 * A class representing the down-reduction from an {@link ExpressionBasedModel}
 * with set operators <code>in</code>, <code>union</code>, <code>intersection</code>, <code>\</code>,
 * <code>\<</code>, <code>\></code>, <code>\<=</code>, <code>\>=</code>.
 * <p>
 * For example, <code>x in {1, 2, y, z}</code>
 * is translated to <code>x = 1 or x = 2 or x = y or x = z<code>.
 * <p>
 * Set subtraction, union, intersection are also supported:
 * <code>x in (Set1 \ Set2)</code> becomes <code>x in Set1 and x not in Set2</code>,
 * and analogous for intersection and union.
 * <p>
 * <code>Set1 <= Set2</code> becomes a conjunction of inclusions for each element in Set1.
 * Set equality becomes <= and >=. Disequality is the negation.
 * Strict inclusion is inclusion and disequality.
*/
class ExtensionalSetsDownReduction(expressionBasedModel: ExpressionBasedModel)
    : AbstractExpressionBasedModelDownReduction(expressionBasedModel) {

    // no types are affected by this transformation
    // note that it does not presuppose set-typed variables.
    override fun processTypeName(typeName: String) = typeName

    override fun translateRootOf(subExpression: Expression) = translateSetExpressions(subExpression)

    private fun translateSetExpressions(subExpression: Expression): Expression {
        return if (subExpression.hasFunctor(EQUAL) && subExpression.hasFunctor("choose")) {
            Or.make(subExpression.get(1).arguments.map { Equality.make(subExpression.get(0), it) })
        }
        else {
            subExpression
        }
    }
}