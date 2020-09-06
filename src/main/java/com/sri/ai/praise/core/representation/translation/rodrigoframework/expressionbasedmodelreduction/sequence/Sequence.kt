package com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.sequence

import com.sri.ai.expresso.api.Expression
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelDownReduction
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelReducer
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelReduction
import com.sri.ai.praise.core.representation.translation.rodrigoframework.expressionbasedmodelreduction.api.ExpressionBasedModelUpReduction
import java.util.*

class SequenceExpressionBasedModelReducer(val reducers: List<out ExpressionBasedModelReducer>)
    : ExpressionBasedModelReducer {

    constructor(vararg reducers: ExpressionBasedModelReducer): this(listOf(*reducers))

    override fun invoke(expressionBasedModel: ExpressionBasedModel): ExpressionBasedModelReduction {
        var currentExpressionBasedModel = expressionBasedModel
        val reductions = reducers.map {
            val currentReduction = it(currentExpressionBasedModel)
            currentExpressionBasedModel = currentReduction.down.translation
            currentReduction
        }
        return SequenceExpressionBasedModelReduction(expressionBasedModel, reductions)
    }

}

class SequenceExpressionBasedModelReduction(
        override val expressionBasedModel: ExpressionBasedModel,
        reductions: List<out ExpressionBasedModelReduction>):
        ExpressionBasedModelReduction {
    override val down =
            SimpleExpressionBasedModelDownReduction(
                    reductions.first().expressionBasedModel,
                    reductions.last().translation)
    override val up = SequenceExpressionBasedModelUpReduction(reductions.map { it.up })
}

class SimpleExpressionBasedModelDownReduction(
        override val expressionBasedModel: ExpressionBasedModel,
        override val translation: ExpressionBasedModel)
    : ExpressionBasedModelDownReduction

class SequenceExpressionBasedModelUpReduction(
        private val upReductions: List<out ExpressionBasedModelUpReduction>)
    : ExpressionBasedModelUpReduction {

    override val upperExpressionBasedModel = upReductions.first().upperExpressionBasedModel
    override val lowerExpressionBasedModel = upReductions.last().lowerExpressionBasedModel

    override fun translateBack(expression: Expression) =
            upReductions.foldRight(expression, ExpressionBasedModelUpReduction::translateBack)

}