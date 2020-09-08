package com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.base

import com.sri.ai.expresso.api.Expression
import com.sri.ai.praise.core.representation.classbased.expressionbased.api.ExpressionBasedModel
import com.sri.ai.praise.core.representation.classbased.expressionbased.core.DefaultExpressionBasedModel
import com.sri.ai.praise.core.representation.translation.expressionbasedmodelreduction.api.ExpressionBasedModelDownReduction

abstract class AbstractExpressionBasedModelDownReduction(override val expressionBasedModel: ExpressionBasedModel)
    : ExpressionBasedModelDownReduction {

    protected abstract fun translateRootOf(subExpression: Expression): Expression

    override val translation: ExpressionBasedModel by lazy {

        val factors = expressionBasedModel.factors.map(::translate)

        val mapFromRandomVariableNameToTypeName =
                processTypeNames(expressionBasedModel.mapFromRandomVariableNameToTypeName)

        val mapFromNonUniquelyNamedConstantNameToTypeName =
                processTypeNames(expressionBasedModel.mapFromNonUniquelyNamedConstantNameToTypeName)

        val mapFromUniquelyNamedConstantNameToTypeName =
                processTypeNames(expressionBasedModel.mapFromUniquelyNamedConstantNameToTypeName)

        val emptyMapFromCategoricalTypeNameToSizeStringSinceAllCategoricalsAreTranslated =
                mapOf<String, String>()

        DefaultExpressionBasedModel(
                factors,
                mapFromRandomVariableNameToTypeName,
                mapFromNonUniquelyNamedConstantNameToTypeName,
                mapFromUniquelyNamedConstantNameToTypeName,
                emptyMapFromCategoricalTypeNameToSizeStringSinceAllCategoricalsAreTranslated,
                expressionBasedModel.additionalTypes,
                expressionBasedModel.isKnownToBeBayesianNetwork,
                expressionBasedModel.theory
        )
    }

    private fun processTypeNames(mapToTypeNames: MutableMap<String, String>) =
            mapToTypeNames.mapValues(::processTypeName)

    private fun processTypeName(symbolAndTypeName: Map.Entry<String, String>): String {
        val typeName = symbolAndTypeName.value
        return processTypeName(typeName)
    }

    protected abstract fun processTypeName(typeName: String): String

    override fun translate(expression: Expression): Expression {
        return expression.replaceAllOccurrences({translateRootOf(it!!)}, expressionBasedModel.context)
    }
}