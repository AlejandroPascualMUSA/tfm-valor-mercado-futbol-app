# Interpretacion SHAP del modelo
section: shap

SHAP permite explicar modelos complejos asignando a cada variable una contribucion a la prediccion. La importancia global muestra que variables como minutos 2023/24, edad, contexto del club, contrato y rendimiento deportivo tienen gran peso en el valor estimado. Los riesgos de lesion aportan informacion adicional, pero su importancia global es secundaria frente a factores deportivos y contextuales.

Los SHAP dependence plots sirven para estudiar relaciones no lineales entre una variable y su contribucion al modelo. El eje vertical indica si la variable empuja la prediccion hacia arriba o hacia abajo. Una contribucion positiva no significa causalidad, sino asociacion interna aprendida por el modelo.

En el caso de las variables de lesion, una relacion positiva puede aparecer porque jugadores valiosos juegan mas, acumulan mas exposicion y por tanto tienen mayor riesgo estimado. Por eso conviene distinguir entre probabilidad absoluta y exceso de riesgo ajustado por exposicion.
