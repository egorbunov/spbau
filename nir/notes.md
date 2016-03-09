### Майнинг правил (Rule mining)
#### Постановка задачи и определения
* $I = \lbrace i_1, i_2, i_3, ..., i_n \rbrace$ — множество бинарных аттрибутов (их называют *items*)
* $D = \lbrace t_1, t_2, t_3, ..., t_m \rbrace$ — множество (база данных) транзакций (его называют *database*)
* $X \rightarrow Y$ — правило (*association rule*), где $X$ и $Y$ — подмножества $I$ и $X \cap Y = \emptyset$

Вся задача заключается в поиске таких $X \rightarrow Y$ правил на заданных данных. **Важно:** правила должны быть статистически значимы.

Вводят следующие обозначения и метрики:

* $support(X) = \frac{|\lbrace x_i | x_i \in D \wedge x_i \subset X \rbrace|}{|D|} = P(X)$
* $support(X \rightarrow Y) = support(X \cup Y) = P(X \wedge Y)$
* $confidence(X \rightarrow Y) = \frac{supprot(X \rightarrow Y)}{supprot(X)} = \frac{P(X \wedge Y)}{P(X)} = P(Y|X)$
* $Conviction(X \rightarrow Y) = \frac{1 - support(Y)}{1 - confidence(X \rightarrow Y)} = \frac{P(\neg Y)}{P(\neg Y|X)}$
