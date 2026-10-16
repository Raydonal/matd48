---
title: "Planejamento e Análise de Experimentos"
subtitle: "Notas de curso — MATD48"
author: "Raydonal Ospina Martínez"
date: "15 de outubro de 2026"
site: bookdown::bookdown_site
documentclass: book
bibliography: refs.bib
csl: ../Aulas2026/assets/apa.csl
link-citations: true
biblio-style: apalike
description: "Livro-texto de apoio à disciplina MATD48 (Planejamento de Experimentos A), Departamento de Estatística, UFBA."
---



# Prefácio {-}

```{=html}
<div class="caixa-aviso">
<strong>Aviso de direitos autorais — versão preliminar</strong><br>
© 2026 Raydonal Ospina Martínez. Departamento de Estatística, Universidade Federal da Bahia (UFBA).
Todos os direitos reservados.
<ul>
<li><strong>Uso restrito.</strong> Material de apoio de uso exclusivo dos estudantes regularmente
matriculados em MATD48 — Planejamento de Experimentos A, para estudo pessoal. Não é material de
uso livre.</li>
<li><strong>Reprodução proibida.</strong> É vedada a reprodução, distribuição, republicação,
tradução, adaptação ou uso comercial deste material, total ou parcial, por qualquer meio, sem
autorização prévia e por escrito do autor (Lei nº 9.610/1998, Lei de Direitos Autorais).</li>
<li><strong>Plágio.</strong> Usar trechos, figuras, exemplos ou códigos deste texto sem citar a
fonte constitui plágio. Para citar: Ospina Martínez, R. (2026). <em>Planejamento e Análise de
Experimentos: notas de curso — MATD48</em> (versão preliminar). Departamento de Estatística, UFBA.</li>
<li><strong>Em construção.</strong> Este texto está em elaboração, pode conter erros e será
revisado sem aviso prévio. Correções e sugestões são bem-vindas pelos canais da disciplina.</li>
</ul>
</div>
```

Este livro reúne as notas de curso de **MATD48 — Planejamento de Experimentos A**, disciplina do
Departamento de Estatística da Universidade Federal da Bahia (UFBA). Ele acompanha os slides de
aula (`Aulas2026/`) e as listas de exercícios semanais (`Listas2026/`), formando um material único
de apoio, publicado junto à [página do curso](https://raydonal.github.io/matd48/).

## Para quem é este livro {-}

Para estudantes que já cursaram inferência estatística e álgebra linear básica e que agora
precisam projetar, executar e analisar experimentos — não apenas ajustar um modelo a dados que já
existem, mas decidir *como os dados deveriam ser gerados* para responder a uma pergunta causal com
confiança. O planejamento de experimentos é, historicamente, um dos poucos ramos da estatística
nascidos para resolver problemas reais e urgentes — desde parcelas agrícolas em Rothamsted até
testes A/B em produtos digitais — e é tratado aqui como tal.

## Como o material está organizado {-}

Este livro é **mais amplo do que a disciplina**: os slides de aula (`Aulas2026/`) e as listas
semanais (`Listas2026/`) seguem estritamente o cronograma de um semestre, mas o livro não tem essa
restrição — ele cobre, além do programa da disciplina, tópicos modernos de delineamento
experimental (Capítulo 8) que não cabem em 14 semanas, mas que são parte do mesmo arcabouço causal
apresentado desde o Capítulo 1. O livro é deliberadamente **livre de listas de exercícios**: os
exercícios de cada capítulo alinhado ao semestre vivem nas listas semanais correspondentes em
`Listas2026/`, com gabarito à parte, para não duplicar conteúdo entre os dois materiais.

| Capítulo | Tópico | Aulas correspondentes |
|---|---|---|
| 1 | Princípios do delineamento de experimentos | Aula 01 |
| 2 | Introdução aos modelos lineares | Aulas 02–03 |
| 3 | Delineamento completamente aleatorizado (DCA) | Aulas 04–08 |
| 4 | Delineamentos com fontes de bloqueio (blocos, quadrado latino, greco-latino) | Aulas 09–11 |
| 5 | Delineamentos com arranjos fatoriais | Aula 12 |
| 6 | Fatoriais $2^k$/$3^k$, confusão e fracionamento | Aula 13 |
| 7 | Metodologia de superfície de resposta | Aula 14 |
| 8 | Testes A/B e bandits em produtos digitais | *(além do programa do semestre)* |

Um fio condutor atravessa os oito capítulos: os mesmos três domínios de aplicação — **psicologia
experimental**, **agricultura/agroindústria** e **ciência de dados/engenharia** — reaparecem
capítulo após capítulo, para que o leitor veja como a mesma estrutura formal (unidades, fatores,
blocos, interações) se manifesta em contextos muito diferentes; o Capítulo 8 é onde esse fio se
encontra de forma mais direta com a prática atual de experimentação em ciência de dados.

O segundo fio é o **coração do curso**: a diferença entre um **experimento controlado**, em que o
pesquisador decide (por sorteio) qual unidade recebe qual tratamento, e um **estudo não controlado**
(observacional), em que ele apenas registra o que aconteceu. Cada capítulo termina com uma seção
*Controlado × não controlado* que pega o delineamento do capítulo e simula, com a mesma verdade, a
versão observacional da mesma pergunta: o agricultor que escolhe as melhores parcelas (Capítulo 1),
a variável omitida que anda junto com $\mathbf X$ (Capítulo 2), o teste $F$ sem sorteio
(Capítulo 3), o bloco que corrige o que foi medido mas não o resto (Capítulo 4), os fatores que os
operadores sempre movem juntos (Capítulo 5), a confusão acidental de registros históricos
(Capítulo 6), os dados de acaso que apontam a direção errada de subida (Capítulo 7) e a adesão
voluntária contra o teste A/B e o bandit (Capítulo 8). A pergunta é sempre a mesma: *o que este
delineamento controla, e o que daria errado sem ele?*

## Convenções tipográficas {-}

```{=html}
<div class="caixa-aplicacao"><strong>Aplicação</strong> — um exemplo completo, com dados reais ou
simulados e a pergunta de pesquisa que motiva a técnica em questão.</div>
<div class="caixa-discussao"><strong>Para discutir</strong> — perguntas abertas, sem resposta única,
pensadas para debate em sala.</div>
<div class="caixa-r"><strong>Uso do R</strong> — código comentado, sempre reprodutível a partir do
próprio capítulo.</div>
```

Todo o código usa o pacote `tidyverse` para manipulação de dados e `ggplot2` para gráficos, na
mesma convenção já usada nos slides de aula do curso.

## Siglas usadas no livro {-}

Toda sigla é escrita por extenso na primeira vez em que aparece em cada capítulo. Para consulta
rápida:

| Sigla | Significado |
|---|---|
| AAS | Amostragem aleatória simples |
| ANCOVA | Análise de covariância |
| ANOVA | Análise de variância |
| ATE | Efeito médio do tratamento (*average treatment effect*) |
| BIB | Blocos incompletos balanceados (*balanced incomplete block*) |
| BLUE | Melhor estimador linear não viesado (*best linear unbiased estimator*) |
| CCD | Delineamento composto central (*central composite design*) |
| CLD | Agrupamento por letras (*compact letter display*) |
| DBCA | Delineamento em blocos completos aleatorizados |
| DCA | Delineamento completamente aleatorizado |
| DMS | Diferença mínima significativa |
| DP | Desvio-padrão |
| ER | Eficiência relativa |
| FWER | Taxa de erro por família (*family-wise error rate*) |
| FWL | Teorema de Frisch-Waugh-Lovell |
| gl | Graus de liberdade |
| GLS | Mínimos quadrados generalizados (*generalized least squares*) |
| HSD | Diferença honestamente significativa de Tukey (*honestly significant difference*) |
| IC | Intervalo de confiança |
| ICC | Coeficiente de correlação intraclasse |
| iid | Independentes e identicamente distribuídos |
| IPW | Ponderação pelo inverso da probabilidade (*inverse probability weighting*) |
| LSD | Diferença mínima significativa de Fisher (*least significant difference*) |
| ME | Margem de erro (método de Lenth) |
| MOLS | Quadrados latinos mutuamente ortogonais (*mutually orthogonal Latin squares*) |
| MQO | Mínimos quadrados ordinários |
| MSR | Metodologia de superfície de resposta |
| OEC | Critério global de avaliação (*overall evaluation criterion*) |
| OFAT | Um fator de cada vez (*one factor at a time*) |
| PSE | Pseudo erro-padrão (*pseudo standard error*, método de Lenth) |
| QL | Quadrado latino |
| QM, QME | Quadrado médio; quadrado médio do erro |
| REQM | Raiz do erro quadrático médio |
| SNK | Teste de Student-Newman-Keuls |
| SPRT | Teste sequencial da razão de probabilidades (*sequential probability ratio test*) |
| SQ, SQE | Soma de quadrados; soma de quadrados dos erros |
| SUTVA | Suposição de valor estável do tratamento por unidade (*stable unit treatment value assumption*) |
| SVD | Decomposição em valores singulares (*singular value decomposition*) |
| UCB | Limite superior de confiança (*upper confidence bound*) |
| UE | Unidade experimental |
| VIF | Fator de inflação da variância (*variance inflation factor*) |
